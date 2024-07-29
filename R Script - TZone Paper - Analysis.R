########################################################################
#################### T - Zone Project Analysis #########################
########################################################################

# Load Libraries ####
library(haven) # read spss files
library(tidyverse) # data cleaning
library(psych) # cronbach's alphas
library(Hmisc) # correlations with p-values
library(qwraps2) #descriptive summary tables
library(foreign) # read spss data, compatable with process package
library(gtsummary) #make regression summary tables
library(broom) #summary stats for regressions
library(gt) # make tables
#note: download and run process script (https://www.processmacro.org/download.html) 


# Load Data ####
# Initial survey
spss.file<-read_sav("LW30821_001A_CLIENT.sav") # 656 x 163
# Follow-up survey
spss.file.2<-read_sav("LW30821_001B_CLIENT.sav") # 656 x 20
# Combined surveys (use for analysis)
spss.file.3<-read_sav("LW30821_001AB_CLIENT.sav") # 656 x 183


# Data Cleaning####
# Convert variables to numeric and recode
data <- spss.file.3 %>%  # 569 x 198
  mutate(conditionsmarker_mouth = as.numeric(conditionsmarker_mouth),
         conditionsmarker_eyes = as.numeric(conditionsmarker_eyes),
         conditionsmarker_nose = as.numeric(conditionsmarker_nose),
         conditionsmarker_mouth = recode(conditionsmarker_mouth, "0" = "0", "1"="1"),
         conditionsmarker_mouth = replace(conditionsmarker_mouth, conditionsmarker_mouth<1, NA),
         conditionsmarker_nose = recode(conditionsmarker_nose, "0" = "0", "1"="2"),
         conditionsmarker_nose = replace(conditionsmarker_nose, conditionsmarker_nose<1, NA),
         conditionsmarker_eyes = recode(conditionsmarker_eyes, "0" = "0", "1"="3"),
         conditionsmarker_eyes = replace(conditionsmarker_eyes, conditionsmarker_eyes<1, NA),
         Condition = coalesce(conditionsmarker_mouth,conditionsmarker_nose,conditionsmarker_eyes),
         Condition = as.factor(Condition),
         Condition = recode(Condition, '1'="Mouth", '2'="Nose", '3'="Eyes")) %>%
  as_tibble() %>%
  mutate(across(c(Q10r1:Q10r2,Q11:Q12,Q22r1:Q22r3), as.numeric),
         across(c(Q10r1:Q10r2,Q11:Q12,Q22r1:Q22r3), ~recode(., '1'=5, '2'=4, '3'=3, '4'=2, '5'=1))) %>%
  drop_na(date_wave2) %>%
  rowwise() %>%
  mutate(intention = mean(Q8r1, Q8r2, Q8r3),
         outcome.exp = mean(Q9r1,Q9r2,Q9r3,Q9r4,Q9r5,Q9r6,Q9r7,Q9r8,Q9r9,Q9r10),
         risk.perception = mean(Q10r1, Q10r2),
         individual.sev = mean(Q11, Q12),
         task.self.efficacy = mean(Q13r1,Q13r2,Q13r3,Q13r4,Q13r5,Q13r6,Q13r7,Q13r8,
                                   Q13r9,Q13r10,Q13r11,Q13r12,Q13r13),
         action.planning = mean(Q15r1,Q15r2,Q15r3,Q15r4),
         coping.planning = mean(Q16r1,Q16r2,Q16r3,Q1r4),
         social.support = mean(Q17r1,Q17r2,Q17r3),
         automaticity = mean(Q19r1,Q19r2,Q19r3,Q19r4),
         goal.conflict = mean(Q23r1,Q23r2,Q23r3,Q23r4,Q23r5,Q23r6,Q23r7),
         stable.context = mean(Q22r1,Q22r2,Q22r3),
         behaviour = mean(Q18r1,Q18r2,Q18r3),
         behaviour2 = mean(Q2r1,Q2r2,Q2r3),
         action.control.t2 = mean(Q1r1:Q1r7)) %>%
  as_tibble()

# Cronbach's alphas for multi-item questions ####

# Intention: alpha = .92 (all items kept)
data %>%
  select(Q8r1, Q8r2, Q8r3) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Intention")

# Outcome expectancies: alpha =.95 (items 11-15 dropped)
data %>% # alpha = .86
  select(Q9r1:Q9r15) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Outcome expectation")

data %>% # alpha = .94
  select(Q9r1:Q9r10) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Outcome expectation")

# Risk perception: alpha = .92 (all items kept)
data %>% 
  select(Q10r1:Q10r2) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Risk perception")

# Individual severity: alpha = .70 (all items kept)
data %>% 
  select(Q11:Q12) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Individual severity")

# Task self-efficacy: alpha = .87 (Q14 removed)
data %>% # alpha = .85
  select(Q13r1:Q13r13, Q14) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Task self-efficacy")

data %>% # alpha = .87
  select(Q13r1:Q13r13) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Task self-efficacy")

# Action planning: alpha = .96 (all items kept)
data %>%
  select(Q15r1:Q15r4) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Action planning")

# Coping planning: alpha = .96 (all items kept)
data %>% 
  select(Q16r1:Q16r4) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Coping planning")

# Social support: alpha = .91 (all items kept)
data %>%  
  select(Q17r1:Q17r3) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Social support")

# Automaticity: alpha = .94 (all items kept)
data %>% 
  select(Q19r1:Q19r4) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Automaticity")

# Goal conflict: alpha = .89 (all items kept)
data %>% 
  select(Q23r1:Q23r7) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Goal conflict")

# Stability of context: alpha = .91 (all items kept)
data %>% 
  select(Q22r1:Q22r3) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Stability of context")

# Face touching behaviour Time 1: alpha = .84 (all items kept)
data %>% 
  select(Q18r1:Q18r3) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Face touching behaviour")

# Face touching behaviour Time 2: alpha = .87 (all items kept)
data %>% 
  select(Q2r1:Q2r3) %>%
  mutate_all(type.convert)%>%
  psych::alpha(title = "Face touching behaviour Time 2")

# Table 1: Demographic information ####

data %>%
  group_by() %>%
  summarise(count = n(),
            age.m = mean(AGENUM),
            age.sd = sd(AGENUM))
data %>%
  group_by(Q2) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

data %>% # note using PROV instead of Q4
  group_by() %>%
  count(PROV) %>%
  mutate(percent = n / sum(n) * 100)

data %>%
  group_by() %>%
  count(Q5) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q6) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q31) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q32) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q33) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q34) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q35) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q36) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q39) %>%
  mutate(percent = n / sum(n) * 100) 

data %>%
  group_by() %>%
  count(Q38) %>%
  mutate(percent = n / sum(n) * 100) 

# Table 2: Mean construct scores ####
data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(intention, na.rm = T),
            SD = sd(intention, na.rm = T))

data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(intention, na.rm = T),
            SD = sd(intention, na.rm = T))

owa <- aov(intention ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(outcome.exp, na.rm = T),
            SD = sd(outcome.exp, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(outcome.exp, na.rm = T),
            SD = sd(outcome.exp, na.rm = T))

owa <- aov(outcome.exp ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(risk.perception, na.rm = T),
            SD = sd(risk.perception, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(risk.perception, na.rm = T),
            SD = sd(risk.perception, na.rm = T))

owa <- aov(risk.perception ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q11, na.rm = T), #Q11 = severity
            SD = sd(Q11, na.rm = T))

data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q11, na.rm = T), #Q11 = severity
            SD = sd(Q11, na.rm = T))

owa <- aov(Q11 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(task.self.efficacy, na.rm = T),
            SD = sd(task.self.efficacy, na.rm = T))

data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(task.self.efficacy, na.rm = T),
            SD = sd(task.self.efficacy, na.rm = T))

owa <- aov(task.self.efficacy ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(action.planning, na.rm = T),
            SD = sd(action.planning, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(action.planning, na.rm = T),
            SD = sd(action.planning, na.rm = T))

owa <- aov(action.planning ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(coping.planning, na.rm = T),
            SD = sd(coping.planning, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(coping.planning, na.rm = T),
            SD = sd(coping.planning, na.rm = T))

owa <- aov(coping.planning ~ Condition, data = data)
summary(owa)


data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(social.support, na.rm = T),
            SD = sd(social.support, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(social.support, na.rm = T),
            SD = sd(social.support, na.rm = T))

owa <- aov(social.support ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(automaticity, na.rm = T),
            SD = sd(automaticity, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(automaticity, na.rm = T),
            SD = sd(automaticity, na.rm = T))

owa <- aov(automaticity ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(goal.conflict, na.rm = T),
            SD = sd(goal.conflict, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(goal.conflict, na.rm = T),
            SD = sd(goal.conflict, na.rm = T))

owa <- aov(goal.conflict ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(stable.context, na.rm = T),
            SD = sd(stable.context, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(stable.context, na.rm = T),
            SD = sd(stable.context, na.rm = T))

owa <- aov(stable.context ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(behaviour, na.rm = T),
            SD = sd(behaviour, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(behaviour, na.rm = T),
            SD = sd(behaviour, na.rm = T))

owa <- aov(behaviour ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q24, na.rm = T),
            SD = sd(Q24, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q24, na.rm = T),
            SD = sd(Q24, na.rm = T))

owa <- aov(Q24 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q25, na.rm = T),
            SD = sd(Q25, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q25, na.rm = T),
            SD = sd(Q25, na.rm = T))

owa <- aov(Q25 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q26, na.rm = T),
            SD = sd(Q26, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q26, na.rm = T),
            SD = sd(Q26, na.rm = T))

owa <- aov(Q26 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q27, na.rm = T),
            SD = sd(Q27, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q27, na.rm = T),
            SD = sd(Q27, na.rm = T))

owa <- aov(Q27 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q28, na.rm = T),
            SD = sd(Q28, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q28, na.rm = T),
            SD = sd(Q28, na.rm = T))

owa <- aov(Q28 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q29, na.rm = T),
            SD = sd(Q29, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q29, na.rm = T),
            SD = sd(Q29, na.rm = T))

owa <- aov(Q29 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q29b, na.rm = T),
            SD = sd(Q29b, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q29b, na.rm = T),
            SD = sd(Q29b, na.rm = T))

owa <- aov(Q29b ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q30, na.rm = T),
            SD = sd(Q30, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q30, na.rm = T),
            SD = sd(Q30, na.rm = T))

owa <- aov(Q30 ~ Condition, data = data)
summary(owa)


data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(action.control.t2, na.rm = T),
            SD = sd(action.control.t2, na.rm = T))
data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(action.control.t2, na.rm = T),
            SD = sd(action.control.t2, na.rm = T))

owa <- aov(action.control.t2 ~ Condition, data = data)
summary(owa)

data%>%
  group_by()%>%
  summarise(count = n(),
            Mean = mean(Q4_wave2, na.rm = T),
            SD = sd(Q4_wave2, na.rm = T))

data%>%
  group_by(Condition)%>%
  summarise(count = n(),
            Mean = mean(Q4_wave2, na.rm = T),
            SD = sd(Q4_wave2, na.rm = T))

owa <- aov(Q4_wave2 ~ Condition, data = data)
summary(owa)


# Table 3: Bivariate correlations ####

# correlations: mouth
cor.mouth <- data %>%
  filter(Condition=="Mouth")

vars <- cor.mouth %>% 
  select(Q4_wave2, intention, task.self.efficacy, outcome.exp, risk.perception, social.support,
         action.planning, coping.planning, automaticity, goal.conflict, stable.context,action.control.t2) 

cor1<-rcorr(as.matrix(vars))

cor1.p<-cor1$P
cor1.cor<-cor1$r

# correlations: eyes
cor.eyes <- data %>%
  filter(Condition=="Eyes")

vars <- cor.eyes %>% 
  select(Q4_wave2, intention, task.self.efficacy, outcome.exp, risk.perception, social.support,
         action.planning, coping.planning, automaticity, goal.conflict, stable.context,action.control.t2) 

cor1<-rcorr(as.matrix(vars))

cor1.p<-cor1$P
cor1.cor<-cor1$r

# correlations: nose
cor.nose <- data %>%
  filter(Condition=="Nose")

vars <- cor.nose %>% 
  select(Q4_wave2, intention, task.self.efficacy, outcome.exp, risk.perception, social.support,
         action.planning, coping.planning, automaticity, goal.conflict, stable.context,action.control.t2) 

cor1<-rcorr(as.matrix(vars))

cor1.p<-cor1$P
cor1.cor<-cor1$r


# Table 4: Regressions - Intention ####

data <- data %>%
  mutate(Q2 = as.factor(Q2),
         Q2 = recode(Q2, "1" = "Male", "2" = "Female", "96" = "Something Else"))

# split data by 3 conditions
mouth.data <- data %>%  
  filter(Condition == "Mouth")

nose.data <- data %>%  
  filter(Condition == "Nose")

eyes.data <- data %>%  
  filter(Condition == "Eyes")

# Mouth
mouth.model1.s1<-lm(intention ~ AGENUM + Q2, data=mouth.data)
summary(mouth.model1.s1) 

mouth.model1.s2<-lm(intention ~ AGENUM + Q2 + task.self.efficacy +risk.perception +
                      social.support + outcome.exp, data=mouth.data)
summary(mouth.model1.s2) 

mouth.model1.s3<-lm(intention ~ AGENUM + Q2 + task.self.efficacy + risk.perception +
                      social.support + outcome.exp+ goal.conflict, data=mouth.data)
summary(mouth.model1.s3) 

## summary table: Mouth
tbl_1 <- mouth.model1.s1 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age")) %>%
  add_glance_source_note(include = r.squared)

tbl_2 <- mouth.model1.s2 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                      task.self.efficacy ~ "Task Self Efficacy",
                                                      risk.perception ~ "Risk Perception",
                                                      social.support ~ "Social Support",
                                                      outcome.exp ~ "Outcome Expectancy"))%>%
  add_glance_source_note(include = r.squared)

tbl_3 <- mouth.model1.s3 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                      task.self.efficacy ~ "Task Self Efficacy",
                                                      risk.perception ~ "Risk Perception",
                                                      social.support ~ "Social Support",
                                                      outcome.exp ~ "Outcome Expectancy",
                                                      goal.conflict ~ "Goal Conflict")) %>%
  add_glance_source_note(include = r.squared)


# Eyes
eyes.model1.s1<-lm(intention ~ AGENUM + Q2, data=eyes.data)
summary(eyes.model1.s1)

eyes.model1.s2<-lm(intention ~ AGENUM + Q2 + task.self.efficacy + risk.perception +
                     social.support + outcome.exp, data=eyes.data)
summary(eyes.model1.s2) 

eyes.model1.s3<-lm(intention ~ AGENUM + Q2 + task.self.efficacy + risk.perception +
                     social.support + outcome.exp + goal.conflict, data=eyes.data)
summary(eyes.model1.s3) 

## summary table: eyes

tbl_1 <- eyes.model1.s1 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age")) %>%
  add_glance_source_note(include = r.squared)

tbl_2 <- eyes.model1.s2 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                     task.self.efficacy ~ "Task Self Efficacy",
                                                     risk.perception ~ "Risk Perception",
                                                     social.support ~ "Social Support",
                                                     outcome.exp ~ "Outcome Expectancy"))%>%
  add_glance_source_note(include = r.squared)

tbl_3 <- eyes.model1.s3 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                     task.self.efficacy ~ "Task Self Efficacy",
                                                     risk.perception ~ "Risk Perception",
                                                     social.support ~ "Social Support",
                                                     outcome.exp ~ "Outcome Expectancy",
                                                     goal.conflict ~ "Goal Conflict")) %>%
  add_glance_source_note(include = r.squared)

  

# Nose
nose.model1.s1<-lm(intention ~ AGENUM + Q2, data=nose.data)
summary(nose.model1.s1) 

nose.model1.s2<-lm(intention ~ AGENUM + Q2 + task.self.efficacy + risk.perception +
                     social.support + outcome.exp, data=nose.data)
summary(nose.model1.s2)

nose.model1.s3<-lm(intention ~ AGENUM + Q2 + task.self.efficacy + risk.perception +
                     social.support + outcome.exp + goal.conflict, data=nose.data)
summary(nose.model1.s3) 

## summary table: nose

tbl_1 <- nose.model1.s1 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age")) %>%
  add_glance_source_note(include = r.squared)

tbl_2 <- nose.model1.s2 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                     task.self.efficacy ~ "Task Self Efficacy",
                                                     risk.perception ~ "Risk Perception",
                                                     social.support ~ "Social Support",
                                                     outcome.exp ~ "Outcome Expectancy"))%>%
  add_glance_source_note(include = r.squared)

tbl_3 <- nose.model1.s3 %>% tbl_regression(label = c(Q2 ~ "Gender", AGENUM ~ "Age",
                                                     task.self.efficacy ~ "Task Self Efficacy",
                                                     risk.perception ~ "Risk Perception",
                                                     social.support ~ "Social Support",
                                                     outcome.exp ~ "Outcome Expectancy",
                                                     goal.conflict ~ "Goal Conflict")) %>%
  add_glance_source_note(include = r.squared)

# Table 5: Regressions - Touching Behaviour ####

# split data by 3 conditions
mouth.data <- data %>%  
  filter(Condition == "Mouth") %>%
  mutate(Age = AGENUM,
         Gender = as.factor(Q2),
         Gender = recode(Gender, '1'="Male", '2'="female", '3'="Something Else"))

nose.data <- data %>%  
  filter(Condition == "Nose")%>%
  mutate(Age = AGENUM,
         Gender = as.factor(Q2),
         Gender = recode(Gender, '1'="Male", '2'="female", '3'="Something Else"))

eyes.data <- data %>%  
  filter(Condition == "Eyes")%>%
  mutate(Age = AGENUM,
         Gender = as.factor(Q2),
         Gender = recode(Gender, '1'="Male", '2'="female", '3'="Something Else"))

# Mouth
mouth.model2.s1<-lm(Q4_wave2 ~ Age + Gender, data=mouth.data)
summary(mouth.model2.s1) 

mouth.model2.s2<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                      coping.planning + action.control.t2, data=mouth.data)
summary(mouth.model2.s2) 

mouth.model2.s3<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                      coping.planning+ action.control.t2 + goal.conflict + stable.context + automaticity, data=mouth.data)
summary(mouth.model2.s3) 


# eyes
eyes.model2.s1<-lm(Q4_wave2 ~ Age + Gender, data=eyes.data)
summary(eyes.model2.s1) 

eyes.model2.s2<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                     coping.planning + action.control.t2, data=eyes.data)
summary(eyes.model2.s2) 

eyes.model2.s3<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                     coping.planning + action.control.t2 + goal.conflict + stable.context + automaticity, data=eyes.data)
summary(eyes.model2.s3) 

# nose
nose.model2.s1<-lm(Q4_wave2 ~ Age + Gender, data=nose.data)
summary(lm.beta(nose.model2.s1)) 

nose.model2.s2<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                     coping.planning + action.control.t2 , data=nose.data)
summary(nose.model2.s2) 

nose.model2.s3<-lm(Q4_wave2 ~ Age + Gender + intention + task.self.efficacy + action.planning +
                     coping.planning + action.control.t2 + goal.conflict + stable.context + automaticity, data=nose.data)
summary(nose.model2.s3) 

# Table 6: Mediation ####

# read with foreign (c.f., haven) so data works with process package

spss.file.100<-foreign::read.spss("LW30821_001AB_CLIENT.sav", use.value.labels = F,
                                  to.data.frame = T) # 569 x 183

data <- spss.file.100 %>%
  mutate(conditionsmarker_mouth = as.numeric(conditionsmarker_mouth),
         conditionsmarker_eyes = as.numeric(conditionsmarker_eyes),
         conditionsmarker_nose = as.numeric(conditionsmarker_nose),
         conditionsmarker_mouth = recode(conditionsmarker_mouth, "0" = "0", "1"="1"),
         conditionsmarker_mouth = replace(conditionsmarker_mouth, conditionsmarker_mouth<1, NA),
         conditionsmarker_nose = recode(conditionsmarker_nose, "0" = "0", "1"="2"),
         conditionsmarker_nose = replace(conditionsmarker_nose, conditionsmarker_nose<1, NA),
         conditionsmarker_eyes = recode(conditionsmarker_eyes, "0" = "0", "1"="3"),
         conditionsmarker_eyes = replace(conditionsmarker_eyes, conditionsmarker_eyes<1, NA),
         Condition = coalesce(conditionsmarker_mouth,conditionsmarker_nose,conditionsmarker_eyes),
         Condition = as.factor(Condition),
         Condition = recode(Condition, '1'="Mouth", '2'="Nose", '3'="Eyes")) %>%
  as_tibble() %>%
  mutate(across(c(Q10r1:Q10r2,Q11:Q12,Q22r1:Q22r3), as.numeric),
         across(c(Q10r1:Q10r2,Q11:Q12,Q22r1:Q22r3), ~recode(., '1'=5, '2'=4, '3'=3, '4'=2, '5'=1))) %>%
  drop_na(date_wave2) %>%
  rowwise() %>%
  mutate(intention = mean(Q8r1, Q8r2, Q8r3),
         outcome.exp = mean(Q9r1,Q9r2,Q9r3,Q9r4,Q9r5,Q9r6,Q9r7,Q9r8,Q9r9,Q9r10),
         risk.perception = mean(Q10r1, Q10r2),
         individual.sev = mean(Q11, Q12),
         task.self.efficacy = mean(Q13r1,Q13r2,Q13r3,Q13r4,Q13r5,Q13r6,Q13r7,Q13r8,
                                   Q13r9,Q13r10,Q13r11,Q13r12,Q13r13),
         action.planning = mean(Q15r1,Q15r2,Q15r3,Q15r4),
         coping.planning = mean(Q16r1,Q16r2,Q16r3,Q1r4),
         social.support = mean(Q17r1,Q17r2,Q17r3),
         automaticity = mean(Q19r1,Q19r2,Q19r3,Q19r4),
         goal.conflict = mean(Q23r1,Q23r2,Q23r3,Q23r4,Q23r5,Q23r6,Q23r7),
         stable.context = mean(Q22r1,Q22r2,Q22r3),
         behaviour = mean(Q18r1,Q18r2,Q18r3),
         behaviour2 = mean(Q2r1,Q2r2,Q2r3),
         action.control.t2 = mean(Q1r1:Q1r7))

process(data =data,
        y = "Q4_wave2",
        x = "intention",
        m =c("action.planning", "coping.planning", "action.control.t2"),
        cov = "automaticity",
        model = 6)





