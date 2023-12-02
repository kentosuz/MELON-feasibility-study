#downloading the data
NICU_pre <- read.csv("MELON_pre_survey.csv")
NICU_post <- read.csv("MELON_post_survey.csv")

library(dplyr)


# Demographics (n=20)
#age 
mean(NICU_pre$Q2) # average 39.4 yrs 
sd(NICU_pre$Q2) # sd = 4.235191


# educational background
NICU_pre$edu <- 
  case_when(NICU_pre$Q4=="中学校" ~ 0,
            NICU_pre$Q4=="高等学校"~ 1,
            NICU_pre$Q4=="専門学校" ~ 2,
            NICU_pre$Q4=="短期大学" ~ 3,
            NICU_pre$Q4=="大学" ~ 4,
            NICU_pre$Q4=="大学院" ~ 5,
  )
table(NICU_pre$edu)

# employment status
NICU_pre$job <- 
  case_when(NICU_pre$Q6=="学生" ~ 0,
            NICU_pre$Q6=="失業中（求職中）"~ 1,
            NICU_pre$Q6=="無職（専業主婦を含む）" | NICU_pre$Q7=="Nothing" ~ 2,
            NICU_pre$Q6=="パートタイマー・派遣等の非正規雇用者" ~ 3,
            NICU_pre$Q6=="フルタイム勤務（正規雇用者）" ~ 4,
            NICU_pre$Q7=="個人事業主" ~ 5,
  )
table(NICU_pre$job)

# number of children
mean(NICU_pre$child_num) # average 2.1 children
sd(NICU_pre$child_num) # sd = 0.8522416

# gestational age at birth
mean(NICU_pre$Q9) # average 28.15 wks
sd(NICU_pre$Q9) # sd = 2.739093

# age of the kid (n=18)
mean(NICU_pre$NICU_age, na.rm=T)# 5.444444 yrs old
sd(NICU_pre$NICU_age, na.rm=T)# 2.914915 yrs old

#Past experiences with meditation
NICU_pre$experience <-
  case_when(NICU_pre$Q10=="いいえ" ~ 0,
            NICU_pre$Q10=="はい" & NICU_pre$Q13=="週に１回未満" ~ 1,
            NICU_pre$Q10=="はい" & NICU_pre$Q13=="週に２回から３回" ~ 2,
            NICU_pre$Q10=="はい" & NICU_pre$Q13=="週に４回から５回" ~ 3,
            NICU_pre$Q10=="はい" & NICU_pre$Q13=="(ほぼ)毎日" ~ 4,
  )
table(NICU_pre$experience)

# length of their children’s NICU hospitalization
mean(NICU_post$Q10, na.rm=TRUE) # average 3.16875 months
sd(NICU_post$Q10, na.rm=TRUE) # sd = 1.073759


#Narrowing it down to those that completed both pre- and post-questionnaire
NICU_overlap <- intersect(NICU_pre$NICU_ID, NICU_post$NICU_ID) 
NICU_pre_overlap <- filter(NICU_pre, NICU_ID %in% NICU_overlap)

# Pre-intervention scores
# STAI (trait anxiety)
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q16", "Q17", "Q18", "Q19", "Q22", "Q23", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32", "Q34"), 
            list(~as.numeric(case_when(
              . == "決してそうでない" ~ 1,
              . == "たまにそうである" ~ 2,
              . == "しばしばそうである" ~ 3,
              . == "いつもそうである" ~ 4,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q15", "Q20", "Q21", "Q24", "Q27", "Q30", "Q33"), 
            list(~as.numeric(case_when(
              . == "決してそうでない" ~ 4,
              . == "たまにそうである" ~ 3,
              . == "しばしばそうである" ~ 2,
              . == "いつもそうである" ~ 1,
              TRUE ~ NA_real_
            ))))
# average STAI scores
NICU_pre_overlap_STAI <- NICU_pre_overlap %>%
  select(Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34)
NICU_pre_overlap$STAI <- rowSums(NICU_pre_overlap_STAI)/ncol(NICU_pre_overlap_STAI)


# PSI-SF
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q35","Q36","Q37","Q38","Q39","Q40","Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q53"), 
            list(~as.numeric(case_when(
              . == "まったく違う" ~ 1,
              . == "違う" ~ 2,
              . == "どちらとも言えない" ~ 3,
              . == "そのとおり" ~ 4,
              . == "まったくそのとおり" ~ 5,
              TRUE ~ NA_real_
            ))))
# average PSI-SF scores
NICU_pre_overlap_PSI <- NICU_pre_overlap %>%
  select(Q35, Q36, Q37, Q38, Q39, Q40, Q41, Q42, Q43, Q44, Q45, Q46, Q47, Q48, Q49, Q50, Q51, Q52, Q53)
NICU_pre_overlap$PSI <- rowSums(NICU_pre_overlap_PSI)/ncol(NICU_pre_overlap_PSI)


# FFMQ-C-SF
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q54", "Q56", "Q60", "Q62", "Q63", "Q64", "Q65", "Q67", "Q68", "Q70", "Q71", "Q72"), 
            list(~as.numeric(case_when(
              . == "まったくあてはまらない" ~ 1,
              . == "めったにあてはまらない" ~ 2,
              . == "たまにあてはまる" ~ 3,
              . == "しばしばあてはまる" ~ 4,
              . == "いつもあてはまる" ~ 5,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q55","Q57","Q58","Q59","Q61","Q66","Q69", "Q73"), 
            list(~as.numeric(case_when(
              . == "まったくあてはまらない" ~ 5,
              . == "めったにあてはまらない" ~ 4,
              . == "たまにあてはまる" ~ 3,
              . == "しばしばあてはまる" ~ 2,
              . == "いつもあてはまる" ~ 1,
              TRUE ~ NA_real_
            ))))
# average FFMQ-C-SF total score omitting the participant without responses on FFMQ questions at post-intervention (n=15)
NICU_pre_overlap_FFMQ <- NICU_pre_overlap %>%
  filter(NICU_ID != "A014")
NICU_pre_overlap_FFMQ<- NICU_pre_overlap_FFMQ %>% 
  select(Q54, Q55, Q56, Q57, Q58, Q59, Q60, Q61, Q62, Q63, Q64, Q65, Q66, Q67, Q68, Q69, Q70, Q71, Q72, Q73)
NICU_pre_overlap_FFMQ_avg <- rowSums(NICU_pre_overlap_FFMQ)/ncol(NICU_pre_overlap_FFMQ)


# SCS-SF
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q77","Q78","Q80","Q82", "Q83", "Q85"), 
            list(~as.numeric(case_when(
              . == "1: ほとんどない" ~ 1,
              . == "2" ~ 2,
              . == "3" ~ 3,
              . == "4" ~ 4,
              . == "5: ほとんどいつも" ~ 5,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_pre_overlap <- NICU_pre_overlap %>% 
  mutate_at(c("Q74","Q75","Q76","Q79","Q81","Q84"), 
            list(~as.numeric(case_when(
              . == "1: ほとんどない" ~ 5,
              . == "2" ~ 4,
              . == "3" ~ 3,
              . == "4" ~ 2,
              . == "5: ほとんどいつも" ~ 1,
              TRUE ~ NA_real_
            ))))
# average of SCS-SF total scores
NICU_pre_overlap_SCS <- NICU_pre_overlap %>%
  select(Q74, Q75, Q76, Q77, Q78, Q79, Q80, Q81, Q82, Q83, Q84, Q85)
NICU_pre_overlap$SCS <- rowSums(NICU_pre_overlap_SCS)/ncol(NICU_pre_overlap_SCS)


# NICU study post-survey
# STAI (trait anxiety)
NICU_post <- NICU_post %>% 
  mutate_at(c("Q16", "Q17", "Q18", "Q19", "Q22", "Q23", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32", "Q34"), 
            list(~as.numeric(case_when(
              . == "決してそうでない" ~ 1,
              . == "たまにそうである" ~ 2,
              . == "しばしばそうである" ~ 3,
              . == "いつもそうである" ~ 4,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_post <- NICU_post %>% 
  mutate_at(c("Q15", "Q20", "Q21", "Q24", "Q27", "Q30", "Q33"), 
            list(~as.numeric(case_when(
              . == "決してそうでない" ~ 4,
              . == "たまにそうである" ~ 3,
              . == "しばしばそうである" ~ 2,
              . == "いつもそうである" ~ 1,
              TRUE ~ NA_real_
            ))))
# average of STAI total scores
NICU_post_STAI <- NICU_post %>%
  select(Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34)
NICU_post$STAI <- rowSums(NICU_post_STAI)/ncol(NICU_post_STAI)
# for A015, out of 19 questions
NICU_post$STAI[11] <- sum(NICU_post_STAI[11,], na.rm = TRUE)/19


# PSI-SF
NICU_post <- NICU_post %>% 
  mutate_at(c("Q35","Q36","Q37","Q38","Q39","Q40","Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q53"), 
            list(~as.numeric(case_when(
              . == "まったく違う" ~ 1,
              . == "違う" ~ 2,
              . == "どちらとも言えない" ~ 3,
              . == "そのとおり" ~ 4,
              . == "まったくそのとおり" ~ 5,
              TRUE ~ NA_real_
            ))))
# average PSI-SF total scores
NICU_post_PSI <- NICU_post %>%
  select(Q35, Q36, Q37, Q38, Q39, Q40, Q41, Q42, Q43, Q44, Q45, Q46, Q47, Q48, Q49, Q50, Q51, Q52, Q53)
NICU_post$PSI <- rowSums(NICU_post_PSI)/ncol(NICU_post_PSI)


# FFMQ-C-SF
NICU_post <- NICU_post %>% 
  mutate_at(c("Q54", "Q56", "Q60", "Q62", "Q63", "Q64", "Q65", "Q67", "Q68", "Q70", "Q71", "Q72"), 
            list(~as.numeric(case_when(
              . == "まったくあてはまらない" ~ 1,
              . == "めったにあてはまらない" ~ 2,
              . == "たまにあてはまる" ~ 3,
              . == "しばしばあてはまる" ~ 4,
              . == "いつもあてはまる" ~ 5,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_post <- NICU_post %>% 
  mutate_at(c("Q55","Q57","Q58","Q59","Q61","Q66","Q69", "Q73"), 
            list(~as.numeric(case_when(
              . == "まったくあてはまらない" ~ 5,
              . == "めったにあてはまらない" ~ 4,
              . == "たまにあてはまる" ~ 3,
              . == "しばしばあてはまる" ~ 2,
              . == "いつもあてはまる" ~ 1,
              TRUE ~ NA_real_
            ))))
# average FFMQ-C-SF total scores, omitting A014 (n=15)
NICU_post_FFMQ <- NICU_post %>%
  filter(NICU_ID != "A014")
NICU_post_FFMQ <- NICU_post_FFMQ %>% 
  select(Q54, Q55, Q56, Q57, Q58, Q59, Q60, Q61, Q62, Q63, Q64, Q65, Q66, Q67, Q68, Q69, Q70, Q71, Q72, Q73)
NICU_post_FFMQ_avg <- rowSums(NICU_post_FFMQ)/ncol(NICU_post_FFMQ)
# for A010, out of 19 questions (Q63 missing)
NICU_post_FFMQ_avg[8] <- sum(NICU_post_FFMQ[8,], na.rm = TRUE)/19


# SCS-SF
NICU_post <- NICU_post %>% 
  mutate_at(c("Q77","Q78","Q80","Q82", "Q83", "Q85"), 
            list(~as.numeric(case_when(
              . == "1: ほとんどない" ~ 1,
              . == "2" ~ 2,
              . == "3" ~ 3,
              . == "4" ~ 4,
              . == "5: ほとんどいつも" ~ 5,
              TRUE ~ NA_real_
            ))))
# reverse coded items
NICU_post <- NICU_post %>% 
  mutate_at(c("Q74","Q75","Q76","Q79","Q81","Q84"), 
            list(~as.numeric(case_when(
              . == "1: ほとんどない" ~ 5,
              . == "2" ~ 4,
              . == "3" ~ 3,
              . == "4" ~ 2,
              . == "5: ほとんどいつも" ~ 1,
              TRUE ~ NA_real_
            ))))
# average of SCS-SF total scores
NICU_post_SCS <- NICU_post %>%
  select(Q74, Q75, Q76, Q77, Q78, Q79, Q80, Q81, Q82, Q83, Q84, Q85)
NICU_post$SCS <- rowSums(NICU_post_SCS)/ncol(NICU_post_SCS)


# Trait Anxiety (STAI) pre-post changes
t_STAI_avg <- t.test(NICU_post$STAI, NICU_pre_overlap$STAI, paired = TRUE)
# t = -2.7944, df = 15, p-value = 0.01361
library(effsize)
cohen.d(NICU_post$STAI, NICU_pre_overlap$STAI, paired = TRUE, hedges.correction=TRUE)
# Hedge's g: -0.3799554 (-0.66749230 to -0.09241858)
#STAI total score pre, post 
sum(NICU_pre_overlap$STAI)/nrow(NICU_pre_overlap) # 2.25
sd(NICU_pre_overlap$STAI)# sd = 0.4912569
sum(NICU_post$STAI)/nrow(NICU_post) # 2.060033　
sd(NICU_post$STAI)# sd = 0.3758686


# Parental Stress (PSI) pre-post changes
t_PSI_avg <- t.test(NICU_post$PSI, NICU_pre_overlap$PSI, paired = TRUE)
# t = -0.80789, df = 15, p-value = 0.4318
cohen.d(NICU_post$PSI, NICU_pre_overlap$PSI, paired = TRUE, hedges.correction=TRUE)
# Hedge's g: -0.1326629 (-0.4694942 to 0.2041685)
#PSI total score pre, post 
sum(NICU_pre_overlap$PSI)/nrow(NICU_pre_overlap) # 2.351974
sd(NICU_pre_overlap$PSI)# sd = 0.4795807
sum(NICU_post$PSI)/nrow(NICU_post) # 2.282895　
sd(NICU_post$PSI)# sd = 0.5062399


# Mindfulness (FFMQ) pre-post changes
t_FFMQ_avg <- t.test(NICU_post_FFMQ_avg, NICU_pre_overlap_FFMQ_avg, paired = TRUE)
# t = 1.7226, df = 14, p-value = 0.107
cohen.d(NICU_post_FFMQ_avg, NICU_pre_overlap_FFMQ_avg, paired = TRUE, hedges.correction=TRUE)
# Hedge's g: 0.3206619 (-0.07032186 to 0.71164562)
#FFMQ total score pre, post 
sum(NICU_pre_overlap_FFMQ_avg)/15 # 3.16
sd(NICU_pre_overlap$FFMQ)# sd = 0.3333809
sum(NICU_post_FFMQ_avg)/15 # 3.294386　
sd(NICU_post_FFMQ_avg)# sd = 0.4263446


# Self-compassion (SCS) pre-post changes
t_SCS_avg <- t.test(NICU_post$SCS, NICU_pre_overlap$SCS, paired = TRUE)
# t = 1.8217, df = 15, p-value = 0.0885
cohen.d(NICU_post$SCS, NICU_pre_overlap$SCS, paired = TRUE, hedges.correction=TRUE)
# Hedge's g: 0.2368748 (-0.0323805 to 0.5061301)
#SCS total score pre, post 
sum(NICU_pre_overlap$SCS)/nrow(NICU_pre_overlap) # 3.005208
sd(NICU_pre_overlap$SCS) # sd = 0.8105032
sum(NICU_post$SCS)/nrow(NICU_post) # 3.213542　
sd(NICU_post$SCS) # sd = 0.8525391


# Loading the usage data
usage <- read.csv("MELON_usage.csv")
 
# Correlation test
  # Calculate the Ratio of Time Live vs Total Time spent
    # (Live)/(Live + Archive)
usage$Sum_real_min <- usage$Week1_real_min + usage$Week2_real_min + usage$Week3_real_min + usage$Week4_real_min
usage$Sum_rec_min <- usage$Week1_rec_min + usage$Week2_rec_min + usage$Week3_rec_min + usage$Week4_rec_min
live_rec_ratio <- usage$Sum_real_min/(usage$Sum_real_min+usage$Sum_rec_min)

# Omitting the participant without responses on FFMQ questions at post-intervention
usage_FFMQ <- usage %>%
  filter(NICU_ID != "A014")
usage_FFMQ$Sum_real_min
# the Ratio of Time Live vs Total Time for FFMQ
live_rec_ratio_FFMQ <- usage_FFMQ$Sum_real_min/(usage_FFMQ$Sum_real_min+usage_FFMQ$Sum_rec_min)
# Correlation between the time ratio and pre-post changes in the average FFMQ scores
NICU_FFMQchange <- NICU_post_FFMQ_avg - NICU_pre_overlap_FFMQ_avg
cor.test(NICU_FFMQchange, live_rec_ratio_FFMQ, method = "pearson", exact = FALSE) # r= 0.5753075, p = 0.02484


# STAI changes and time ratio
NICU_STAIchange <- NICU_post$STAI- NICU_pre_overlap$STAI
cor.test(NICU_STAIchange, live_rec_ratio, method = "pearson", exact = FALSE) # r= -0.421787, p = 0.1037
# PSI changes and real-recorded
NICU_PSIchange <- NICU_post$PSI- NICU_pre_overlap$PSI
cor.test(NICU_PSIchange, live_rec_ratio, method = "pearson", exact = FALSE) # r= -0.2403653, p = 0.3699
# SCS changes and real-recorded
NICU_SCSchange <- NICU_post$SCS- NICU_pre_overlap$SCS
cor.test(NICU_SCSchange, live_rec_ratio, method = "pearson", exact = FALSE) # r= 0.2786749, p = 0.2959

  
# Simple linear regression between the time ratio (x) and outcomes (y)
  # trait anxiety
anx_time_lm <- lm(NICU_STAIchange ~ live_rec_ratio)
summary(anx_time_lm) # t = -1.741, df = 14, p = 0.1037, R_sq = 0.1192
  # parenting stresss
stress_time_lm <- lm(NICU_PSIchange ~ live_rec_ratio)
summary(stress_time_lm) # t = -0.927, df = 14, p = 0.3699, R_sq = -0.009526
  # mindfulness
ffmq_time_lm <- lm(NICU_FFMQchange ~ live_rec_ratio_FFMQ)
summary(ffmq_time_lm) # t = 2.536, df = 13, p = 0.02484, R_sq = 0.2795
  # self-compassion
scs_time_lm <- lm(NICU_SCSchange ~ live_rec_ratio)
summary(scs_time_lm) # t = 1.086, df = 14, p = 0.2959, R_sq = 0.01178

