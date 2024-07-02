library('tidyverse')
library('likert')

df <- read.csv('./data.csv', header=TRUE)

df_questionKey <- read.csv("./question_key.csv", header=TRUE)
names(df_questionKey)[names(df_questionKey) == "ï..Q_number"] <- "Q_Number"
likert_questions = c("Are you distracted by your smartphone?",
                     "I have already tried to adapt my smartphone usage. (e.g. by using apps, time limits, blockers, ...)",
                     "Having office tools (e.g. E-Mail, Slack, ...) on my private smartphone at home stresses me out.",
                     "Using my smartphone during down time is relaxing.",
                     "When I leave my smartphone by my bed, I pick it up and use it before sleeping and after I wake up.",
                     "Using my smartphone in bed reduces the quality of my sleep.",
                     "When someone is using their smartphone in social settings or gatherings I feel annoyed.",
                     "Having my smartphone in reach hinders my focus on other tasks.")


N = length(df$Age)
N_female = sum(df$Gender == "Female")
N_male = sum(df$Gender == "Male")
N_diverse = sum(df$Gender == "Diverse")
female_perc = 100*N_female/N
male_perc = 100*N_male/N
diverse_perc = 100*N_diverse/N

ages = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55 or older")
times = c("<1 hour", "1-2 hours", "2-3 hours", "3-4 hours", ">4 hours")
answers <- c("Strongly Disagree", 
             "Disagree", 
             "Somewhat Disagree", 
             "Neutral",
             "Somewhat Agree",
             "Agree",
             "Strongly Agree")

df$Age <- factor(df$Age, levels = ages)
df$Q1 <- factor(df$Q1, levels = times)


df$Q2_text[df$Q2 == 1] <- answers[1]
df$Q2_text[df$Q2 == 2] <- answers[2]
df$Q2_text[df$Q2 == 3] <- answers[3]
df$Q2_text[df$Q2 == 4] <- answers[4]
df$Q2_text[df$Q2 == 5] <- answers[5]
df$Q2_text[df$Q2 == 6] <- answers[6]
df$Q2_text[df$Q2 == 7] <- answers[7]

df$Q3_text[df$Q3 == 1] <- "No"
df$Q3_text[df$Q3 == 2] <- "Sometimes"
df$Q3_text[df$Q3 == 3] <- "Yes"

df$Q4_text[df$Q4 == 1] <- answers[1]
df$Q4_text[df$Q4 == 2] <- answers[2]
df$Q4_text[df$Q4 == 3] <- answers[3]
df$Q4_text[df$Q4 == 4] <- answers[4]
df$Q4_text[df$Q4 == 5] <- answers[5]
df$Q4_text[df$Q4 == 6] <- answers[6]
df$Q4_text[df$Q4 == 7] <- answers[7]

df$Q5_text[df$Q5 == 1] <- answers[1]
df$Q5_text[df$Q5 == 2] <- answers[2]
df$Q5_text[df$Q5 == 3] <- answers[3]
df$Q5_text[df$Q5 == 4] <- answers[4]
df$Q5_text[df$Q5 == 5] <- answers[5]
df$Q5_text[df$Q5 == 6] <- answers[6]
df$Q5_text[df$Q5 == 7] <- answers[7]

df$Q6_text[df$Q6 == 1] <- answers[1]
df$Q6_text[df$Q6 == 2] <- answers[2]
df$Q6_text[df$Q6 == 3] <- answers[3]
df$Q6_text[df$Q6 == 4] <- answers[4]
df$Q6_text[df$Q6 == 5] <- answers[5]
df$Q6_text[df$Q6 == 6] <- answers[6]
df$Q6_text[df$Q6 == 7] <- answers[7]

df$Q7_text[df$Q7 == 1] <- answers[1]
df$Q7_text[df$Q7 == 2] <- answers[2]
df$Q7_text[df$Q7 == 3] <- answers[3]
df$Q7_text[df$Q7 == 4] <- answers[4]
df$Q7_text[df$Q7 == 5] <- answers[5]
df$Q7_text[df$Q7 == 6] <- answers[6]
df$Q7_text[df$Q7 == 7] <- answers[7]

df$Q8_text[df$Q8 == 1] <- answers[1]
df$Q8_text[df$Q8 == 2] <- answers[2]
df$Q8_text[df$Q8 == 3] <- answers[3]
df$Q8_text[df$Q8 == 4] <- answers[4]
df$Q8_text[df$Q8 == 5] <- answers[5]
df$Q8_text[df$Q8 == 6] <- answers[6]
df$Q8_text[df$Q8 == 7] <- answers[7]

df$Q9_text[df$Q9 == 1] <- answers[1]
df$Q9_text[df$Q9 == 2] <- answers[2]
df$Q9_text[df$Q9 == 3] <- answers[3]
df$Q9_text[df$Q9 == 4] <- answers[4]
df$Q9_text[df$Q9 == 5] <- answers[5]
df$Q9_text[df$Q9 == 6] <- answers[6]
df$Q9_text[df$Q9 == 7] <- answers[7]

df$Q10_text[df$Q10 == 1] <- answers[1]
df$Q10_text[df$Q10 == 2] <- answers[2]
df$Q10_text[df$Q10 == 3] <- answers[3]
df$Q10_text[df$Q10 == 4] <- answers[4]
df$Q10_text[df$Q10 == 5] <- answers[5]
df$Q10_text[df$Q10 == 6] <- answers[6]
df$Q10_text[df$Q10 == 7] <- answers[7]





age_bar <- ggplot(df, aes(x=Age)) +
  geom_bar(fill="#5AB4AC", position = position_dodge(preserve = "single")) + 
  scale_y_continuous(name = "Count") +
  theme_classic()
age_bar

q1_bar <- ggplot(df, aes(x=Q1, fill=Age)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) +
  theme_classic()
q1_bar
q1_bar <- ggplot(df, aes(x=Q1)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q1_bar

# q2_bar <- ggplot(df, aes(x=Q2, fill=Age)) +
#   geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) +
#   theme_classic()
# q2_bar
q2_bar <- ggplot(df, aes(x=Q2)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q2_bar

q3_bar <- ggplot(df, aes(x=Q3)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q3_bar


q4_bar <- ggplot(df, aes(x=Q4)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q4_bar

q5_bar <- ggplot(df, aes(x=Q5)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q5_bar

q6_bar <- ggplot(df, aes(x=Q6)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q6_bar

q7_bar <- ggplot(df, aes(x=Q7)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q7_bar

q8_bar <- ggplot(df, aes(x=Q8)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q8_bar

q9_bar <- ggplot(df, aes(x=Q9)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q9_bar

q10_bar <- ggplot(df, aes(x=Q10)) +
  geom_bar(alpha=0.6, position = position_dodge(preserve = "single")) + 
  theme_classic()
q10_bar

df_timeage <- data.frame("Time" = c(rep(times[1], length(ages)), rep(times[2], length(ages)),
                                      rep(times[3], length(ages)), rep(times[4], length(ages))),
                           "Age" = rep(ages, length(times))
                           )

df_Q1 <- data.frame("Age" = c(rep(ages[1],length(unique(df$Q1))), rep(ages[2],length(unique(df$Q1))),
                               rep(ages[3],length(unique(df$Q1))), rep(ages[4],length(unique(df$Q1))),
                               rep(ages[5],length(unique(df$Q1))), rep(ages[6],length(unique(df$Q1)))),
                    "Response" = rep(times,length(ages)),
                     "Count" = c(100*sum(df$Q1 == times[1] & df$Age == ages[1])/age_counts[1,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[1])/age_counts[1,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[1])/age_counts[1,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[1])/age_counts[1,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[1])/age_counts[1,2],
                              100*sum(df$Q1 == times[1] & df$Age == ages[2])/age_counts[2,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[2])/age_counts[2,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[2])/age_counts[2,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[2])/age_counts[2,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[2])/age_counts[2,2],
                              100*sum(df$Q1 == times[1] & df$Age == ages[3])/age_counts[3,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[3])/age_counts[3,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[3])/age_counts[3,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[3])/age_counts[3,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[3])/age_counts[3,2],
                              100*sum(df$Q1 == times[1] & df$Age == ages[4])/age_counts[4,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[4])/age_counts[4,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[4])/age_counts[4,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[4])/age_counts[4,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[4])/age_counts[4,2],
                              100*sum(df$Q1 == times[1] & df$Age == ages[5])/age_counts[5,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[5])/age_counts[5,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[5])/age_counts[5,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[5])/age_counts[5,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[5])/age_counts[5,2],
                              100*sum(df$Q1 == times[1] & df$Age == ages[6])/age_counts[6,2],
                              100*sum(df$Q1 == times[2] & df$Age == ages[6])/age_counts[6,2],
                              100*sum(df$Q1 == times[3] & df$Age == ages[6])/age_counts[6,2],
                              100*sum(df$Q1 == times[4] & df$Age == ages[6])/age_counts[6,2],
                              100*sum(df$Q1 == times[5] & df$Age == ages[6])/age_counts[6,2]))

q1_bar_age <- ggplot(df_Q1, aes(x=Response, y=Count, fill=Age, color=Age)) +
  geom_bar(alpha=0.6, position = "dodge", stat="identity") + 
  scale_y_continuous(name = "Count") +
  theme_classic()
q1_bar_age

q1_bar_age <- ggplot(df, aes(x=Q2, fill=Age, color=Age)) +
  # geom_histogram(aes(y=..density..), alpha=0.4, position="dodge", binwidth = 1) +
  geom_density(alpha=0.2) + 
  scale_y_continuous(name = "Count") +
  theme_classic()
q1_bar_age



#### likert plots try 2
df_likert <- data.frame(df$Age, df[c("Q2_text", "Q4_text", "Q5_text",
                                     "Q6_text", "Q7_text", "Q8_text", "Q9_text", "Q10_text")])

df_likert$Q2_text <- factor(df_likert$Q2_text, levels = answers)
df_likert$Q4_text <- factor(df_likert$Q4_text, levels = answers)
df_likert$Q5_text <- factor(df_likert$Q5_text, levels = answers)
df_likert$Q6_text <- factor(df_likert$Q6_text, levels = answers)
df_likert$Q7_text <- factor(df_likert$Q7_text, levels = answers)
df_likert$Q8_text <- factor(df_likert$Q8_text, levels = answers)
df_likert$Q9_text <- factor(df_likert$Q9_text, levels = answers)
df_likert$Q10_text <- factor(df_likert$Q10_text, levels = answers)
names(df_likert)[2:9] <- likert_questions
df_likert
results = likert(df_likert[2:8])
results
plot(results, type="bar")

results2 = likert(df_likert[3:7], grouping = df_likert$`Are you distracted by your smartphone?`)
results2
plot(results2, type="bar")

results3 = likert(df_likert[c(6,7,8,9)])

plot(results3, type="bar")

