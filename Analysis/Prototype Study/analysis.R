library('tidyverse')
library('scales')
library('ggsignif')
library('sjPlot')
library('likert')

tukey_detect<-function(dv,Tukey_crit=1.5){
  IQR=IQR(dv,na.rm = TRUE)
  Quant_25=quantile(dv,probs=0.25,na.rm = TRUE)
  Quant_75=quantile(dv,probs=0.75,na.rm = TRUE)
  upper=Quant_75+Tukey_crit*IQR
  lower=Quant_25-Tukey_crit*IQR
  outlier_Tukey=ifelse(dv>upper,1,ifelse(dv<lower,1,0))
  print(outlier_Tukey)
  as.numeric(paste(outlier_Tukey))
}


df <- read.csv('DiaryResults_overview.csv', header=TRUE)

#########
#Data preprocessing
############

colnames(df)
names(df)[names(df) == "ï..ID"] <- "ID"

question_numbers <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6")
questions = c(
  "M01: I felt the urge to take my phone out of the box tonight",
  "M02: I was worried that something is going on that I needed to track",
  "M03: I feel restless and frustrated when putting my phone away",
  "N01: My first impulse after waking up was to check the phone",
  "N02: Today I have used my phone without a goal in mind",
  "N03: I preferred using my smartphone rather than spend time with others"
  
  
  
)

answers <- c("Strongly Disagree", 
             "Disagree", 
             "Somewhat Disagree", 
             "Neutral",
             "Somewhat Agree",
             "Agree",
             "Strongly Agree")


names(df)[4:9] <- question_numbers


N = length(df$ID)

weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

df$weekday_num <- rep(8,N)
df$weekday_num[df$Weekday == weekdays[1]] <- 1
df$weekday_num[df$Weekday == weekdays[2]] <- 2
df$weekday_num[df$Weekday == weekdays[3]] <- 3
df$weekday_num[df$Weekday == weekdays[4]] <- 4
df$weekday_num[df$Weekday == weekdays[5]] <- 5
df$weekday_num[df$Weekday == weekdays[6]] <- 6
df$weekday_num[df$Weekday == weekdays[7]] <- 7

df$week <- rep(0,N)
df$week[df$Day <= 7] <- 1
df$week[df$Day > 7] <- 2

df$prototype[df$Day <= 7] <- "Prototype"
df$prototype[df$Day > 7] <- "Post Prototype"

# outliers_initial1 <- tukey_detect(dv=df$Q1, Tukey_crit = 1.5)
# outliers_initial2 <- tukey_detect(dv=df$Q2, Tukey_crit = 1.5)
# outliers_initial3 <- tukey_detect(dv=df$Q3, Tukey_crit = 1.5)
# outliers_initial4 <- tukey_detect(dv=df$Q4, Tukey_crit = 1.5)
# outliers_initial5 <- tukey_detect(dv=df$Q5, Tukey_crit = 1.5)
# outliers_initial6 <- tukey_detect(dv=df$Q6, Tukey_crit = 1.5)
# df[outliers_initial1 == 1,]$Q1 <- mean(df$Q1)
# df[outliers_initial2 == 1,]$Q2 <- mean(df$Q2)
# df[outliers_initial3 == 1,]$Q3 <- mean(df$Q3)
# df[outliers_initial4 == 1,]$Q4 <- mean(df$Q4)
# df[outliers_initial5 == 1,]$Q5 <- mean(df$Q5)
# df[outliers_initial6 == 1,]$Q6 <- mean(df$Q6)

df$Q1_text[df$Q1 == 1] <- answers[1]
df$Q1_text[df$Q1 == 2] <- answers[2]
df$Q1_text[df$Q1 == 3] <- answers[3]
df$Q1_text[df$Q1 == 4] <- answers[4]
df$Q1_text[df$Q1 == 5] <- answers[5]
df$Q1_text[df$Q1 == 6] <- answers[6]
df$Q1_text[df$Q1 == 7] <- answers[7]

df$Q2_text[df$Q2 == 1] <- answers[1]
df$Q2_text[df$Q2 == 2] <- answers[2]
df$Q2_text[df$Q2 == 3] <- answers[3]
df$Q2_text[df$Q2 == 4] <- answers[4]
df$Q2_text[df$Q2 == 5] <- answers[5]
df$Q2_text[df$Q2 == 6] <- answers[6]
df$Q2_text[df$Q2 == 7] <- answers[7]

df$Q3_text[df$Q3 == 1] <- answers[1]
df$Q3_text[df$Q3 == 2] <- answers[2]
df$Q3_text[df$Q3 == 3] <- answers[3]
df$Q3_text[df$Q3 == 4] <- answers[4]
df$Q3_text[df$Q3 == 5] <- answers[5]
df$Q3_text[df$Q3 == 6] <- answers[6]
df$Q3_text[df$Q3 == 7] <- answers[7]

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

summary(df)



#Q1_mean <- mean(df$Q1, na.rm = TRUE)
#Q2_mean <- mean(df$Q2, na.rm = TRUE)
#Q3_mean <- mean(df$Q3, na.rm = TRUE)
#Q4_mean <- mean(df$Q4, na.rm = TRUE)
#Q5_mean <- mean(df$Q5, na.rm = TRUE)
#Q6_mean <- mean(df$Q6, na.rm = TRUE)

#df$Q1[is.na(df$Q1)] <- Q1_mean
#df$Q2[is.na(df$Q2)] <- Q2_mean
#df$Q3[is.na(df$Q3)] <- Q3_mean
#df$Q4[is.na(df$Q4)] <- Q4_mean
#df$Q5[is.na(df$Q5)] <- Q5_mean
#df$Q6[is.na(df$Q6)] <- Q6_mean


#############

summary(df)

person_mean <- aggregate(.~df$ID, df[, c(2,4:9)], mean, na.action=na.omit)
day_mean <- aggregate(.~Day, df[, c(2,4:9)], mean, na.action=na.omit)
weekday_mean <- aggregate(.~Weekday, df[, c(3,4:9)], mean, na.action=na.omit)
week_mean <- aggregate(.~week, df[, c(4:9,11)], mean, na.action=na.omit)

df_weekday <- df[!(df$Weekday == "Saturday" | df$Weekday == "Sunday"),]
df_weekend <- df[(df$Weekday == "Saturday" | df$Weekday == "Sunday"),]

person_mean_weekday <- aggregate(.~ID, df_weekday[, c(1,4:9)], mean, na.action=na.omit)
person_mean_weekend <- aggregate(.~ID, df_weekend[, c(1,4:9)], mean, na.action=na.omit)
day_mean_weekday <- aggregate(.~Day, df_weekday[, c(2,4:9)], mean, na.action=na.omit)
day_mean_weekend <- aggregate(.~Day, df_weekend[, c(2,4:9)], mean, na.action=na.omit)

df$Day <- factor(df$Day)

predisposed = c("AE", "DG", "JP", "IB", "JT")
df$Predisposition <- c(rep("Low", length(df$ID)))
df$Predisposition[df$ID %in% predisposed] <- "High"

df$Predisposition <- factor(df$Predisposition)

df$proto_predis <- c(rep("Low & During", length(df$ID)))
df$proto_predis[df$prototype == "Prototype" & df$Predisposition == "High"] <- "High & During"
df$proto_predis[df$prototype == "Post Prototype" & df$Predisposition == "Low"] <- "Low & Post"
df$proto_predis[df$prototype == "Post Prototype" & df$Predisposition == "High"] <- "High & Post"

df$proto_predis <- factor(df$proto_predis, c("High & Post", "High & During",  "Low & Post", "Low & During"  ))

sum(df$proto_predis == "High & Post")
sum(df$proto_predis == "Low & Post")
sum(df$proto_predis == "High & During")
sum(df$proto_predis == "Low & During")

sum(df$prototype == "Prototype")
sum(df$prototype == "Post Prototype")

# ### plot the responses by day number
# plt <- ggplot(df, aes(x=Day, y=Q4, fill=ID)) +
#   geom_bar(stat="identity", position="dodge")
# plt
# 
# plt <- ggplot(df, aes(x=Day, y=Q3, fill=Predisposition)) +
#   geom_boxplot()
# plt
# 
# 
# 
# ### mean responses by day number
# plt <- ggplot(day_mean, aes(x=Day, y=Q1)) +
#   geom_line()
# plt
# 
# ### mean responses by day number
# plt <- ggplot(day_mean, aes(x=Day, y=Q2)) +
#   geom_line()
# plt
# 
# 
# 
# 
# ### responses by day number with means
# plt <- ggplot() +
#   geom_point(data=df, aes(x=Day, y=Q1, color=ID)) +
#   geom_line(data=day_mean, aes(x=Day, y=Q1), color="black", size = 2) 
# plt
# 
# ### responses by day number with weekends removed
# plt <- ggplot() +
#   geom_point(data=df_weekday, aes(x=Day, y=Q1, color=ID)) +
#   geom_line(data=day_mean_weekday, aes(x=Day, y=Q1), color="black", size = 2) 
# plt
# 
# ### responses by day number but only weekends
# plt <- ggplot() +
#   geom_point(data=df_weekend, aes(x=Day, y=Q1, color=ID)) +
#   geom_line(data=day_mean_weekend, aes(x=Day, y=Q1), color="black", size = 2) 
# plt
# 
# df$Weekday <- factor(df$Weekday, levels = weekdays)
# weekday_mean$Weekday <- as.numeric(weekday_mean$Weekday)
# 
# ### responses by weekday with means
# plt <- ggplot() +
#   geom_point(data=df, aes(x=Weekday, y=Q1, color=ID)) +
#   geom_line(data=weekday_mean, aes(x=Weekday, y=Q1), color="black", size = 2) 
# 
# plt
# 
# 
# 
# ### plot the responses week 1 vs week 2
# plt <- ggplot(df, aes(x=week, y=Q1, color = ID)) +
#   geom_point()
# plt



#### likert plots
likert_groups = df$week



# df_likert <- data.frame(df$prototype, df[4:9])
# df_likert$Q1 <- factor(df_likert$Q1, levels = seq(1,7))
# df_likert$Q2 <- factor(df_likert$Q2, levels = seq(1,7))
# df_likert$Q3 <- factor(df_likert$Q3, levels = seq(1,7))
# df_likert$Q4 <- factor(df_likert$Q4, levels = seq(1,7))
# df_likert$Q5 <- factor(df_likert$Q5, levels = seq(1,7))
# df_likert$Q6 <- factor(df_likert$Q6, levels = seq(1,7))
# names(df_likert)[2:7] <- questions
# df_likert
# 
# results = likert(df_likert[2:7], grouping = df_likert$df.prototype)
# results
# plot(results, type="bar")
# 
# #### likert plots try 2
# df_likert2 <- data.frame(df$prototype, df[13:18])
# df_likert2$Q1_text <- factor(df_likert2$Q1_text, levels = answers)
# df_likert2$Q2_text <- factor(df_likert2$Q2_text, levels = answers)
# df_likert2$Q3_text <- factor(df_likert2$Q3_text, levels = answers)
# df_likert2$Q4_text <- factor(df_likert2$Q4_text, levels = answers)
# df_likert2$Q5_text <- factor(df_likert2$Q5_text, levels = answers)
# df_likert2$Q6_text <- factor(df_likert2$Q6_text, levels = answers)
# names(df_likert2)[2:7] <- questions
# df_likert2
# 
# results2 = likert(df_likert2[2:7], grouping = df_likert2$df.prototype)
# results2
# plot(results2, type="bar")

#### likert plots try 3
df_likert3 <- data.frame(df$proto_predis, df[13:18])
df_likert3$Q1_text <- factor(df_likert3$Q1_text, levels = answers)
df_likert3$Q2_text <- factor(df_likert3$Q2_text, levels = answers)
df_likert3$Q3_text <- factor(df_likert3$Q3_text, levels = answers)
df_likert3$Q4_text <- factor(df_likert3$Q4_text, levels = answers)
df_likert3$Q5_text <- factor(df_likert3$Q5_text, levels = answers)
df_likert3$Q6_text <- factor(df_likert3$Q6_text, levels = answers)
names(df_likert3)[2:7] <- questions
df_likert3

results3 = likert(df_likert3[c(2:7)], grouping = df_likert3$df.proto_predis)
plot(results3, type="bar")

results4 = likert(df_likert3[c(3:6)], grouping = df_likert3$df.proto_predis)
plot(results4, type="bar")



#########################
# significance
#####################
df_Q<- data.frame("ID" = rep(df$ID, 6),
                  "Condition" = rep(df$prototype, 6), 
                   "Question" = c(rep("Q1", N), rep("Q2", N), rep("Q3", N),
                                  rep("Q4", N), rep("Q5", N), rep("Q6", N)),
                   "Response" = c(df$Q1, df$Q2, df$Q3, df$Q4, df$Q5, df$Q6),
                  "Predisposition" = rep(df$Predisposition, 6),
                  "Proto_Predis" = rep(df$proto_predis, 6))

df_Q$Condition <- factor(df_Q$Condition, levels = c("Prototype", "Post Prototype"))
df_Q$Predisposition <- factor(df_Q$Predisposition)


df_Q_box <- ggplot(df_Q, aes(x = Question, y = Response, fill=Condition, color=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Prototype Diary Questions") +
  geom_signif(y_position = c(8,8,8), xmin=c(3.8,4.8,5.8), xmax=c(4.2,5.2,6.2), annotation=c("***", "**", "***"))
df_Q_box


df_Q1_box <- ggplot(df_Q[df_Q$Question == "Q1",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q1")
df_Q1_box

df_Q2_box <- ggplot(df_Q[df_Q$Question == "Q2",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q2")
df_Q2_box

df_Q3_box <- ggplot(df_Q[df_Q$Question == "Q3",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q3")
df_Q3_box

df_Q4_box <- ggplot(df_Q[df_Q$Question == "Q4",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q4")
df_Q4_box

df_Q5_box <- ggplot(df_Q[df_Q$Question == "Q5",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q5")
df_Q5_box

df_Q6_box <- ggplot(df_Q[df_Q$Question == "Q6",], aes(x = Condition, y = Response, fill=Predisposition)) +
  geom_boxplot() +
  scale_y_continuous(name = "Response") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Q6")
df_Q6_box

t.test(df$Q1[df$week == 1], df$Q1[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q2[df$week == 1], df$Q2[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q3[df$week == 1], df$Q3[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q4[df$week == 1], df$Q4[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q5[df$week == 1], df$Q5[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q6[df$week == 1], df$Q6[df$week == 2], paired=TRUE, p.adjust.method="bonferroni")

t.test(df$Q1[df$week == 1 & df$Predisposition == "High"], df$Q1[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q1[df$week == 1 & df$Predisposition == "Low"], df$Q1[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q2[df$week == 1 & df$Predisposition == "High"], df$Q2[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q2[df$week == 1 & df$Predisposition == "Low"], df$Q2[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q3[df$week == 1 & df$Predisposition == "High"], df$Q3[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q3[df$week == 1 & df$Predisposition == "Low"], df$Q3[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q4[df$week == 1 & df$Predisposition == "High"], df$Q4[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q4[df$week == 1 & df$Predisposition == "Low"], df$Q4[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q5[df$week == 1 & df$Predisposition == "High"], df$Q5[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q5[df$week == 1 & df$Predisposition == "Low"], df$Q5[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q6[df$week == 1 & df$Predisposition == "High"], df$Q6[df$week == 2 & df$Predisposition == "High"], paired=TRUE, p.adjust.method="bonferroni")
t.test(df$Q6[df$week == 1 & df$Predisposition == "Low"], df$Q6[df$week == 2 & df$Predisposition == "Low"], paired=TRUE, p.adjust.method="bonferroni")



wilcox.test(df$Q1 ~ df$week, data = df)
wilcox.test(df$Q2 ~ df$week, data = df)
wilcox.test(df$Q3 ~ df$week, data = df)
wilcox.test(df$Q4 ~ df$week, data = df, paired=TRUE, p.adjust.method="bonferroni", exact = FALSE)
wilcox.test(df$Q5 ~ df$week, data = df, paired=TRUE, p.adjust.method="bonferroni", exact = FALSE)
wilcox.test(df$Q6 ~ df$week, data = df, paired=TRUE, p.adjust.method="bonferroni", exact = FALSE)

oneway.test(df$Q1 ~ df$week, data = df)
oneway.test(df$Q2 ~ df$week, data = df)
oneway.test(df$Q3 ~ df$week, data = df)
oneway.test(df$Q4 ~ df$week, data = df)
oneway.test(df$Q5 ~ df$week, data = df)
oneway.test(df$Q6 ~ df$week, data = df)

