use1 <- read.csv("use1.csv")
# Revised the name under Circle_map variable, original data has capital or lower case issues.
# I didn't do the cleaning or other jobs, which might be needed in the regression precedures.

# 1st part EDA
# create a data summary with things like 
# counts of for each condition (harm or help), 
# how many subjects said the child was hiding in each location. 
# Be sure to separate out the data based on the true location, 
# so you can distinguish lying from telling the truth.


condition1 <- subset(use1,use1$Condition==1)
condition2 <- subset(use1,use1$Condition==2)
length(condition1$Condition) #50 stealing
length(condition2$Condition) #47 giving
unique(condition1$Circled_Map)
library(plyr)
count(condition1$Circled_Map)

#x freq
#1       fence    9
#2 Recycle Bin   13
#3       rocks   16
#4        tree   12

count(condition2$Circled_Map)
#x freq
#1       fence   18
#2 Recycle Bin   11
#3       rocks   15
#4        tree    3

# 2nd part EDA
# Since we have two age groups and two genders,separate out the data for the different sets.
# create one histogram for the harm condition, with the child hiding on the left,
# which shows the number of subjects who responded in each location, 
# broken down by age group. And do the same thing, breaking down by gender, and so on. 
library(ggplot2)
ggplot(data.frame(condition1$Circled_Map), aes(x=condition1$Circled_Map)) + geom_bar()
ggplot(data.frame(condition2$Circled_Map), aes(x=condition2$Circled_Map)) + geom_bar()
ggplot(data.frame(use1$Circled_Map), aes(x=use1$Circled_Map)) + geom_bar()
attach(use1)
ggplot(data.frame(Circled_Map), aes(x=Circled_Map,group=Gender,fill=Gender)) + geom_bar()
ggplot(data.frame(Circled_Map), aes(x=Circled_Map,group=AgeGroup,fill=AgeGroup)) + geom_bar()

#created count of lies based on different conditions, different age groups and genders.
condition1<-subset(use1, use1$Condition == "1")
ggplot(condition1, aes(x=condition1$Lie, group = Gender, fill= Gender))+geom_bar()
ggplot(condition1, aes(x=condition1$Lie, group = AgeGroup, fill= AgeGroup))+geom_bar()

condition2<- subset(use1, use1$Condition =="2")
ggplot(condition2, aes(x=condition2$Lie, group = Gender, fill= Gender))+geom_bar()
ggplot(condition2, aes(x=condition2$Lie, group = AgeGroup, fill= AgeGroup))+geom_bar()

#plots for same age group but different condition
younger<-subset(use1, use1$AgeGroup == "1")
older<- subset(use1, use1$AgeGroup == "2")

ggplot(younger, aes(x=younger$Condition, group = Lie, fill= Lie))+geom_bar()
ggplot(older, aes(x=older$Condition, group = Lie, fill= Lie))+geom_bar()

# data analysis 
# create the multinomial logistic models for experiments 1 and 2. 
library(nnet)
lying1 <- subset(use1,select=c(Sub_No,Age,AgeGroup,Gender,Condition,Circled_Map,Lie,If.yes..how.far.))
names(lying1) <- c("id","age","age_gp","gender","condition","circled_map","lie","dis")
lying1$age_gp <- ifelse(lying1$age_gp == 1,0,1)
lying1$condition <- ifelse(lying1$condition == 1,0,1)
mod <- multinom(dis~age_gp+condition,lying1)
summary(mod)
exp(coef(mod))
#probability for each children to lie
prob_for_child <- predict(mod,lying1,"probs")
prob_for_child

new1 <- melt(t1,id.vars=c("age","gender","con"))
saveRDS(newprob1,"p1.rds")

lying2 <- MoralLying2
newdata<-data.frame(lying2$AgeGroup, lying2$Condition, lying2$Gender, lying2$`If yes, how far?`,lying2$Circled_Map)
colnames(newdata)<-c("agegroup","condition","gender","dis","circled_map")
newdata$agegroup <- ifelse(newdata$agegroup == 1,0,1)
#Hiding indicator
lying2 <- newdata
lying2$hide[lying2$circled_map=="Tree" & lying2$dis == "2"] <- "0"
lying2$hide[lying2$circled_map=="Tree" & lying2$dis == "1"] <- "1"
lying2$hide[lying2$circled_map=="Fence" & lying2$dis == "0"] <- "1"
lying2$hide[lying2$circled_map=="Fence" & lying2$dis == "3"] <- "0"
lying2$hide[lying2$circled_map=="Rocks" & lying2$dis == "3"] <- "1"
lying2$hide[lying2$circled_map=="Rocks" & lying2$dis == "0"] <- "0"
lying2$hide[lying2$circled_map=="Recycling bin" & lying2$dis == "2"] <- "1"
lying2$hide[lying2$circled_map=="Recycling bin" & lying2$dis == "1"] <- "0"
lying2$hide[lying2$circled_map=="stones" & lying2$dis == "3"] <- "1"
lying2$hide[lying2$circled_map=="sontes" & lying2$dis == "0"] <- "0"
lying2$hide[lying2$circled_map=="tree" & lying2$dis == "2"] <- "0"
lying2$hide[lying2$circled_map=="tree" & lying2$dis == "1"] <- "1"
lying2$hide[lying2$circled_map=="fence" & lying2$dis == "0"] <- "1"
lying2$hide[lying2$circled_map=="fence" & lying2$dis == "3"] <- "0"
lying2$hide[lying2$circled_map=="Recycling" & lying2$dis == "2"] <- "1"
lying2$hide[lying2$circled_map=="Recycling" & lying2$dis == "1"] <- "0"
lying2$hide[lying2$circled_map=="Recycling Bin" & lying2$dis == "2"] <- "1"
lying2$hide[lying2$circled_map=="Recycling Bin" & lying2$dis == "1"] <- "0"
saveRDS(lying2,"lying2.rds")
