# ScoringCodeBCI

Spring15OnlinePre=read.csv("Spring15RCVPreBCI.csv") #this is the chaos to read and clean all the data in#
Spring15OnlinePost=read.csv("Spring15RCVPostBCI.csv")

write.csv(Spring15OnlinePre,file="Spring15OnlinePre.csv")
write.csv(Spring15OnlinePost,file="Spring15OnlinePost.csv")

Spring15OnlinePre=read.csv("Spring15OnlinePre.csv")
Spring15OnlinePost=read.csv("Spring15OnlinePost.csv")

Spring15OnlinePre$X=NULL
Spring15OnlinePost$X=NULL

names(Spring15OnlinePre)=c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q24","Q25","Q26","Q27","Q28","Q29","Q30")
names(Spring15OnlinePost)=c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q24","Q25","Q26","Q27","Q28","Q29","Q30")

source("ScoringCodeforBCI.R") #this scored it and is magic#
S15OnlinePreScore=Score(Spring15OnlinePre)
S15OnlinePostScore=Score(Spring15OnlinePost)


names(S15OnlinePreScore)=c("ID","Pre")
names(S15OnlinePostScore)=c("ID","Post")


S15OnlinePrePost <- merge(S15OnlinePreScore,S15OnlinePostScore,by=c("ID"))
write.csv(S15OnlinePrePost,file="Spring15OnlinePrePost.csv")

#S15AllSectionsPrePost <- rbind(S15ThursdayPrePost,S15TuesdayPrePost)

t.test(S15OnlinePrePost$Pre,S15OnlinePrePost$Post, paired=TRUE)

summary(S15OnlinePrePost$Pre)
summary(S15OnlinePrePost$Post)

#write.csv(S5AllSectionsPrePost,file="Spring15AllSectionsPrePost.csv")

Spring15SecRepBCI=read.csv("BSC1010_Spring2015_SAT_Section_Repeat.csv")

fit1 <- aov(Post~Pre + Section, data=Spring15SecRepBCI)
summary(fit1)

summary(lm(Post~Pre + Section, data=Spring15SecRepBCI))

library(car)
leveneTest(Pre~Section, data=Spring15SecRepBCI)

fit2 <- aov(Post~Pre + Repeat, data=Spring15SecRepBCI)
summary(fit2)

str(Spring15SecRepBCI)
Spring15SecRepBCI$Repeat<-as.factor(Spring15SecRepBCI$Repeat)

summary(lm(Post~Pre+Repeat,data=Spring15SecRepBCI))
summary(lm(Post~Pre+SAT,data=Spring15SecRepBCI))
summary(lm(Post~Pre,data=Spring15SecRepBCI))


fit3 <- aov(Post~Pre+AMPM, data=Spring15SecRepBCI)
summary(fit3)

summary(lm(Post~Pre+AMPM,data=Spring15SecRepBCI))

library(psych)
describeBy(Spring15SecRepBCI$Pre)
describeBy(Spring15SecRepBCI$Post)

t.test(Spring15SecRepBCI$Post,Spring15SecRepBCI$Pre, paired=TRUE)
describeBy(Spring15SecRepBCI$Post, Spring15SecRepBCI$AMPM)
describeBy(Spring15SecRepBCI$Pre, Spring15SecRepBCI$AMPM)
