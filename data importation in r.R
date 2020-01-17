SWISS <- datasets::swiss
 View(SWISS)
write.csv(SWISS,"swissdata.csv")
 setwd("C:/Users/christopher/Desktop/r practices")
 write.csv(SWISS,"swissdata.csv")
 
 
 
 #### method 2
 
 library(tidyverse)
 write_csv(datasets::swiss,"C:/Users/christopher/Desktop/r practices/chris_swiss3.csv")
 
 
 library(psych)
describe(swiss) 
summary(swiss)


# if we want to do regression we need to check for assumotions 
#multicolinearity , nornmality , 
# if p value is <0.05 we reject ho implying the data is not normal
#if p value is >0.5 the data is normal 
#we use shaapiro test
rownames(swiss) <- NULL
shapiro.test(swiss$Infant.Mortality)

for(i in 1:6){
  a <- shapiro.test(swiss[,i])
  print(a)
}

car::qqPlot(swiss)

qqplot(swiss[,4],swiss[,2])
# the result shows the data is not normal



##collinearity 
## does mortality depend on fertility
swiss4 <- subset(swiss[,c(1,6)])

test <- lm(data = swiss4,Infant.Mortality~Fertility)
test

##test for multicollinearity 
#we  use vif 
#if greater than 10 implies multicollinearity 

Anova(test)
##ANOVA 
test2 <- lm((swiss[,6])~(swiss[,1])+swiss[,2])
test2
anova



for(i in 1:12){ 
  a <- shapiro.test(USJ[,i])
  #b <- kruskal.test(USJ[,i])
  print(a)
  #print(b)
}

## to get description of a data set 

?USJudgeRatings

### rechecking for multicollinearity using graphs instead of shapiro





dev.new()
par(mfrow=c(3,2))
for(i in 1:6){
  aloo <- hist(swiss[,i]) 
  aloo
}

dev.off()


## ANOVA TESTS[continous and categorical data ] ,T TESTS ,ONE sample and 2 sample and 
#chi square[used when we have a cross tabulation table and mostly used when we have categorical data ] 

mobile <-read_csv("https://raw.githubusercontent.com/vmandela99/Class-assignment-12th-july-2019/master/mobilemoney_data.csv")
write.csv(mobile,"mobile moneydata.csv")

vars <- table(mobile$gender,mobile$mm_account)

chisq.test(vars)
#mobile %>% rename(mobile$mm_account_telco="service provider")


library(gmodels)
crosstable1 <- CrossTable(mobile$gender,mobile$mm_account)


chisq.test(mobile$gender,mobile$mm_account)




#mobile %>% count(mm_account) %>% ggplot(aes(x=mobile$mm_account,y=n))+geom_col()+facet_wrap(~gender)
dev.new()
#library(ggthemes)
ggplot(data = mobile,aes(x=gender,fill=as.factor(mm_account)))+geom_bar()+theme_economist()


## anova method 2
anovamod <- aov(mobile$age~mobile$gender)
summary(anovamod)


males <- filter(mobile,gender=="male")
females <- filter(mobile,gender=="female")

a <- mean(males$age)

c <-sd(males$age)


#to get mode we create a function
getmode <- function(x){
  u <- unique(x)
  u[which.max(tabulate(match(x,u)))]
}
b<- getmode(males$age)


d <- mean(females$age)
e<-sd(females$age)
f <- getmode(females$age)

stats1 <- matrix(c(a,b,c,d,e,f),nrow = 2,byrow = TRUE)
#library(psych)
#describe(males)
#######################################################################
#another way of finding the summary statistics

library(stats)
mfs <- aggregate(mobile$age,by=list(sex=mobile$gender),mean,na.rm=TRUE)
mfs

##############################################

# convert the yes and no to zero and ones 
newdata <- mobile %>% mutate(mm_account1=case_when(mm_account=="yes"~1,mm_account=="no"~0)) %>% drop_na(mm_account1)
##GENERALIZED LINEAR MODELS 
newdata$mm_account <- NULL


glm_mm_acc <- glm(newdata$mm_account1~newdata$age+newdata$gender+newdata$urban,family = binomial(link = "logit"),method = glm.fit)
summary(glm_mm_acc)
# FOR A CATEGORICAL DATA LIKE URBAN AND GENDER IT CALCULATES A ODDS RATIO DEPENDING ON THE ODDS RATIO

# it can even  happen for a categorical variable with 3 levels 
#for examle introducing the variable district
glm_mm_acc2 <- glm(newdata$mm_account1~newdata$age+newdata$gender+newdata$urban+newdata$district,family = binomial(link = "logit"),method = glm.fit)
summary(glm_mm_acc2)
table(newdata$district,newdata$mm_account1)

## plot a bar graph of districts vs mobile account 
########################################################
#one and two sample t tests 
##test if there is a difference in judges farmiliaarity with the law

judge <- datasets::USJudgeRatings
### one sample , is there a difference between the group that got greater than the specified mu
t1 <- t.test(judge[,8],c="greater",mu=50,paired = FALSE,conf.level = 0.95)
t1
t2 <- t.test(judge[,8],c="less",mu=5,paired = FALSE,conf.level = 0.95)
t2


## two sample 
#is it that those who are more familiar with the law make better judjements 
t3 <- t.test(judge[,8],judge[,10],c="two.sided",conf.level = .95,paired = TRUE)
t3


