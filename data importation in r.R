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

