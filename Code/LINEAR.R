#Include Library Packages:
library("RSQLite")
library(lattice)
library(MASS)
library(plyr)
library(car)
library(leaps)

#Read the input data sets:
options(max.print=1000)
Result_data=read.csv("C:\\Users\\Ramki\\Downloads\\Mcomp\\Sem2\\HowBA\\Final Project\\ACC_CRIME1.csv",header=TRUE)

#Convert string values to numeric if any:

is.numeric(Result_data$CRIME_TYPE)
Result_data$CRIME_TYPE=as.numeric(Result_data$CRIME_TYPE)

#Scatterplot to find the coorelation:

#Change NA values to 0:
Result_data[is.na(Result_data)] <- 0

#Create a log for POPULATION:
#plot(Result_data$POPULATION,Result_data$MINUTESPLAYED)
Result_data$POPULATION=log(Result_data$POPULATION)

Result_data$ACC_COUNT=log(Result_data$ACC_COUNT)
scatterplot(WEATHER~ACC_COUNT+DRINKING, reg.line=lm, smooth=TRUE, spread=TRUE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=Result_data)

scatterplot(ACC_COUNT+CRIME_COUNT~AGE+SEX+DRUGS+CRIME_TYPE+DRINKING, reg.line=lm, smooth=TRUE, spread=TRUE, id.method='mahal', id.n = 5, boxplots='xy', span=0.5, data=Result_data)




#Create a data frame:
Result_data1 = data.frame(Result_data)




#Perform Liner Regression Analysis:
LinearM <- lm(ACC_COUNT~STATE+CITY+COUNTY+AGE+SEX+DRINKING+DRUGS+WEATHER+CRIME_TYPE+CRIME_COUNT+HEALTH_WEATHER,data = Result_data1 )
abline(LinearM)


#Perform stepAIC function:
step <- stepAIC(LinearM, direction="both")
step$anova


LinearM1 <- lm(ACC_COUNT ~ STATE + CITY + COUNTY + SEX + DRUGS + WEATHER +HEALTH_WEATHER,data = Result_data1 )
summary(LinearM1)



LinearM <- lm(CRIME_COUNT~STATE+CITY+COUNTY+SEX+DRINKING+DRUGS+WEATHER+CRIME_TYPE+ACC_COUNT+HEALTH_WEATHER,data = Result_data1 )
summary(LinearM)
