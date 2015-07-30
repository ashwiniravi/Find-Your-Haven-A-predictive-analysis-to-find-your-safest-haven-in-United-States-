###################ACCIDENT Vs WEATHER Vs DRINKING####################
data1<-read.csv("Temperature_weather.csv", header=TRUE)
data1$TEMP_TYPE<-as.factor(data1$TEMP_TYPE)
data1$WEATHER<-as.factor(data1$WEATHER)
data<-data1
data[is.na(data)] <- 0
data$TEMP = NULL
#data1$TEMP_TYPE<-as.numeric(data1$TEMP_TYPE)
#data1$WEATHER<-as.numeric(data1$WEATHER)
is.numeric(data$TEMP_TYPE)
##########################################################
##########Finding the number of Clusters##################
wss <- (nrow(data[,3:7])-1)*sum(apply(data[,3:7],2,var))
for (i in 2:10) wss[i] <- sum(kmeans(data[,3:7], centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="ACCIDENT Vs TEMPERATURE Vs DRINKING")

kdata<- kmeans(data[,3:7], 4,nstart=20)
plot(data1$TEMP,c(data1$DRINKING+data1$FATALS),col=kdata$cluster)
KMClusterMeans = aggregate(data1[,3:9],by=list(kdata$cluster),FUN=mean)
player.clustered <- data.frame(data1, kdata$cluster)
head(KMClusterMeans)
######################################################
############LINEAR REGRESSION###########################

LinearM1 <- lm(FATALS~DRINKING,data = data1 )
LinearM2<-lm(DRINKING~WEATHER+TEMP,data=data1)
LinearM3<-lm(FATALS~DRINKING+WEATHER+TEMP,data=data1)
summary(LinearM1)
summary(LinearM2)
summary(LinearM3)
scatterplot(FATALS~DRINKING, reg.line=lm, smooth= TRUE, data=data1,main="FATALITY VS TEMPERATURE_DRINKING", 
            xlab="Fatality", ylab="DRUNK_DRIVE ", pch=16)
#Plot graph for Linear Regression:
residualPlots(LinearM1)
avPlots(LinearM1, id.n=2, id.cex=0.7)
qqPlot(LinearM1, id.n=3)
outlierTest(LinearM1)
influenceIndexPlot(LinearM1, id.n=3)
#############################################

