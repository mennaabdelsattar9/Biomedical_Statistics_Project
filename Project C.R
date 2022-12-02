

################################## Project C #############################################

##################### 0- Reading Data set  ##############################################

mydata<-get(load("PWD.RData"))

#Summarize data
summary(PWD)

#assume 1= male, 2= female
PWD$Sex<-factor(PWD$Sex,labels=c('male','female'))

###################################### Descriptive statistics ######################################

#calculate the following:

#mean, median, minimum, maximum, first and third quartile (for each variable). 

mean(mydata$W0, na.rm =TRUE )
mean(mydata$P0, na.rm =TRUE )
mean(mydata$ ADWG0021, na.rm =TRUE )
mean(mydata$ ADWG2150, na.rm =TRUE )
mean(mydata$ ADWG0050, na.rm =TRUE )


median(mydata$W0, na.rm =TRUE )
median(mydata$P0, na.rm =TRUE )
median(mydata$ ADWG0021, na.rm =TRUE )
median(mydata$ ADWG2150, na.rm =TRUE )
median(mydata$ ADWG0050, na.rm =TRUE )

max(mydata$W0, na.rm =TRUE )
max(mydata$P0, na.rm =TRUE )
max(mydata$ ADWG0021, na.rm =TRUE )
max(mydata$ ADWG2150, na.rm =TRUE )
max(mydata$ ADWG0050, na.rm =TRUE )

min(mydata$W0, na.rm =TRUE )
min(mydata$P0, na.rm =TRUE )
min(mydata$ ADWG0021, na.rm =TRUE )
min(mydata$ ADWG2150, na.rm =TRUE )
min(mydata$ ADWG0050, na.rm =TRUE )

quantile(mydata$W0, probs =c(0.25,0.75),na.rm =TRUE)
quantile(mydata$P0, probs =c(0.25,0.75),na.rm =TRUE)
quantile(mydata$ ADWG0021, probs =c(0.25,0.75),na.rm =TRUE)
quantile(mydata$ ADWG2150, probs =c(0.25,0.75),na.rm =TRUE)
quantile(mydata$ ADWG0050, probs =c(0.25,0.75),na.rm =TRUE)

#For the categorical variable existing, calculate a frequency table
counts<-table(factor(PWD[,"Sex"]))
counts

#Calculate the correlation coefficient
#(ADWG0021 and ADWG2150) and (ADWG0021and ADWG0050)
corr1<-cor(mydata$ADWG0021, mydata$ ADWG2150, method = "pearson", use = "complete.obs")
corr2<-cor(mydata$ ADWG0021, mydata$ ADWG2150, method = "spearman", use = "complete.obs")

corr3<-cor(mydata$ADWG0021, mydata$ ADWG0050, method = "pearson", use = "complete.obs")
corr4<-cor(mydata$ ADWG0021, mydata$ ADWG0050, method = "spearman", use = "complete.obs")

#res<-cor.test(mydata$ ADWG0021, mydata$ ADWG2150, method = "pearson")
#sum(is.na(mydata))

################### 2- Graphics ####################

#Generate a bar chart of a categorical variable for the gender (Sex parameter).
barplot(counts, main="Sex", horiz=FALSE,names.arg=c("1", "2"),xlab="Gender",ylab="Frequency")

#Generate a bar chart graph with mean ADWG0021 in  males and females
barplot(tapply(PWD$ADWG0021,list(PWD$Sex),mean,na.rm=T),
        xlab="Gender",ylab="Mean ADWG0021")


#Make a histogram of a continuous variable: "ADWG2150" as well as "ADWG0021".
hist(mydata$ADWG2150,xlab = " ADWG2150", main="Histogram of ADWG2150")
hist(mydata$ADWG0021,xlab = " ADWG0021", main="Histogram of ADWG0021")



#Make a scatterplot of 2 continuous variables (Length) ADWG0050 and (Weight) ADWG0021, and add the regression lines for each gender
plot(mydata$ ADWG0021,mydata$ ADWG0050 , main = "Main title",
     xlab = "ADWG0021", ylab = "ADWG0050",
     pch = 19, frame = FALSE) 

points(mydata$ADWG0021[mydata$Sex=="1"],mydata$ADWG0050[mydata$Sex=="1"],col=2,pch = 19)
points(mydata$ADWG0021[mydata$Sex=="2"],mydata$ADWG0050[mydata$Sex=="2"],col=3,pch = 19)
#Regression Lines
abline(lm( mydata$ADWG0050[mydata$Sex=="1"] ~ mydata$ADWG0021[mydata$Sex=="1"]),col=2)
abline(lm(mydata$ ADWG0050 [mydata$Sex=="2"] ~ mydata$ ADWG0021 [mydata$Sex=="2"]),col=3)


#Make a boxplot of ADWG0021 in  and a separate boxplots per Treatment (as.factors).
boxplot(mydata$ ADWG0021 ~ mydata$ Treatment, main='Box plot sperated by Treatment group', xlab=' Treatment ',ylab=' ADWG0021 ' )

############################## 3- Outlier detection ######################################

# we have two outliers in the dataset which are 178.5714 and 110.1190
outliers <- boxplot(ADWG0021~Treatment,data=mydata)$out
outliers[1]
outliers[2]


# For a categorical variable a frequency table is more appropriate
###table(PWD$Sex)

Data <- PWD[-c(1,6)]

######################## 4- Testing for normality/ homoscedasticity ######################################
## checking normalility using Q-Q plot

## Categorical data
# treatment A
qqnorm(Data[Data$Treatment == "A",]$Feeder, main='Treatment "A" Feeder')
qqline(Data[Data$Treatment == "A",]$Feeder)
qqnorm(Data[Data$Treatment == "A",]$W0, main='Treatment "A" W0')
qqline(Data[Data$Treatment == "A",]$W0)
qqnorm(Data[Data$Treatment == "A",]$ADWG0021, main='Treatment "A" ADWG0021')
qqline(Data[Data$Treatment == "A",]$ADWG0021)
qqnorm(Data[Data$Treatment == "A",]$ADWG2150, main='Treatment "A" ADWG2150')
qqline(Data[Data$Treatment == "A",]$ADWG2150)
qqnorm(Data[Data$Treatment == "A",]$ADWG0050, main='Treatment "A" ADWG0050')
qqline(Data[Data$Treatment == "A",]$ADWG0050)

#treatment B
qqnorm(Data[Data$Treatment == "B",]$Feeder, main='Treatment "B" Feeder')
qqline(Data[Data$Treatment == "B",]$Feeder)
qqnorm(Data[Data$Treatment == "B",]$W0, main='Treatment "B" W0')
qqline(Data[Data$Treatment == "B",]$W0)
qqnorm(Data[Data$Treatment == "B",]$ADWG0021, main='Treatment "B" ADWG0021')
qqline(Data[Data$Treatment == "B",]$ADWG0021)
qqnorm(Data[Data$Treatment == "B",]$ADWG2150, main='Treatment "B" ADWG2150')
qqline(Data[Data$Treatment == "B",]$ADWG2150)
qqnorm(Data[Data$Treatment == "B",]$ADWG0050, main='Treatment "B" ADWG0050')
qqline(Data[Data$Treatment == "B",]$ADWG0050)

#treatment c
qqnorm(Data[Data$Treatment == "C",]$Feeder, main='Treatment "C" Feeder')
qqline(Data[Data$Treatment == "C",]$Feeder)
qqnorm(Data[Data$Treatment == "C",]$W0, main='Treatment "C" W0')
qqline(Data[Data$Treatment == "C",]$W0)
qqnorm(Data[Data$Treatment == "C",]$ADWG0021, main='Treatment "C" ADWG0021')
qqline(Data[Data$Treatment == "C",]$ADWG0021)
qqnorm(Data[Data$Treatment == "C",]$ADWG2150, main='Treatment "C" ADWG2150')
qqline(Data[Data$Treatment == "C",]$ADWG2150)
qqnorm(Data[Data$Treatment == "C",]$ADWG0050, main='Treatment "C" ADWG0050')
qqline(Data[Data$Treatment == "C",]$ADWG0050)

#treatment D
qqnorm(Data[Data$Treatment == "D",]$Feeder, main='Treatment "D" Feeder')
qqline(Data[Data$Treatment == "D",]$Feeder)
qqnorm(Data[Data$Treatment == "D",]$W0, main='Treatment "D" W0')
qqline(Data[Data$Treatment == "D",]$W0)
qqnorm(Data[Data$Treatment == "D",]$ADWG0021, main='Treatment "D" ADWG0021')
qqline(Data[Data$Treatment == "D",]$ADWG0021)
qqnorm(Data[Data$Treatment == "D",]$ADWG2150, main='Treatment "D" ADWG2150')
qqline(Data[Data$Treatment == "D",]$ADWG2150)
qqnorm(Data[Data$Treatment == "D",]$ADWG0050, main='Treatment "D" ADWG0050')
qqline(Data[Data$Treatment == "D",]$ADWG0050)

#treatment E
qqnorm(Data[Data$Treatment == "E",]$Feeder, main='Treatment "E" Feeder')
qqline(Data[Data$Treatment == "E",]$Feeder)
qqnorm(Data[Data$Treatment == "E",]$W0, main='Treatment "E" W0')
qqline(Data[Data$Treatment == "E",]$W0)
qqnorm(Data[Data$Treatment == "E",]$ADWG0021, main='Treatment "E" ADWG0021')
qqline(Data[Data$Treatment == "E",]$ADWG0021)
qqnorm(Data[Data$Treatment == "E",]$ADWG2150, main='Treatment "E" ADWG2150')
qqline(Data[Data$Treatment == "E",]$ADWG2150)
qqnorm(Data[Data$Treatment == "E",]$ADWG0050, main='Treatment "E" ADWG0050')
qqline(Data[Data$Treatment == "E",]$ADWG0050)

# Male
qqnorm(Data[Data$Sex == "male",]$Feeder, main='Male Feeder')
qqline(Data[Data$Sex == "male",]$Feeder)
qqnorm(Data[Data$Sex == "male",]$W0, main='Male W0')
qqline(Data[Data$Sex == "male",]$W0)
qqnorm(Data[Data$Sex == "male",]$ADWG0021, main='Male ADWG0021')
qqline(Data[Data$Sex == "male",]$ADWG0021)
qqnorm(Data[Data$Sex == "male",]$ADWG2150, main='Male ADWG2150')
qqline(Data[Data$Sex == "male",]$ADWG2150)
qqnorm(Data[Data$Sex == "male",]$ADWG0050, main='Male ADWG0050')
qqline(Data[Data$Sex == "male",]$ADWG0050)

#female
qqnorm(Data[Data$Sex == "female",]$Feeder, main='female Feeder')
qqline(Data[Data$Sex == "female",]$Feeder)
qqnorm(Data[Data$Sex == "female",]$W0, main='female W0')
qqline(Data[Data$Sex == "female",]$W0)
qqnorm(Data[Data$Sex == "female",]$ADWG0021, main='female ADWG0021')
qqline(Data[Data$Sex == "female",]$ADWG0021)
qqnorm(Data[Data$Sex == "female",]$ADWG2150, main='female ADWG2150')
qqline(Data[Data$Sex == "female",]$ADWG2150)
qqnorm(Data[Data$Sex == "female",]$ADWG0050, main='female ADWG0050')
qqline(Data[Data$Sex == "female",]$ADWG0050)

## Numarical Data

qqnorm(Data$Feeder, pch = 1, frame = FALSE)
qqline(Data$Feeder, col = "steelblue", lwd = 2)

qqnorm(Data$W0, pch = 1, frame = FALSE)
qqline(Data$W0, col = "darkgreen", lwd = 2)

qqnorm(Data$ADWG0021, pch = 1, frame = FALSE)
qqline(Data$ADWG0021, col = "green", lwd = 2)

qqnorm(Data$ADWG2150, pch = 1, frame = FALSE)
qqline(Data$ADWG2150, col = "lightblue", lwd = 2)

qqnorm(Data$ADWG0050, pch = 1, frame = FALSE)
qqline(Data$ADWG0050, col = "lightgreen", lwd = 2)


#shapiro test for checking normality 
# numeric data
shapiro.test(Data$Feeder)
shapiro.test(Data$W0) # P_value = 0.009664, so it is less than 0.05 (alpha), it is not normally distributed
shapiro.test(Data$ADWG0021)
shapiro.test(Data$ADWG2150)
shapiro.test(Data$ADWG0050)

#categorical data
#female
shapiro.test(Data[Data$Sex == "female",]$Feeder)
shapiro.test(Data[Data$Sex == "female",]$W0)
shapiro.test(Data[Data$Sex == "female",]$ADWG0021)
shapiro.test(Data[Data$Sex == "female",]$ADWG2150)
shapiro.test(Data[Data$Sex == "female",]$ADWG0050)

#male
shapiro.test(Data[Data$Sex == "male",]$Feeder)
shapiro.test(Data[Data$Sex == "male",]$W0)
shapiro.test(Data[Data$Sex == "male",]$ADWG0021)
shapiro.test(Data[Data$Sex == "male",]$ADWG2150)
shapiro.test(Data[Data$Sex == "male",]$ADWG0050)

#treatment A
shapiro.test(Data[Data$Treatment == "A",]$Feeder)
shapiro.test(Data[Data$Treatment == "A",]$W0)
shapiro.test(Data[Data$Treatment == "A",]$ADWG0021)
shapiro.test(Data[Data$Treatment == "A",]$ADWG2150)
shapiro.test(Data[Data$Treatment== "A",]$ADWG0050)

#treatment B
shapiro.test(Data[Data$Treatment == "B",]$Feeder)
shapiro.test(Data[Data$Treatment == "B",]$W0)
shapiro.test(Data[Data$Treatment == "B",]$ADWG0021)
shapiro.test(Data[Data$Treatment == "B",]$ADWG2150)
shapiro.test(Data[Data$Treatment== "B",]$ADWG0050)

#treatment C
shapiro.test(Data[Data$Treatment == "C",]$Feeder)
shapiro.test(Data[Data$Treatment == "C",]$W0)
shapiro.test(Data[Data$Treatment == "C",]$ADWG0021)
shapiro.test(Data[Data$Treatment == "C",]$ADWG2150)
shapiro.test(Data[Data$Treatment== "C",]$ADWG0050)

#treatment D
shapiro.test(Data[Data$Treatment == "D",]$Feeder)
shapiro.test(Data[Data$Treatment == "D",]$W0)
shapiro.test(Data[Data$Treatment == "D",]$ADWG0021)
shapiro.test(Data[Data$Treatment == "D",]$ADWG2150)
shapiro.test(Data[Data$Treatment== "D",]$ADWG0050)

#treatment E
shapiro.test(Data[Data$Treatment == "E",]$Feeder)
shapiro.test(Data[Data$Treatment == "E",]$W0)
shapiro.test(Data[Data$Treatment == "E",]$ADWG0021)
shapiro.test(Data[Data$Treatment == "E",]$ADWG2150)
shapiro.test(Data[Data$Treatment== "E",]$ADWG0050)

# checking Normality with Histogram
#numerical data

hist(Data$Feeder,xlab = "Feeder",main = "Histogram Distribution of Feeder",col = "lightblue")

hist(Data$W0,xlab = "W0",main = "Histogram Distribution of W0",col = "lightgreen")

hist(Data$ADWG0021,xlab = "ADWG0021",main = "Histogram Distribution of ADWG0021",col = "steelblue")

hist(Data$ADWG2150,xlab = "ADWG2150",main = "Histogram Distribution of ADWG2150",col = "blue")

hist(Data$ADWG0050,xlab = "ADWG0050",main = "Histogram Distribution of ADWG0050",col="darkblue")

library(ggplot2)
library(hrbrthemes)

#Sex
Data %>%
  ggplot( aes(x=W0,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=Feeder,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=ADWG0021,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

Data %>%
  ggplot( aes(x=ADWG2150,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

Data %>%
  ggplot( aes(x=ADWG0050,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
#Treatment
Data %>%
  ggplot( aes(x=Feeder,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=W0,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=ADWG0021,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=ADWG2150,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")
Data %>%
  ggplot( aes(x=ADWG0050,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")


## Check the homoscedasticity .
## leveneTest
library(car)
leveneTest(Feeder~Treatment, data=Data)
leveneTest(W0~Treatment, data=Data)
leveneTest(ADWG0021~Treatment, data=Data)
leveneTest(ADWG2150~Treatment, data=Data)
leveneTest(ADWG0050~Treatment, data=Data)

leveneTest(Feeder~Sex, data=Data)
leveneTest(W0~Sex, data=Data)
leveneTest(ADWG0021~Sex, data=Data)
leveneTest(ADWG2150~Sex, data=Data)
leveneTest(ADWG0050~Sex, data=Data)

## bartlett test
bartlett.test(Feeder ~ Treatment, data = Data)
bartlett.test(W0~Treatment, data=Data)
bartlett.test(ADWG0021~Treatment, data=Data)
bartlett.test(ADWG2150~Treatment, data=Data)
bartlett.test(ADWG0050~Treatment, data=Data)


bartlett.test(Feeder~Sex, data=Data)
bartlett.test(W0~Sex, data=Data)
bartlett.test(ADWG0021~Treatment, data=Data)
bartlett.test(ADWG2150~Treatment, data=Data)
bartlett.test(ADWG0050~Sex, data=Data)

#box plot
#Sex
Data %>% ggplot(aes(x=Sex, y=Feeder, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Sex, y=W0, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Sex, y=ADWG0021, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Sex, y=ADWG2150, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Sex, y=ADWG0050, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

#Treatment
Data %>% ggplot(aes(x=Treatment, y=Feeder, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Treatment, y=W0, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Treatment, y=ADWG0021, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Treatment, y=ADWG2150, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)
Data %>% ggplot(aes(x=Treatment, y=ADWG0050, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)


###################	5- Statistical Inference ################################
#	Calculate the 90%, 95%, 99% confidence interval for the means of ADWG0021per each gender.


means <- tapply(Data$ADWG0021,list(Sex=Data$Sex),mean,na.rm=T)
sd <- tapply(Data$ADWG0021,list(Sex=Data$Sex),sd,na.rm=T)
x = means
s = sd
n = 40

#90%
error <- qnorm(0.951)*s/sqrt(n)
lowerinterval <- x - error
upperinterval <- x + error

#95
error <- qnorm(0.975)*s/sqrt(n)
lowerinterval <- x - error
upperinterval <- x + error


#99
error <- qnorm(0.9995)*s/sqrt(n)
lowerinterval <- x - error
upperinterval <- x + error
lowerinterval
upperinterval


############################ 6- Hypothesis testing #########################################

install.packages("dunn.test")
install.packages("report")
install.packages("car")
install.packages("multcomp")
library(dunn.test)
library(report)
library(car)
library(multcomp)

# We hypothesis that ADWG0021is different between male vs female. Assuming normality and homoscedasticity, can you test this hypothesis using statistical hypothesis framework

#we have the statistical hypothesis testing framework after converting the research question into statistical question which is: is the mean different between male vs female in ADWG0021 ? ,we have null hypothesis which is the mean of groups male and female are equal and alternative hypothesis which is the mean of groups male and female is different then we compare the p value result to our significance level alpha 0.05 and check results, if results of p value smaller than 0.05 then we have enough evidence to reject the null hypothesis in support of alternative hypothesis and if the results greater than 0.05 then we don't have evidence to reject the null hypothesis,we firstly test normality and homogeneity of variance to know what test we are going to use in the , but here in this case normality and homoscedasticity were assumed so we will use standard two sample t test directly according to these assumptions
#t.test(ADWG0021~Sex, data=PWD, var.equal = TRUE)
t.test(ADWG0021~Sex, data=PWD,alternative = "two.sided", paired = FALSE ,var.equal = TRUE)
#here we added alternative equal two sided because the the data is two sided as it is different in general not greater or smaller than a specific value, also data is independent not dependent or paired so paired equal false, here also homo variance is assumed in the question so we make var.equal = TRUE
#the results of the standard two sample t test gives a p value equal to 0.7557 which is greater than the significance level alpha 0.05 which means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis which means that the mean of the 2 groups males and females is not different from each other, and also means that ADWG0021 is not different between male vs female. 

#now we will assess whether the previous test assumptions have been meet for the test
#to test normality we use QQ plots and shapiro welk test then we have null hypothesis which is the data is normally distributes and alternative hypothesis which is the data is not normally distributed then we compare the p value result to our significance level alpha 0.05 and check results 

#females
qqnorm(PWD[PWD$Sex == "female",]$ADWG0021, main='Females ADWG0021')
qqline(PWD[PWD$Sex == "female",]$ADWG0021)
hist(PWD[PWD$Sex == "female",]$ADWG0021, main='Females ADWG0021')
shapiro.test(PWD[PWD$Sex == "female",]$ADWG0021)
#firstly to test normality of data we used QQ plot and the line has no deviations from the data points so the data of females is normally distributed
#we also tested normality using shapiro welk test of normality and the results of the test gives a p value equal to 0.9513 which is greater than the significance level alpha 0.05 , this means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis, here the null hypothesis of shapiro test is that the data is normally distributed and the alternative is not normally distributed, so here the data of females is normally distributed

#males
qqnorm(PWD[PWD$Sex == "male",]$ADWG0021, main='males ADWG0021')
qqline(PWD[PWD$Sex == "male",]$ADWG0021)
hist(PWD[PWD$Sex == "male",]$ADWG0021, main='males ADWG0021')
shapiro.test(PWD[PWD$Sex == "male",]$ADWG0021)
#we did the same thing for the data of males, firstly we used QQ plot and the line has no deviations from the data points so the data of males is normally distributed
#we also used shapiro wilk test of normality and the results of the test gives a p value equal to 0.9771 which is greater than the significance level alpha 0.05, this means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis, here the null hypothesis of shapiro test is that the data is normally distributed and the alternative is not normally distributed, so the data of males is normal.

#we tested normality so now we will check homogeneity of variance of data, we will test using levene and F test to double check variace, here the null hypothesis of these tests is homo variance and alternative is hetero variance so we get the p value result and check if smaller than 0.05 so reject null so hetero and if greater than 0.05 so do not reject null so homo
boxplot(ADWG0021~Sex, data=PWD)
leveneTest(ADWG0021~Sex, data=PWD)#better to use levene because it does not assume normality and more robust
var.test(ADWG0021~Sex, data=PWD)

#after normality assessment we tested the homogeneity of variance of the groups males and females by levene test and the result of the test gives a p value equal to 0.3009 which is greater than the significance level alpha 0.05 this means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis so the data has homo variance or equal variance, so we will use standart two sample t test not wilsh t test for hetero vaiance as our data is homo not hetero, also the results of F test gives a p value of 0.4193 which is also greater than 0.05 so we do not have enough evidence to reject null so the data has homo variance
#so the answer is yes, the previous test assumptions have been meet for the test as we tested and the results are that the data is normal and homo in variance 


#	We hypothesis that ADWG0021is "different" in the group receiving Treatment A (normal feed + ZnO)  compared to the Treatment B (normal feed + nutraceuticals). Can you test this hypothesis assuming heteroscedasiticy 

#we have the statistical hypothesis testing framework after converting the research question into statistical question which is : Is the mean different between treatments A and B in ADWG0021, then we have the null hypothesis which is mean of treatment A equal to mean of treatment B and alternative hypothesis which is mean of treatment A different from mean of treatment B, then test normality to see what test we gonna use 
#we firstly test normality but here heteroscedasticity of variance is assumed so we gonna check only normality not variance to know what test we are going to use, then we have the null hypothesis which is mean of treatment A equal to mean of treatment B and alternative hypothesis which is mean of treatment A different from mean of treatment B, then test normality to see what test we gonna use 
#to test normality we use QQ plots and shapiro welk test then we have null hypothesis which is the data is normally distributes and alternative hypothesis which is the data is not normally distributed then we compare the p value result to our significance level alpha 0.05 and check results 

#treatment A
par(mfrow=c(1,2))
#QQPLOT for treatment A
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021')
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021')
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)

#Histogram for treatment A
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021')
#Histogram for treatment B
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021')

# shapiro test for treatment A
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)

# shapiro test for treatment B
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)

#we tested normality of treatment A data we used QQ plot firstly and the line has deviations from the data points so the data of treatment A is not normally distributed, the histogram plot also shows that the data of A is not normal
#we also used shapiro wilk test of normality of treatment A and the results of the test gives a p value equal to 0.0395 which is smaller than the significance level alpha 0.05 , this means that we have enough evidence to reject the null hypothesis in support of alternative hypothesis, here the null hypothesis of shapiro test is that the data is normally distributed and the alternative is not normally distributed, so this data of treatment A is not normally distributed
#we did the same thing to treatment B so we tested normality of treatment B data using QQ plot firstly and the line has no deviations from the data points so the data of treatment B is normally distributed, the histogram plot also shows that the data of A is normal
#we also used shapiro wilk test of normality of treatment B and the results of the test gives a p value equal to 0.8132 which is much greater than the significance level alpha 0.05 , this means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis, here the null hypothesis of shapiro test is that the data is normally distributed and the alternative is not normally distributed, so this data of treatment B is normally distributed
#in our case we have data A not normal and data B normal but when we have at least one of our data not normal so we assume that the data is not normally distributed so here we gonna use man whitny test (welcox test) instead of standart two sample t test assuming non normality of data (non parametric)

wilcox.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021, var.equal = FALSE,alternative = "two.sided", paired = FALSE)

#here we have two sided data and independent not dependent or paired so paired equal false, here also hetero variance is assumed in the question so we make var.equal = false
#the results of wilcoxon test gives a p value equal to 0.0312 which is smaller than the significance level alpha 0.05 that means that we have enough evidence to reject the null hypothesis in support of alternative hypothesis , this means that ADWG0021 is "different" in the group receiving Treatment A (normal feed + ZnO)  compared to the Treatment B (normal feed + nutraceuticals). 

# Assess the previous test assumption
#now we will test hetero variance of data and see the results
var.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021)

#all the results of F test of variance gives a p value of 0.9567 for groups A and B which is much greater than the significance level alpha 0.05, this means that we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis which means that the variance of these groups is homo variance or equal variance
#this means that the variance is homo in this data not hetero so the answer is No, this do not go with the previous test assumptions
#now we tested variance and the results was homo and we tested normality before this and the results not normal so we gonna use welcox test but make var.equa = FALSE
wilcox.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021, var.equal = TRUE,alternative = "two.sided", paired = FALSE)
#when we changed the variance homo in code it gives us the same results as we tested before by un equal variance (p value = 0.0312 so reject null so groups are different)

#last point
# We hypothesis that ADWG0021is different between the different Treatments . Can you perform comparison between the different groups, after assessing the assumptions and performing post-hoc testing (assuming normality and homoscedasticity).

# here we have in our data the groups of treatments are categorical and the ADWG0021 is continuous so we gonna use anova test
#we have the statistical hypothesis testing framework after converting the research question into statistical question which is: is the mean different between the different Treatments in ADWG0021 ? ,we have null hypothesis which is the mean of the treatment groups are equal and alternative hypothesis which is the mean of treatment groups male and female is different then we compare the p value result to our significance level alpha 0.05 and check results, if results of p value smaller than 0.05 then we have enough evidence to reject the null hypothesis in support of alternative hypothesis and if the results greater than 0.05 then we don't have evidence to reject the null hypothesis,we firstly test normality and homogeneity of variance to know what test we are going to use
#now test normality and homo variance to check assumptions are true or not 

#QQPLOT for treatment A
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021')
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021')
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "C",]$ADWG0021, main='C ADWG0021')
qqline(PWD[PWD$Treatment == "C",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "D",]$ADWG0021, main='D ADWG0021')
qqline(PWD[PWD$Treatment == "D",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "E",]$ADWG0021, main='E ADWG0021')
qqline(PWD[PWD$Treatment == "E",]$ADWG0021)

#QQ plot results show that data of treatment A is not normally distributed as the line but B,C,Dand E are normally distributed

#check normality within each group with histogram
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment A')
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment B')
hist(PWD[PWD$Treatment == "C",]$ADWG0021, main='Treatment C')
hist(PWD[PWD$Treatment == "D",]$ADWG0021, main='Treatment D')
hist(PWD[PWD$Treatment == "E",]$ADWG0021, main='Treatment E')

#histogram results show that data of treatment A is not normally distibuted but B,C,Dand E are normally distributed

# shapiro test for treatment A
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021) #p value = 0.0395 #less than 0.05 #reject null #not normal

# shapiro test for treatment B
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021) #p value = 0.8312 #more than 0.05 #don't reject null #normal

# shapiro test for treatment C
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021) #p value = 0.6954 #more than 0.05 #don't reject null #normal

# shapiro test for treatment D
shapiro.test(PWD[PWD$Treatment == "D",]$ADWG0021) #p value = 0.7126 #more than 0.05 #don't reject null #normal

# shapiro test for treatment E
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021) #p value = 0.86 #more than 0.05 #don't reject null #normal

#here we have at least one of the data is not normal (data of treatment A) so we gonna use kruskal walis test ( non-parametric )
kruskal.test(ADWG0021 ~Treatment , data=PWD) # the results gives a p value equal to 0.09972 which is greater than 0.05 so we do not have evidence to reject the null so ADGW0021 is not different between gropus of treatments. 
#check variance to see wether it met the assumptions or not 
plot(ADWG0021~Treatment, data=PWD, main="Variance")
leveneTest(ADWG0021~Treatment, data=PWD) #0.8968
#the results of levene test of homogeneity of variance  gives a p value 0.8968 which is greater than 0.05 so we do not have enough evidenece to reject the null so the data has homo variance 
#oneway.test(ADWG0021~Treatment, data= PWD)#One-way analysis of means (not assuming equal variances) #0.2022

#assuming normality and homoscedasticity we gonna use standard anova test 
par(mfrow=c(1,1))
AnovaModel<- aov(ADWG0021~Treatment, var.equal = TRUE,alternative = "two.sided",data= PWD)
summary(AnovaModel) 
coef(AnovaModel)
report(AnovaModel) #The main effect of Treatment is statistically not significant and large (F(4, 35) = 2.10, p = 0.101; Eta2 = 0.19, 95% CI [0.00, 1.00])
#the results of anova test gives a p value equal to 0.101 which is much greater than the significance level alpha 0.05 so we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis so ADWG0021 is not different between the different Treatments 
#posthoc using tukey test
#posthoc is performed after annova assuming normality and homoscedasticity (includes p value correction to be p adjusted) 
testing <- TukeyHSD(AnovaModel)
testing 
#the reults of post hoc shows the differene of means betwwen treatments with each others(B-A,C-A,D-A,E=A,C-B,D-B,E-B,D-C,E-C,E-D), all the treatments with each others have a confidence interval values indicate that the zero value lies in the interval so this indicates the null hypothesis will not be rejected and the p adjusted value will be too large and if the zero value is not in the interval this will indicate lower p value so we will have evidence to reject the null hypothesis (the p adjust is the correction of p value performed by posthoc after anova)
#after anova we perform posthoc(tuky test) , the reults of posthoc of p adjusted value show that all treatments with each other gives a p adjusted value greater than the significance level alpha which means that we do not have enough evidence to reject the null so this means that ADWG0021 is not different 
plot(testing)
summary(glht(AnovaModel, linfct =mcp(Treatment = "Tukey")))
#the plot of testing posthoc shows that the zero value lies in the middle of confidence interval which reults in not rejecting the null and the higher p value
plot(glht(AnovaModel, linfct =mcp(Treatment = "Tukey")))# this plot differ from the previous plot in showing the upper and lower of confidence interval (start and end)
summary(glht(AnovaModel, linfct =mcp(Treatment = "Dunnett"))) #after adjusting p values,treatment B-A gives a p value significant equal to 0.035 which is less than 0.05 so we have enough evidence to reject the null so the mix of treatment B with A is different in ADWG0021 
plot(glht(AnovaModel, linfct =mcp(Treatment = "Dunnett"))) 
#glht and mcp function is done here by dunnet which is same as tuky but here the reults gives the linear hypothesis between each pair of treatments and how it can affect the model as the 2 treatments with eachb other indicate the non normality of data(they are the reason that the data is not normal), the results shows that the treatments B-A gives the higher p value 0.035 and one star which means that the result is significant as it is lower than alpha 0.05 so we have enough evidence to reject the null hypothesis so not normal data is due to these 2 treatments with each other(B-A)
pairwise.t.test(PWD$ADWG0021, PWD$Treatment , p.adjust.method = "bonferroni")#pairwise t test gives adjust p value with benferroni method but also results were not signficant 
#tukey and benferoni assume normality
dunn.test(PWD$ADWG0021, PWD$Treatment, method ="Bonferroni")
#the results of tukey test with adjusted p values with benferoni gives a p adjusted value 0.0437 between treatment A and B (A mixed with B) which is lower than 0.05 so we have evidence to reject null so this leads to non normality of data(also then ADWG0021 is different in A with B after adjusting p value with benferoni)  , while the p value of other treatments with each other is greater than 0.05 so we do not have evidence to reject the null so this leads to normality of other treatments(also ADWG0021 is not different between other treatment with each others(except A with B)) , also the mean of each pair of treatment is shown in the results. 


############################# 7- linear regression ##################################


#Fit a linear regression to the data and interpret the regression coefficient (for the one of the hypotheses mentioned above)

#here linear regression is continuous(continuous) with categorical (sex)
#firstly we plot the data then we generate our regression model to fit the regression line, then we have the null hypothesis that slope equal zero (y can not be predicted by X )and alternative hypothesis slope differ from zero (y can be predicted by x) so we test and check results
PWD$Sex<-as.integer(PWD$Sex)
plot(PWD$Sex, PWD$ADWG0021)
regression <- lm(ADWG0021 ~ Sex , data= PWD)
regression #coefficients a and b (intercept = 144.07 and slope = -1.92), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
abline(regression, col="red")
#it is obvious that there is no linear relation ship between the explanatory variable sex with the response variable ADWG0021 as the sex is categorical (all males equal to one and females equal to 2) and ADVG0021 is continuous so the regression line is horizontal seems to be zero so it means that y(ADVG0021) can't be predicted by x (sex) as there is no linearity between them
#the results of regression models shows that the residuals result is too bad which is 1.23 so the error is too large, also the p value is equal to 0.756 which is much greater than the significance level 0.05 so we do not have enough evidence to reject the null so the slope is equal to zero so y can't be predicted by x (there is no linear relationship between the response variable and the explanatory variable), also the residual standard error(mean square error) equal 19.37 which is too large which is not good and f value is too small 0.09824 which results in the large p value and finally the adjusted R squares is too small -0.02367 which is a not good at all(it explains -0.02 from the variability of y)
#linear regression is between continuous and continuous but here all used in hypothesis were continuous and categorical

#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'Sex', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-14.31805) and 97.5% (10.47877) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex


#now we will perform linear regression but 2 continuous variables as it is supposed to be done like that 
#so we will perform ADGW2150(y axis response variable) with ADWG0021(x axis explanatory variable) 
plot(PWD$ADWG2150, PWD$ADWG0021)
regression <- lm(ADWG0021 ~ ADWG2150 , data= PWD)
regression #coefficients a and b (intercept = 103.25181 and slope = 0.07958), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
#the results of linear regression between the 2 continuous variables ADWG0021(y) and ADWG2150(x) gives a bad model as the residuals median is too large(2.849) and the p values is equal to 0.158882 which is much greater than 0.05 so we do not have enough evidence to reject the null hypothesis so y(ADWG0021) can't be predicted by x(ADWG2150) as there is no linearity between them also the regression line when fitted shows that it is horizontally so that means that the slope is close to zero (null hypothesis), also here the adjusted R squared is more accurate than the R squared and the adjusted gives a very small value which is 0.02659(2.5%) which means that the regression model explains(capture) only 2.5% of the total variation of y(ADGW0021) which is not good at all, so there is no linearity
abline(regression, col="red")
#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'ADWG2150', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-0.03252242) and 97.5% (0.191674) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex


##now we will perform linear regression the same 2 continuous variables but exchange x with y
#so we will perform ADGW2150(x axis response variable) with ADWG0021(y axis explanatory variable) 
plot(PWD$ADWG0021, PWD$ADWG2150)
regression <- lm(ADWG2150 ~ ADWG0021 , data= PWD)
regression #coefficients a and b (intercept = 408.2149 and slope = 0.6477), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
#the results of linear regression between the 2 continuous variables ADWG0021(x) and ADWG2150(y) gives also a bad model as the residuals median is too large(4.802) and the p values is equal to 0.159 which is much greater than 0.05 so we do not have enough evidence to reject the null hypothesis so y(ADWG2150) can't be predicted by x(ADWG0021) as there is no linearity between them also the regression line when fitted shows that it is horizontally so that means that the slope is close to zero (null hypothesis), so that means that the slope is close to zero (null hypothesis), also here the adjusted R squared is more accurate than the R squared and the adjusted gives a very small value which is 0.02659(2.5%) which means that the regression model explains(capture) only 2.5% of the total variation of y(ADGW2150) which is not good at all, so there is no linearity.
abline(regression, col="red")
#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'ADWG0021', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-0.2647333) and 97.5% (1.560231) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex

# Estimating the average ADWG0021 change for with changing the gender from 1 to 2 (bonus).
#now we will perform multiple linear regression with the 2 continuous variables ADGW0021 and ADGW2150 with the existence of Sex to see wether sex affect or not

multiple_regression <- lm(ADWG0021~ ADWG2150+Sex, data = PWD)# performing model so we plot the response variable (ADWG0021) against the the 2 explainatory variables (sex+ADWG2150)
multiple_regression #coefficients (intercept = 102.97717 and slope of ADWG2150= 0.07981 , slope of Sex = 0.10592)
summary(multiple_regression)
avPlots(multiple_regression)#produce added variable plots
#the results of multiple linear regression between the 3 variables ADWG0021(y) and ADWG2150+Sex gives also a bad model as the residuals median is too large(2.854) and the p value of ADWG2150 is equal to 0.17519 which is much greater than 0.05 so we do not have enough evidence to reject the null hypothesis so y(ADWG0021) can't be predicted by x(ADWG2150) in the existence of Sex as there is no linearity between them also p value of sex equal0.98652 which is also greater than 0.05 so do not reject the null so sex is not significant and has no effect on the model, also the regression line when fitted shows that it is horizontally so that means that the slope is close to zero (null hypothesis)
#also the also here the adjusted R squared is more accurate than the R squared and the adjusted gives a very small value which is 0.0002851(0.02%) which means that the regression model explains(capture) only 0.02% of the total variation of y(ADGW2150) which is not good at all, so there is no linearity