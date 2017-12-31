#####################################
#      R CODE FOR ASSIGNMENT 2      #
#   -- Multivariate Statistics --   #
#                                   #
# OCTOBER 5, 2015                   #
# GROUP 23                          #
#####################################

#0. Load the Data
MIPData<-read.csv("Assignment2_Data.csv")
MIPData
library(car)
library(lmtest)

#First check for missing data
MissData <- apply(is.na(MIPData),2,sum)
MissData1 <- MissData[MissData>0]
MissData1    # NO DATA MISSING, we can continue

#1. What is the proportion of enterprises that co-operate on innovation activities with other enterprises or institutions?
print(mean(MIPData$CoopRnD),digits = 4)

#2.Proving the model first...

#Create the standardized values for innovation
MIPData$StdInnovEnvironBenefits<-(MIPData$InnovEnvironBenefits- mean(MIPData$InnovEnvironBenefits))/ sd(MIPData$InnovEnvironBenefits)
MIPData$StdInnovEnvironBenefits
MIPData$lnInnovEnvironBenefits<-log(MIPData$InnovEnvironBenefits+1)

#Create the three models
names(MIPData)
MIPData$lnSize<-log(MIPData$Size+1)
MIPData$lnSize
logit1<-glm(CoopRnD~Size+InnovEnvironBenefits,family = binomial(link=logit), data = MIPData)
logit2<-glm(CoopRnD~lnSize+InnovEnvironBenefits,family = binomial(link=logit), data = MIPData)
logit3<-glm(CoopRnD~lnSize+lnInnovEnvironBenefits,family = binomial(link=logit), data = MIPData)

# Look if transformations are necessary
avPlots(logit1)
avPlots(logit2)
avPlots(logit3)

#Check for multicollinearity
vif(logit1)
vif(logit2)
vif(logit3)

#3 Fit the model
#logit1<-glm(CoopRnD~lnSize+StdInnovEnvironBenefits,family = binomial(link=logit), data = MIPData)
summary(logit1)
summary(logit2)
summary(logit3)
# Both variables has a p-value less than 0.05 so Betas are significant in model


# No problem with multicollinearity

#6 HIT and True + and True -
#Calculating HIT rate
Logit1Prediction <- logit1$fitted.values >= 0.5
mean(Logit1Prediction==MIPData$CoopRnD)
Logit2Prediction <- logit2$fitted.values >= 0.5
mean(Logit2Prediction==MIPData$CoopRnD)
Logit3Prediction <- logit3$fitted.values >= 0.5
mean(Logit3Prediction==MIPData$CoopRnD)

#Calculate True +
mean(Logit1Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit2Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit3Prediction[which(MIPData$CoopRnD==1)]==1)

#Calculate True -
mean(Logit1Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit2Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit3Prediction[which(MIPData$CoopRnD==0)]==0)

#7. Add a variable to the model
names(MIPData)

logit11<-glm(CoopRnD~Size+InnovEnvironBenefits+Turnover,family = binomial(link=logit), data = MIPData)
logit12<-glm(CoopRnD~Size+InnovEnvironBenefits+InhouseRnD,family = binomial(link=logit), data = MIPData)
logit13<-glm(CoopRnD~Size+InnovEnvironBenefits+ExternalRnD,family = binomial(link=logit), data = MIPData)
logit14<-glm(CoopRnD~Size+InnovEnvironBenefits+Machinery,family = binomial(link=logit), data = MIPData)
logit15<-glm(CoopRnD~Size+InnovEnvironBenefits+ExternalKnow ,family = binomial(link=logit), data = MIPData)
logit16<-glm(CoopRnD~Size+InnovEnvironBenefits+MarketSpend,family = binomial(link=logit), data = MIPData)
logit17<-glm(CoopRnD~Size+InnovEnvironBenefits+ProcessInnov,family = binomial(link=logit), data = MIPData)

summary(logit11)
summary(logit12)
summary(logit13)
summary(logit14)
summary(logit15)
summary(logit16)
summary(logit17)


vif(logit11)
vif(logit12)
vif(logit13)
vif(logit14)
vif(logit15)
vif(logit16)
vif(logit17)

avPlots(logit13)

#HIT Rate
Logit11Prediction <- logit11$fitted.values >= 0.5
mean(Logit11Prediction==MIPData$CoopRnD)
Logit12Prediction <- logit12$fitted.values >= 0.5
mean(Logit12Prediction==MIPData$CoopRnD)
Logit13Prediction <- logit13$fitted.values >= 0.5
mean(Logit13Prediction==MIPData$CoopRnD)
Logit14Prediction <- logit14$fitted.values >= 0.5
mean(Logit14Prediction==MIPData$CoopRnD)
Logit15Prediction <- logit15$fitted.values >= 0.5
mean(Logit15Prediction==MIPData$CoopRnD)
Logit16Prediction <- logit16$fitted.values >= 0.5
mean(Logit16Prediction==MIPData$CoopRnD)
Logit17Prediction <- logit17$fitted.values >= 0.5
mean(Logit17Prediction==MIPData$CoopRnD)

#True positive
mean(Logit11Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit12Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit13Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit14Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit15Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit16Prediction[which(MIPData$CoopRnD==1)]==1)
mean(Logit17Prediction[which(MIPData$CoopRnD==1)]==1)

#True negative
mean(Logit11Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit12Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit13Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit14Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit15Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit16Prediction[which(MIPData$CoopRnD==0)]==0)
mean(Logit17Prediction[which(MIPData$CoopRnD==0)]==0)

#8. Fit the model
MIPData$lnExternalRnD<-log(MIPData$ExternalRnD+1)

#Since the ExternalRnD has a maximum value of 0.880 millions of USD, the interpretation of the effect of each additional million of EUR is not ok; make analysis in euros; p-values and AIC are not affected
MIPData$ExternalRnDEuro<-MIPData$ExternalRnD*1000
logit132<-glm(CoopRnD~Size+InnovEnvironBenefits+ExternalRnDEuro,family = binomial(link=logit), data = MIPData)
summary(logit132)

#9. Compare the AIC of the model in question 3 to the AIC of the model in question 8
# What do you conclude?

#aic of model 1 - 640.18
#aic of model 2 - 574.16

avPlots(Reg6)

names(Reg6)
Reg6$coefficients

vif(Reg6)

#Based on your results from question 8, interpret the regression coeffcient of com-
#pany size. Does your answer differ from your answer to question 4? If so, why is
#the interpretation different?

Reg6$coefficients
Reg3$coefficients


#11. Based on your results from question 8, interpret the regression coeffcient of the
#factor scores of the factor analysis on \innovations with environmental benefits".
#Does your answer differ from your answer to question 4? If so, why is the inter-
#pretation different?

# regression coefficient of InnovEnvironBenefit is statistically insignificant

#12. Report and interpret the hit-rate, the true-positive rate and the true-negative rate.
#Compare these numbers to your answer to question 6. What do you conclude?

#model 1

Reg3<- glm(formula = MIPData$CoopRnD ~ MIPData$Size + MIPData$InnovEnvironBenefits, family = binomial(link= "logit" ))
summary(Reg3)
vif(Reg3)
avPlots(Reg3)
#compute hit rate
Reg3prediction <- Reg3$fitted.values>=0.5
mean(Reg3prediction==MIPData$CoopRnD)
#compute true positive rate
mean(Reg3prediction[which(MIPData$CoopRnD==1)]==1)
#compute true negative rate
mean(Reg3prediction[which(MIPData$CoopRnD==0)]==0)


#model 2

Reg6<- glm(formula = MIPData$CoopRnD ~ MIPData$Size + MIPData$InnovEnvironBenefits + MIPData$ExternalRnD, family = binomial(link="logit"), data = MIPData)
summary(Reg6)
Reg6prediction <- Reg6$fitted.values>=0.5
#compute hit rate
mean(Reg6prediction==MIPData$CoopRnD)
#compute true positive rate
mean(Reg6prediction[which(MIPData$CoopRnD==1)]==1)
#compute true negative rate
mean(Reg6prediction[which(MIPData$CoopRnD==0)]==0)

# accuracy of hit rate has gone up with inclusion of External RnD, esp. in true positives

#13. Write down the model specifcation that allows you to answer the 
# above two questions.

MIPData$lnTurnover <- log(MIPData$Turnover+1)
MIPData$lnInRnD <- log(MIPData$InhouseRnD+1)
MIPData$lnExtRnd <- log(MIPData$ExternalRnD+1)
MIPData$lnMach <- log(MIPData$Machinery+1)
MIPData$lnExtknow <- log(MIPData$ExternalKnow+1)

# First a model is chosen without any alterations

lm<- lm(Turnover ~Size+CoopRnD+InhouseRnD+ExternalRnD+Machinery+ExternalKnow, data=MIPData)
summary(lm)

# A new model with alterations

lm1<- lm(lnTurnover~lnSize+CoopRnD+lnInRnD+lnExtRnd+lnMach+lnExtknow,data=MIPData)
summary(lm1)

vif(lm1)
avPlots(lm1)
plot(y=lm1$residuals,x=lm1$fitted.values)
qqnorm(lm1$residuals)
qqline(lm1$residuals)

#14. Estimate the model you suggested by ordinary least squares and report the results.
vif(lm1)
avPlots(lm1)

#15. Interpret the R2 value.
summary(lm1)

#16. Do you need to apply a correction to the standard errors? Why or why not?
#Support your answer by a plot and statistical test. Explain why you use that
#specific plot and test.
names(lm1)
plot(y=lm1$residuals,x=lm1$fitted.values)
bptest(lm1)
abline(lm1)

#17. If you have to apply a correction to the standard errors, apply the correction and
#report the new results. What has changed?
hccm(lm1)
coeftest(lm1, vcov=hccm(lm1))

qqnorm(lm1$residuals)
qqline(lm1$residuals)

#18. Interpret the regression parameters.
summary(lm1)
coeftest(lm1, vcov=hccm(lm1))

#19. Which changes do you suggest to the turnover model to answer questions Q1 and
#Q2? Write down the model equation.
names(MIPData)
MIPData$lnMarkSpend = log(1+MIPData$MarketSpend)

lmtrial <- lm(lnTurnover~lnSize+CoopRnD+lnInRnD+lnExtRnd+lnMach+InnovEnvironBenefits,data = MIPData)
summary(lmtrial)
lmtrial <- lm(lnTurnover~lnSize+CoopRnD+lnInRnD+lnExtRnd+lnMach+lnMarkSpend,data = MIPData)
summary(lmtrial)
lmtrial <- lm(lnTurnover~lnSize+CoopRnD+lnInRnD+lnExtRnd+lnMach+ProcessInnov,data = MIPData)
summary(lmtrial)

lm3 <- lm(lnTurnover~lnSize+CoopRnD+lnInRnD+lnExtRnd+lnMach+lnMarkSpend,data = MIPData)
summary(lm3)
vif(lm3)
avPlots(lm3)
names(lm3)
plot(y=lm3$residuals,x=lm3$fitted.values)
bptest(lm3)
abline(lm3)

hccm(lm3)
coeftest(lm3, vcov=hccm(lm3))

qqnorm(lm3$residuals)
qqline(lm3$residuals)

#20. Estimate and report the results

summary(lm3)
plot(y=lm3$residuals,x=lm3$fitted.values)
bptest(lm3)
abline(lm3)
hccm(lm3)
coeftest(lm3, vcov=hccm(lm3))

#21. Do you have to worry about multicollinearity?

vif(lm3)