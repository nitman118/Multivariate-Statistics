#####################################
#      R CODE FOR ASSIGNMENT 1      #
#   -- Multivariate Statistics --   #
#                                   #
# SEPTEMBER 25, 2017                #
# GROUP 23                          #
#####################################

MIP <- read.csv("Assignment1_Data.csv")
library(psych)

##General Inspection of the Data Set##
summary(MIP)
head(MIP)
dim(MIP)

# 1. proportion of enterprises that launch innovations that are new to the 
# market across the four sectors


print(aggregate(MIP$NewToMarket ~ MIP$Sector, FUN = mean), digits=4)

# 2. Are new to the market innovations more common in industry than in services?

KIndustryNTM <- mean(MIP$NewToMarket[MIP$Sector=="KIndustry"])
OIndustryNTM <- mean(MIP$NewToMarket[MIP$Sector=="OIndustry"])
KServicesNTM <- mean(MIP$NewToMarket[MIP$Sector=="KServices"])
OServicesNTM <- mean(MIP$NewToMarket[MIP$Sector=="OServices"])

IndustryNTM <- mean(MIP$NewToMarket[MIP$Sector=="KIndustry" | MIP$Sector=="OIndustry"])
ServicesNTM <- mean(MIP$NewToMarket[MIP$Sector=="KServices" | MIP$Sector=="OServices"])

print(IndustryNTM, digits=4)
print(ServicesNTM, digits =4)  

# 4. Do firms that launch innovations that are new to the market rate themselves higher
# in terms of their margin?

boxplot(MIP$Margin ~ MIP$NewToMarket, xlab="New(1) or Not New(0) To Market", ylab="Margin", main="Margin differences across new or ot new to market companies") 

##Performing two sample, two sided t-test for unequal sample sizes 
##H_0: There is statistically no difference in the average turnover if a company has made a process innovation or not.
##H_a:There is a statistical difference in the average turnover if a company has made a process innovation.

t.test(MIP$Margin~MIP$NewToMarket, alternative="two.sided", mu=0)

# 5. Does any of the variables contain missing values? If so, which variable(s)?

MissData <- apply(is.na(MIP),2,sum)
MissData1 <- MissData[MissData>0]
MissData1

# 6. What is the percentage of missingness?

MP <- MissData1/dim(MIP)[1]
print(MP, digits=4)

#7. Are any of the competence variables related to the missingness?
missingdata<-MIP[!complete.cases(MIP),]
completecasesdata<-MIP[complete.cases(MIP),]

turnov<-completecasesdata$InnovTurnoverP
marg<-completecasesdata$Margin
ntm<-completecasesdata$NewToMarket
competence1<-completecasesdata$Competence1
competence2<-completecasesdata$Competence2
competence3<-completecasesdata$Competence3
competence4<-completecasesdata$Competence4
competence5<-completecasesdata$Competence5
competence6<-completecasesdata$Competence6
competence7<-completecasesdata$Competence7
competence8<-completecasesdata$Competence8
competence9<-completecasesdata$Competence9
competence10<-completecasesdata$Competence10

naturnov<-missingdata$InnovTurnoverP
namarg<-missingdata$Margin
nantm<-missingdata$NewToMarket
nacompetence1<-missingdata$Competence1
nacompetence2<-missingdata$Competence2
nacompetence3<-missingdata$Competence3
nacompetence4<-missingdata$Competence4
nacompetence5<-missingdata$Competence5
nacompetence6<-missingdata$Competence6
nacompetence7<-missingdata$Competence7
nacompetence8<-missingdata$Competence8
nacompetence9<-missingdata$Competence9
nacompetence10<-missingdata$Competence10


t.test(turnov,naturnov) #No difference Missing Completely at random MCAR
t.test(marg,namarg) #Difference    MAR
t.test(ntm,nantm) #No difference Missing Completely at random MCAR

t.test(competence1,nacompetence1) #No difference Missing Completely at random MCAR
t.test(competence2,nacompetence2)#No difference Missing Completely at random MCAR
t.test(competence3,nacompetence3)#Difference    MAR, Scope for development via 'trial and error'
t.test(competence4,nacompetence4)#Difference    MAR, Strong individual responsibility of employees
t.test(competence5,nacompetence5)#No difference Missing Completely at random MCAR
t.test(competence6,nacompetence6)#Difference    MAR, Incentive schemes for employees to innovate
t.test(competence7,nacompetence7)#Difference    MAR, Internal co-operation between departments / firm units
t.test(competence8,nacompetence8)#No difference Missing Completely at random MCAR
t.test(competence9,nacompetence9)#No difference Missing Completely at random MCAR
t.test(competence10,nacompetence10)#No difference Missing Completely at random MCAR

# No, it cannot be ignored since there are 4 variables that are MAR an not MCAR
# Model missingness is necesary

# 10. Compute the Mahalanobis distance of the enterprises based on the competence
# variables. Graphically present the Mahalanobis distances.

mu<- colMeans(MIP[,c(6:15)])
S<-cov(MIP[,c(6:15)])
MD <- mahalanobis(x=MIP[,c(6:15)],center = mu, cov = S)
boxplot(MD,ylab="Mahalanobis distnace",  main= "Mahalanobis distance for all innovation enterprises")

##Investigating the minimum and maximum distances
max(MD)
which.max(MD)
min(MD)
which.min(MD)

# 11. Can we use the rule-of-thumb that observations of which the Mahalanobis Distance
# divided q is larger than 3 are outliers?

##For Competence 1
qqnorm(MIP$Competence1, main = "Normal Q-Q Plot for Competence 1")
qqline(MIP$Competence1)
hist(MIP$Competence1, main = "Histogram Graph for Competence 1", xlab="Competence 1")
shapiro.test(MIP$Competence1)

##For Competence 2
qqnorm(MIP$Competence2)
qqline(MIP$Competence2)
hist(MIP$Competence2)
shapiro.test(MIP$Competence2)

##For Competence 3
qqnorm(MIP$Competence3)
qqline(MIP$Competence3)
hist(MIP$Competence3)
shapiro.test(MIP$Competence3)

##For Competence 4
qqnorm(MIP$Competence4)
qqline(MIP$Competence4)
hist(MIP$Competence4)
shapiro.test(MIP$Competence4)

##For Competence 5
qqnorm(MIP$Competence5)
qqline(MIP$Competence5)
hist(MIP$Competence5)
shapiro.test(MIP$Competence5)

##For Competence 6
qqnorm(MIP$Competence6)
qqline(MIP$Competence6)
hist(MIP$Competence6)
shapiro.test(MIP$Competence6)

##For Competence 7
qqnorm(MIP$Competence7)
qqline(MIP$Competence7)
hist(MIP$Competence7)
shapiro.test(MIP$Competence7)

##For Competence 8
qqnorm(MIP$Competence8)
qqline(MIP$Competence8)
hist(MIP$Competence8)
shapiro.test(MIP$Competence8)

##For Competence 9
qqnorm(MIP$Competence9)
qqline(MIP$Competence9)
hist(MIP$Competence9)
shapiro.test(MIP$Competence9)

##For Competence 10
qqnorm(MIP$Competence10)
qqline(MIP$Competence10)
hist(MIP$Competence10)
shapiro.test(MIP$Competence10)

# 12. Identify the index of the outliers

MD/10
max(MD/10)
bigmd<-which(MD/10>4)
bigmd

# 13. Is a factor analysis appropriate for the questions on obstacles to innovations?
cor(MIP[,6:15])
#for two digits
round(cor(MIP[,6:15]),digits=2)

# 14. Make a scree plot
EigenDecomp <- eigen(cov(MIP[,c(6:15)]))
EigenValues <- EigenDecomp$values
plot(EigenValues, main="Scree Plot",type="l",xlab="Number of Factors",ylab="Eigen Values")

# 15. Make a rotation

##without rotation##
EFA.kfactors <- factanal(MIP[,6:15], factors=2, rotation="none")
print(EFA.kfactors, digits=2, cutoff=.2)

##with varimax rotation##
EFA.kfactors <- factanal(MIP[,6:15], factors=2, rotation="varimax")
print(EFA.kfactors, digits=2, cutoff=.2)

# 17. 

##Re-estimate by dropping variable Competence 8
# New Scree Plot
EigenDecomp <- eigen(cov(MIP[,c(6:12,14:15)]))
EigenValues <- EigenDecomp$values
plot(EigenValues,main="Scree Plot after dropping competence8",type="l",xlab="Number of Factors",ylab="Eigen Values")
# New Estimation
k <- 2  
EFA.kfactors <- factanal(MIP[,c(6:12,14:15)], factors=k, rotation="varimax")
print(EFA.kfactors, digits=2, cutoff=.2)

#Look for convergence
EFA.kfactors$converged    #If this is TRUE, we should continue with analysis, if not, we have to make another kind of analysis
EFA.kfactors

NrFactors<- 1:5
pvals <- rep(NA,times=length(NrFactors))

for(k in NrFactors){
  #Estimate k-factor models
  Estimation<- factanal(MIP[,c(6,7,8,9,10,11,12,14,15)], factors = k, rotation="varimax")
  pvals[k] <- Estimation$PVAL
}

cbind(NrFactors,pvals)


# 19. Factor reliability on both factors

##Compute the Cronbach's alpha for the Financial MAtters factor
CalphaEmployees <- alpha(MIP[8:12])
CalphaEmployees

##Compute the Cronbach's alpha for the Human Resources factor
CalphaDevelopment <- alpha(MIP[,c(6,7,14,15)])
CalphaDevelopment

# 20. Perform a validity analysis of your factor model by splitting
# the sample in two random partitions and comparing the factor loadings
N <- dim(MIP)[1]
shuffle.index <- sample(1:N,size=N)
MIPshuffle <- MIP[shuffle.index,]

MIP.Partition1 <- MIPshuffle[1:round(N/2),]
MIP.Partition2 <- MIPshuffle[(round(N/2)+1):N,]

##Estimate the factor model we have identified before to the first data partition.
EFA.2factors.P1 <- factanal(MIP.Partition1[,c(6:12,14:15)], factors = 2, rotation = "varimax")
EFA.2factors.P1$converged
print(EFA.2factors.P1, digits=2, cutoff=.2)

##Estimate the factor model we have identified before to the SECOND data partition.
EFA.2factors.P2 <- factanal(MIP.Partition2[,c(6:12,14:15)], factors = 2, rotation = "varimax")
EFA.2factors.P2$converged
print(EFA.2factors.P2, digits=2, cutoff=.2)

#21. Summarize and report the results of the factor analysis.

EFA.2factors<-factanal(MIP[,c(6,7,8,9,10,11,12,14,15)], factors = 2, rotation = "varimax", scores="regression")
FactorScores<- data.frame(EFA.2factors$scores)
FactorScores

Factor1.score<-FactorScores$Factor1

Factor2.score<-FactorScores$Factor2

plot(Factor1.score,Factor2.score, xlab="Employee engagement", ylab="Responsiveness to innovations")
cor(Factor1.score,Factor2.score)

# 23. Do enterprises that launch new to the market innovations score better or 
# worse than enterprises that have not in terms of the factors that you have 
# identified? Support your answer by a plot and by a t-test 

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$NewToMarket, xlab="NewToMarket", main="Employee Engagement vs New to Market") 
boxplot(MIP$Development ~ MIP$NewToMarket, xlab="NewToMarket", main="Responsiveness to Innovation vs New to Market") 

t.test(MIP$Employees ~ MIP$NewToMarket)
t.test(MIP$Development ~ MIP$NewToMarket)

# 24.Do enterprises that have a strictly positive margin (i.e. they have indicated in the
#survey that the margin is more than 0%) perform better or worse than enterprises
#with a negative or zero-margin in terms of the factors that you have identified?
#Support your answer by a plot and by a t-test.

MIP$PosMargin <- ifelse(MIP$Margin >= 4,1 , 0)

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$PosMargin, xlab="Positive Margin", main="Employee Engagement vs Positive Margin") 
boxplot(MIP$Development ~ MIP$PosMargin, xlab="Positive Margin", main="Responsiveness to Innovation vs Positive Margin") 

t.test(MIP$Employees ~ MIP$PosMargin)
t.test(MIP$Development ~ MIP$PosMargin)

# 25 Do firms with a higher percentage of their turnover attributed to newly introduced
#or significantly improved products or services perform better or worse in terms of
#the factors that you have identified?

MIP$InnovTurnoverP12 <- ifelse(MIP$InnovTurnoverP == 1,1 ,ifelse(MIP$InnovTurnoverP == 2,2 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP12, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP12, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP12)
t.test(MIP$Development ~ MIP$InnovTurnoverP12)

MIP$InnovTurnoverP23 <- ifelse(MIP$InnovTurnoverP == 2,2 ,ifelse(MIP$InnovTurnoverP == 3,3 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP23, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP23, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP23)
t.test(MIP$Development ~ MIP$InnovTurnoverP23)

MIP$InnovTurnoverP34 <- ifelse(MIP$InnovTurnoverP == 3,3 ,ifelse(MIP$InnovTurnoverP == 4,4 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP34, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP34, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP34)
t.test(MIP$Development ~ MIP$InnovTurnoverP34)

MIP$InnovTurnoverP45 <- ifelse(MIP$InnovTurnoverP == 4,4 ,ifelse(MIP$InnovTurnoverP == 5,5 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP45, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP45, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP45)
t.test(MIP$Development ~ MIP$InnovTurnoverP45)

MIP$InnovTurnoverP56 <- ifelse(MIP$InnovTurnoverP == 5,5 ,ifelse(MIP$InnovTurnoverP == 6,6 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP56, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP56, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP56)
t.test(MIP$Development ~ MIP$InnovTurnoverP56)

MIP$InnovTurnoverP67 <- ifelse(MIP$InnovTurnoverP == 6,6 ,ifelse(MIP$InnovTurnoverP == 7,7 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP67, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP67, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP67)
t.test(MIP$Development ~ MIP$InnovTurnoverP67)

MIP$InnovTurnoverP78 <- ifelse(MIP$InnovTurnoverP == 7,7 ,ifelse(MIP$InnovTurnoverP == 8,8 ,NA))

##Computing the factor scores:

EFA.2factors<-factanal(MIP[,c(6:12,14:15)], factors=2, rotation= "varimax", scores="regression")

FactorScores<-data.frame(EFA.2factors$scores)

MIP$Employees<-FactorScores$Factor1
MIP$Development<-FactorScores$Factor2

##Adding estimated constructs "Financial Matters", Human Resources" and "Limited Research" to the data

boxplot(MIP$Employees ~ MIP$InnovTurnoverP78, xlab="Turnover from innovation", main="Employees vs Turnover from innovation") 
boxplot(MIP$Development ~ MIP$InnovTurnoverP78, xlab="Turnover from innovation", main="Development vs Turnover from innovation") 

t.test(MIP$Employees ~ MIP$InnovTurnoverP78)
t.test(MIP$Development ~ MIP$InnovTurnoverP78)

