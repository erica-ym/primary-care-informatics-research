#how to use mice on this data - fun with data imputation
#using MICE
CohortWithDiseaseMICE <- CohortWithDiseaseMOD
View(CohortWithDiseaseMICE)

#removing columns I don't want to predict or things that were just used in the process of building cohort
CohortWithDiseaseMICE$Patient_ID <- NULL
CohortWithDiseaseMICE$DateCreated <- NULL
CohortWithDiseaseMICE$BMILarge <- NULL
CohortWithDiseaseMICE$MostRecentEncounter <- NULL
CohortWithDiseaseMICE$YearsBetween <- NULL
CohortWithDiseaseMICE$GotDepression <- ifelse(!is.na(CohortWithDiseaseMICE$YearsSinceDepDiag),1,NA)
CohortWithDiseaseMICE$DepressionDiagDate <- NULL
CohortWithDiseaseMICE$YearsSinceDepDiag <- NULL
CohortWithDiseaseMICE$Depression <- NULL

View(CohortWithDiseaseMICE)
install.packages("VIM")
library(mice)
library(lattice)
library(VIM)

md.pattern (CohortWithDiseaseMICE) #hard to see importance of this given so many data points
#difficult to make valuable VIM representations with so much data
#md.pairs(CohortWithDiseaseMICE)
#to see what each variable is
#str(CohortWithDiseaseMICE)
methodsList <- c("logreg", "pmm", "pmm", "pmm", "logreg","logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "sample", "pmm", "pmm", "logreg")
length(methodsList)
#first attempt to run mice
tempData <- mice(CohortWithDiseaseMICE, m=5, seed = 19000, method = methodsList)
#used warnings() to see what was wrong - need to make categorical variables into factors for mice to recognize
warnings()
#print methods that were used
tempData$meth
#the response to this is currently NULL
levels(CohortWithDiseaseMICE$CCOPD)
#convert to factor
CohortWithDiseaseMICE$CCOPD <- factor(CohortWithDiseaseMICE$CCOPD)
#now it has levels, this produces "0" and "1" -- so now logreg in mice can recognize
levels(CohortWithDiseaseMICE$CCOPD)
#note: I made a mistake with CCOPD in that I forgot to take out the 0s and make them 1s - I have fixed this here
#added a line of code to fix it in Script 9, but this next line is a quick fix
#reminder to self: you originally made the 0s into 1s because the 0s were people who got diagnosed
#with that other chronic disease DURING the 5 year study, they didn't have it at baseline
CohortWithDiseaseMICE$CCOPD <- ifelse(CohortWithDiseaseMICE$CCOPD==0, NA, 1)
#converting all the categorical columns into factors
CohortWithDiseaseMICE[5:14] <- lapply(CohortWithDiseaseMICE[5:14], factor)
CohortWithDiseaseMICE$GotDepression <- factor(CohortWithDiseaseMICE$GotDepression)
levels(CohortWithDiseaseMICE$Smoker)
#now each factor has one level and is recognizable by logreg within mice
#so we can run mice again and maybe it will work this time!!
#refactor


#second attempt to run mice
tempData <- mice(CohortWithDiseaseMICE, m=5, seed = 19000, method = methodsList)
summary(tempData)
#no warnings this time - yay
#results of summary: No missing data in sex, birth year, age, postal, Site_ID
#3454 missing in BMI
#196 missing in Income
#between 16000 and 20000 missing in all other categories
#looking at predictor matrix - there aren't many 1s - I want everything to predict everything!!
#need to upload my own predictor matrix - thank you to Jason for this advice
tempData$predictorMatrix #current matrix
PredMat <- read.csv("PredMat.csv", sep = ";", header = FALSE)
PredMat <- as.matrix(PredMat)
View(PredMat)
#third time running mice

#next steps, using with() to analyze the results of each dataset
#then pool() to put them back together into one

expr <- expression(glm(GotDepression~BMI+Age, family=binomial(link='logit')))
fit <- with(data=tempData, eval(expr))
summary(pool(fit)) 

#now I decide what model I want to run on it
#lm()would be linear model

summary(pool(fit))  
finalpool <- pool(fit)

#a test to see if my factors need 0s
CohortWithDiseaseMICETWO<-CohortWithDiseaseMICE
CohortWithDiseaseMICETWO[5:14] <- lapply(CohortWithDiseaseMICETWO[5:14], function(x) ifelse(is.na(x),0,1))
CohortWithDiseaseMICETWO$GotDepression <- ifelse(is.na(CohortWithDiseaseMICETWO$GotDepression),0,1)
CohortWithDiseaseMICE[5:14] <- lapply(CohortWithDiseaseMICE[5:14], factor)
CohortWithDiseaseMICETWO$GotDepression <- factor(CohortWithDiseaseMICETWO$GotDepression)
CohortWithDiseaseMICETWO$Sex <- factor(CohortWithDiseaseMICETWO$Sex)
View(CohortWithDiseaseMICETWO)
tempDataTWO <- mice(CohortWithDiseaseMICETWO, m=5, predictorMatrix = PredMat, seed = 19000, method = methodsList)
summary(tempDataTWO)

expr <- expression(glm(GotDepression~BMI+Age, family=binomial(link='logit')))
fit <- with(data=tempDataTWO, eval(expr))
summary(pool(fit)) 

#THIS DOWN HERE IS A WORKING ONE FINALLY
expr <- expression(glm(GotDepression~BMI+Age+Sex+Income+CCOPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol+Smoker, family=binomial(link='logit')))
fitTwo <- with(data=tempDataTWO, eval(expr))
summary(pool(fitTwo)) 


