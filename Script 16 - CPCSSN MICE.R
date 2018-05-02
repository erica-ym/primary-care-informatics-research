#how to use mice on this data - fun with data imputation
#using MICE
#315359 people
PatientIntCohortFinal$Age <- 2018-PatientIntCohortFinal$BirthYear
#Add in BMI if you forgot
#PatientIntCohortFinal<- PatientIntCohortFinal[,c(3,4,19,18,5,6,7,8,9,10,11,12,13,16)]
#^^ only there because R crashed and you needed to rebuild
CohortWithDiseaseMICE <- PatientIntCohortFinal
View(CohortWithDiseaseMICE)
View(PatientIntCohortFinal)

#removing columns I don't want to predict or things that were just used in the process of building cohort
CohortWithDiseaseMICE$Patient_ID <- NULL
CohortWithDiseaseMICE$DateCreated <- NULL
CohortWithDiseaseMICE$BirthYear <- NULL
CohortWithDiseaseMICE$Site_ID <- NULL
CohortWithDiseaseMICE$PostalCodeFinal <- NULL
CohortWithDiseaseMICE$MostRecentEncounter <- NULL
CohortWithDiseaseMICE$YearsBetween <- NULL
#CohortWithDiseaseMICE$GotDepression <- ifelse(is.na(CohortWithDiseaseMICE$DepressionDiagDate),0,1)
CohortWithDiseaseMICE$GotDepression <- ifelse(!is.na(CohortWithDiseaseMICE$YearsSinceDepDiag),1,0)
CohortWithDiseaseMICE$DepressionDiagDate <- NULL
CohortWithDiseaseMICE$YearsSinceDepDiag <- NULL
count(CohortWithDiseaseMICE$GotDepression==1) #39193/ 315369

CohortWithDiseaseMICE[5:13] <- lapply(CohortWithDiseaseMICE[5:13], function(x) ifelse(is.na(x),0,1))
CohortWithDiseaseMICE <- subset(CohortWithDiseaseMICE, !is.na(CohortWithDiseaseMICE$Sex))
CohortWithDiseaseMICE$Sex <- ifelse(CohortWithDiseaseMICE$Sex == "Female", "F", "M")
CohortWithDiseaseMICE$Sex <- as.factor(CohortWithDiseaseMICE$Sex)
contrasts(CohortWithDiseaseMICE$Sex)
CohortWithDiseaseMICE[5:14] <- lapply(CohortWithDiseaseMICE[5:14], as.factor)
#not using income
#CohortWithDiseaseMICE$Income <- as.numeric(levels(CohortWithDiseaseMICE$Income))[CohortWithDiseaseMICE$Income]

install.packages("caret")
install.packages("VIM")
install.packages("mice")
library(mice)
library(lattice)
library(VIM)
library(caret)
library(ggplot2)

View(CohortWithDiseaseMICEBACKUP)
CohortWithDiseaseMICEBACKUP <- CohortWithDiseaseMICE
View(CohortWithDiseaseMICE)

#trying to get MICE to work again 
MICETesting <- CohortWithDiseaseMICE[sample(1:nrow(CohortWithDiseaseMICE), 10000,
                                           replace=FALSE),]
tempData$predictorMatrix #current matrix
PredMat <- read.csv("PredMat.csv", sep = ";", header = FALSE)
PredMat <- as.matrix(PredMat)
PredMat <- PredMat[, -c(15:18)]
PredMat <- PredMat[-c(15,16,17,18), ]
View(PredMat)

methodsList <- c("logreg", "norm", "norm", "norm", "logreg","logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "Logreg")
length(methodsList)

tempDataTWO <- mice(MICETesting, m=5, predictorMatrix = PredMat, seed = 19000, method = methodsList)
summary(tempDataTWO)
expr <- expression(model)
fit <- with(data=tempDataTWO, eval(expr))
summary(pool(fit))
#MICE works on the 10,000 data set

#Trying MICE again on the WHOLE dataset
tempData <- mice(CohortWithDiseaseMICE, m=5, predictorMatrix = PredMat, seed = 19000, method = methodsList)
summary(tempData)
View(tempData)
#model <- glm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, family=binomial(link='logit'))
print(model)
#running model on fully imputed dataset
expr <- expression(glm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, family=binomial(link='logit')))
fit <- with(data=tempData, eval(expr))
MICEModelData <- summary(pool(fit))
MICECoefficients <- MICEModelData[, c('est')]
View(MICECoefficients)

## 70% of the sample size
smp_size <- floor(0.75 * nrow(CohortWithDiseaseMICE))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(CohortWithDiseaseMICE)), size = smp_size)

train <- CohortWithDiseaseMICE[train_ind, ]
test <- CohortWithDiseaseMICE[-train_ind, ]
View(train)
View(test)

#This end part is just testing if the models could be built on a small part of the dataset
TestSample <- CohortWithDiseaseMICE[sample(1:nrow(CohortWithDiseaseMICE), 10000,
                          replace=FALSE),]
View(TestSample)
#all factors
glm(GotDepression~., data=TestSample, family=binomial(link='logit'))
#income taken out
model <- glm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=TestSample, family=binomial(link='logit'))
#smaller factors taken out - just a test
glm(GotDepression~BMI+Age+Sex+Epilepsy+Anxiety+RhArthritis+Schizo+Alcohol, data=TestSample, family=binomial(link='logit'))

summary(model)



              