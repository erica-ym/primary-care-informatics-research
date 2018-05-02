#calculated on CohortWithDiseaseMICE

#sample size = 315357

View(CohortWithDiseaseMICE)
#mean and SD age of cohort
mean(CohortWithDiseaseMICE$Age)
sd(CohortWithDiseaseMICE$Age)
#56.7 and 17.4

#Mean BMI
#didn't work because NA values, so had to make a new data frame and remove those NAs
newdata <- CohortWithDiseaseMICE[c(4)]
newdata <- na.omit(newdata)
View(newdata)
sd(newdata$BMI)
mean(newdata$BMI)
rm(newdata)
#28.17

#Percent of patients who got depression
sum(CohortWithDiseaseMICE$GotDepression == 1)/nrow(CohortWithDiseaseMICE)
#12.43

count = sum(CohortWithDiseaseMICE$GotDepression == 1)
print(count)

#Percentage of depression patients over 40
sum(CohortWithDiseaseMICE$GotDepression == 1 & CohortWithDiseaseMICE$Age > 40)/sum(CohortWithDiseaseMICE$GotDepression == 1)
#74.53

#Percentage of female depression patients
sum(CohortWithDiseaseMICE$GotDepression == 1 & CohortWithDiseaseMICE$Sex == 'F')/sum(CohortWithDiseaseMICE$GotDepression == 1)
#52.09

#CHRONIC DISEASE PREVALENCES

#COPD
sum(CohortWithDiseaseMICE$COPD == 1)/nrow(CohortWithDiseaseMICE)
#0.015

#Diabetes
sum(CohortWithDiseaseMICE$Diabetes == 1)/nrow(CohortWithDiseaseMICE)
#0.057

#Epilepsy
sum(CohortWithDiseaseMICE$Epilepsy == 1)/nrow(CohortWithDiseaseMICE)
#0.0053

#Anxiety
sum(CohortWithDiseaseMICE$Anxiety == 1)/nrow(CohortWithDiseaseMICE)
#0.046

#CVD
sum(CohortWithDiseaseMICE$CVD == 1)/nrow(CohortWithDiseaseMICE)
#0.044

#RhArthritis
sum(CohortWithDiseaseMICE$RhArthritis == 1)/nrow(CohortWithDiseaseMICE)
#0.0063

#Schizo
sum(CohortWithDiseaseMICE$Schizo == 1)/nrow(CohortWithDiseaseMICE)
#0.0073

#Cancer
sum(CohortWithDiseaseMICE$Cancer == 1)/nrow(CohortWithDiseaseMICE)
#0.032

#Alcohol
sum(CohortWithDiseaseMICE$Alcohol == 1)/nrow(CohortWithDiseaseMICE)
#0.0042

#Percentage of people with depression who have NO other (chronic)condition
percentageDepNo = sum(CohortWithDiseaseMICE$GotDepression == 1 & CohortWithDiseaseMICE$COPD == 0 & CohortWithDiseaseMICE$Diabetes == 0 & CohortWithDiseaseMICE$Epilepsy == 0
    & CohortWithDiseaseMICE$Anxiety == 0 & CohortWithDiseaseMICE$CVD == 0 & CohortWithDiseaseMICE$RhArthritis == 0
    & CohortWithDiseaseMICE$Schizo == 0 & CohortWithDiseaseMICE$Cancer == 0 & CohortWithDiseaseMICE$Alcohol == 0)

percentageDepNo/sum(CohortWithDiseaseMICE$GotDepression == 1) 
#0.819
  
  #No depression, and no chronic condition
sum(CohortWithDiseaseMICE$GotDepression == 0 & CohortWithDiseaseMICE$COPD == 0 & CohortWithDiseaseMICE$Diabetes == 0 & CohortWithDiseaseMICE$Epilepsy == 0
    & CohortWithDiseaseMICE$Anxiety == 0 & CohortWithDiseaseMICE$CVD == 0 & CohortWithDiseaseMICE$RhArthritis == 0
    & CohortWithDiseaseMICE$Schizo == 0 & CohortWithDiseaseMICE$Cancer == 0 & CohortWithDiseaseMICE$Alcohol == 0) /sum(CohortWithDiseaseMICE$GotDepression == 0) 
#0.828



