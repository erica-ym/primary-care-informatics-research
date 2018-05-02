#at this point, CohortWithDiseaseMOD has 19060 entries
#what i did for final processing is all in Script 9

sum()
count()
View(CohortWithDiseaseMOD)

#mean and SD age of cohort
mean(CohortWithDiseaseMOD$Age)
sd(CohortWithDiseaseMOD$Age)
#51.3 and 16.99

#percent female
sum(CohortWithDiseaseMOD$Sex == 'Female')/nrow(CohortWithDiseaseMOD)
#55.5%

#Total count of cohort
nrow(CohortWithDiseaseMOD)
#16480 people

#Mean BMI
CohortWithDiseaseMOD$BMI <- as.numeric(CohortWithDiseaseMOD$BMI)
mean(CohortWithDiseaseMOD$BMI)
#didn't work because NA values, so had to make a new data frame and remove those NAs
newdata <- CohortWithDiseaseMOD[c(6)]
View(newdata)
mean(newdata$BMI)
#28.96
rm(newdata)

#Percent of patients who got depression
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))/nrow(CohortWithDiseaseMOD)
#11.92%

#Number of patients who got depression
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#1964 people

#Percentage of depression patients over 30
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & CohortWithDiseaseMOD$Age > 30)/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#83.2%

#Percentage of depression patients with large BMI
CohortWithDiseaseMOD$BMILarge <- as.integer(CohortWithDiseaseMOD$BMILarge)
count(CohortWithDiseaseMOD$BMILarge == 1)
sum(CohortWithDiseaseMOD$BMILarge == 1 & !is.na(CohortWithDiseaseMOD$BMILarge) & !is.na(CohortWithDiseaseMOD$DepressionDiagDate))/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#57.9%

#Number of women who got depression
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & CohortWithDiseaseMOD$Sex=="Female")
#1261 people

#Percentage of depression-diagnosed who are women
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & CohortWithDiseaseMOD$Sex=="Female")/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#64.2%

#Percentage of people with depression who have NO other (chronic)condition
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & !is.na(CohortWithDiseaseMOD[c(8,9,10,11,12,13,14,15)]))/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#24.08%
#inverse of that, so ppl that have AT LEAST ONE other chronic condition
1-(sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & !is.na(CohortWithDiseaseMOD[c(8,9,10,11,12,13,14,15)]))/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate)))

#income comparisons
summary(CohortWithDiseaseMOD$Income)
#first quartile = 38230, median = 44950
sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate) & CohortWithDiseaseMOD$Income<=44950 & !is.na(CohortWithDiseaseMOD$Income))/sum(!is.na(CohortWithDiseaseMOD$DepressionDiagDate))
#first quartile is 14%, median is 29.8%

CohortWithDiseaseMOD$Depression <- ifelse(!is.na(CohortWithDiseaseMOD$DepressionDiagDate),1,NA)
#looking at normality for income
plot(CohortWithDiseaseMOD$Income, CohortWithDiseaseMOD$Depression)
qqnorm(CohortWithDiseaseMOD$Income)
qqline(CohortWithDiseaseMOD$Income)
