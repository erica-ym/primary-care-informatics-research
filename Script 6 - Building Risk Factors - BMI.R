#goal: build the first risk factor in your cohort, BMI

#ensure you're connected with the database - Need to RUN it
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)
library(dplyr)

#just testing syntax, use as base if you need to write any new code to get things from database
res <- sqlQuery(chan,"SELECT TOP 10 * FROM Exam")
View(res)

#goal is to build a list of Patient_IDs with a list of BMI's
#in the Exam table, BMI's are either given, or the weight and height are given 
#if it's weight and height, you need to calculate! 
#originally included datecreated - now did not because people's BMIs are likely about similar throughout life
#all this code is from the wonderful Jason Black (thank you!)

CBMI <- sqlQuery(chan, "SELECT Patient_ID,Result1_orig,DateCreated FROM Exam WHERE Exam1 LIKE '%BMI%'")
CBMI$DateCreated <- as.Date(CBMI$DateCreated, format = "%Y-%m-%d")
CBMI$Result1_orig <- as.numeric(levels(CBMI$Result1_orig))[CBMI$Result1_orig]
CBMI <- filter(CBMI, Result1_orig > 10 & Result1_orig < 100)
#View(CBMI)

#find and get weights
CHeight <- sqlQuery(chan,"SELECT Patient_ID,Exam1,Result1_orig,DateCreated FROM Exam WHERE Exam.Exam1 LIKE '%Height%'")
CHeight[3] <- as.numeric(levels(CHeight$Result1_orig))[CHeight$Result1_orig]
CHeight <- filter(CHeight, Exam1 == "Height (cm)")
CHeight <- filter(CHeight, Result1_orig > 100, Result1_orig < 200)
#View(CHeight)

CWeight <- sqlQuery(chan,"SELECT Patient_ID,Exam1,Result1_orig,DateCreated FROM Exam WHERE Exam.Exam1 LIKE '%weight%'")
CWeight[3] <- as.numeric(levels(CWeight$Result1_orig))[CWeight$Result1_orig]
CWeight <- filter(CWeight, Exam1 == "Weight (kg)" | Exam1 == "Weight -KG")
CWeight <- filter(CWeight, Result1_orig > 27.2, Result1_orig < 272.5)
#View(CWeight)

#put weight and height together in the same datatable and calculate BMI
CHW <- merge(CHeight, CWeight, by = c("Patient_ID","DateCreated"))
CHW$Result1_orig <- CHW$Result1_orig.y/((CHW$Result1_orig.x/100)^2)

CHW <- CHW[c("Patient_ID","Result1_orig","DateCreated")]
View(CBMI)

CBMI <- rbind(CBMI,CHW)

#without filtration by date (as I had before) it goes from 60 000 to 326 000 entries

#back to Erica writing code
#putting together with the Cohort by Patient_ID
#existing cohort is PatientIntCohort, with birth year, sex and patient ID
CBMI$DateCreated <- NULL

#add an Age column by subtracting the year of DateCreated by their BirthYear
PatientIntCohort$Age = as.numeric(substring(PatientIntCohort$DateCreated,0,4))-PatientIntCohort$BirthYear
colnames(CBMI) <- c("Patient_ID","BMI")
CohortWithBMI <- left_join(PatientIntCohort,CBMI,by="Patient_ID")

#need to dedupe again because the join command added a bunch of duplicates
CohortWithBMI <- dedupe(CohortWithBMI)

#add a column that's 0 or 1 based on if their BMI is large or not
#did this in DELPHI, but not for CPCSSN because did not seem relevant
CohortWithBMI$BMILarge <- ifelse(CohortWithBMI$BMI >=25, 1, 0)
#here's the cohort with BMI,birth year, sex and patient ID
View(CohortWithBMI)
