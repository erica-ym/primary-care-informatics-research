#building in the next encounter date
#very similar code to BUILDING COHORT (script 5), but this time I'm querying for different dates
#looking for encounters for this same cohort after the recruitment date
#looking for the latest date of all encounters after tracking start date Jan 1 2011

#get your older cohort, before i deduped for multiple dates

#getting all people who had an interaction with primary care provider
#the difference here is we're looking for FINAL FOLLOW UP 
#checking Medication, EncounterDiagnosis, Billing, Referral, MedicalProcedure, Lab, Exam
#1.Medication
MedInt <- sqlQuery(chan,"SELECT Patient_ID, StartDate FROM Medication WHERE StartDate > '2011-01-01'")
MedInt$StartDate <- as.Date(MedInt$StartDate, format = "%Y-%m-%d")
#MedInt <- subset(MedInt, StartDate > "2011-01-01")
View(MedInt)
colnames(MedInt) <- (c("Patient_ID", "DateCreated"))
#2. EncounterDiagnosis
EncounterInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM EncounterDiagnosis WHERE DateCreated > '2011-01-01'")
View(EncounterInt)
#res <- sqlQuery(chan,"SELECT COUNT(*) FROM [EncounterDiagnosis]")
#print(res)
#3. Billing
BillingInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DateCreated > '2011-01-01'")
View(BillingInt)
#4. Referral
ReferralInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Referral WHERE DateCreated > '2011-01-01'")
View(ReferralInt)
#5. MedicalProcedure
ProcedureInt <-sqlQuery(chan, "SELECT Patient_ID,PerformedDate FROM MedicalProcedure WHERE PerformedDate  > '2011-01-01'")
View(ProcedureInt)
colnames(ProcedureInt) <- (c("Patient_ID", "DateCreated"))
#6. Lab
LabInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Lab WHERE DateCreated > '2011-01-01'")
View(LabInt)
#7.Exam
ExamInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Exam WHERE DateCreated > '2011-01-01'")
View(ExamInt)

library(plyr); library(dplyr)
#combine all into one datatable
do.call("rbind", newList <- list(ReferralInt, EncounterInt, BillingInt, LabInt, ExamInt, MedInt, ProcedureInt))
AllPatientIntFinalDates <- ldply(newList, data.frame)
View(AllPatientIntFinalDates)
#got this error message, but it still seemed to work - help!! "Warning message:
#In `[<-.factor`(`*tmp*`, ri, value = c(14613, 14613, 14614, 14614,invalid factor level, NA generated

#left_join
#anti_join
#cheatsheet on R studio dyplyr cheatsheet

dedupe <- function(df) {
  #returns a dataframe where only the first duplicate is retained
  df <- subset(df,!duplicated(df$Patient_ID))
  return(df)
}

#sorts the AllPatientInt with all interactions by date and patient ID so that only the first and earliest date remains
newthing <- AllPatientIntFinalDates[(order(AllPatientIntFinalDates$Patient_ID,AllPatientIntFinalDates$DateCreated, decreasing=T)),]
View(newthing)
#we want the most RECENT Dates

#remove duplicates using pre-built function
PatientIntFinalDates <- dedupe(AllPatientIntFinalDates)
View(PatientIntFinalDates)
colnames(PatientIntFinalDates) <- c("Patient_ID", "MostRecentEncounter")

#merge dates with CohortWithDisease so you have an ending date for each person
CohortWithDisease <- left_join(CohortWithDisease,PatientIntFinalDates,by="Patient_ID")
View(CohortWithDisease)

#create these columns just so it's easier for you to make sense of the data
CohortWithDisease$MostRecentEncounter <- as.Date(CohortWithDisease$MostRecentEncounter, format = "%Y-%m-%d")
CohortWithDisease$YearsBetween <- round((CohortWithDisease$MostRecentEncounter - CohortWithDisease$DateCreated)/365, digits=2)

