#between recruitment window of jan 1 2009 - dec 31 2010 (start tracking jan 1 2011)
#who did NOT have depression but had an encounter of some sort: that's our cohort
#must also be above 18
View(DepDiagnosed)
library(plyr); library(dplyr)
#may cause issues if i use both ^^ load dplyr only, if you need plyr, reference it BEFORE

#testing to know what headings are in which table
res <- sqlQuery(chan,"SELECT Patient_ID, Sex, BirthYear FROM Patient WHERE BirthYear < 1991")
View(res)

#getting all list of all patientIDs via age 18 and over
PatientBirthYear <- sqlQuery(chan,"SELECT Patient_ID, Sex, BirthYear FROM Patient_deid WHERE BirthYear < 1991")
View(PatientBirthYear)

#getting all people who had an interaction with primary care provider in recruitment window
#checking Medication, EncounterDiagnosis, Billing, Referral, MedicalProcedure, Lab, Exam
#1.Medication
MedInt <- sqlQuery(chan,"SELECT Patient_ID, StartDate FROM Medication")
MedInt$StartDate <- as.Date(MedInt$StartDate, format = "%Y-%m-%d")
MedInt <- subset(MedInt, StartDate < "2011-01-01" & StartDate > "2008-12-31")
View(MedInt)
colnames(MedInt) <- (c("Patient_ID", "DateCreated"))
#2. EncounterDiagnosis
EncounterInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM EncounterDiagnosis WHERE DateCreated < '2011-01-01' AND DateCreated > '2008-12-31'")
View(EncounterInt)
#3. Billing
BillingInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DateCreated < '2011-01-01' AND DateCreated > '2008-12-31'")
View(BillingInt)
#4. Referral
ReferralInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Referral WHERE DateCreated < '2011-01-01' AND DateCreated > '2008-12-31'")
View(ReferralInt)
#5. MedicalProcedure
ProcedureInt <-sqlQuery(chan, "SELECT Patient_ID,PerformedDate FROM MedicalProcedure WHERE PerformedDate < '2011-01-01' AND PerformedDate > '2008-12-31'")
View(ProcedureInt)
colnames(ProcedureInt) <- (c("Patient_ID", "DateCreated"))
#6. Lab
LabInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Lab WHERE DateCreated < '2011-01-01' AND DateCreated > '2008-12-31'")
View(LabInt)
#7.Exam
ExamInt <-sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Exam WHERE DateCreated < '2011-01-01' AND DateCreated > '2008-12-31'")
View(ExamInt)

#combine all into one datatable
do.call("rbind", newList <- list(ReferralInt, EncounterInt, BillingInt, LabInt, ExamInt, MedInt, ProcedureInt))
AllPatientInt<- ldply(newList, data.frame)
View(AllPatientInt)
#got this error message, but it still seemed to work "Warning message:
#In `[<-.factor`(`*tmp*`, ri, value = c(14613, 14613, 14614, 14614,invalid factor level, NA generated

dedupe <- function(df) {
  #returns a dataframe where only the first duplicate is retained
  df <- subset(df,!duplicated(df$Patient_ID))
  return(df)
}

#sorts the AllPatientInt with all interactions by date and patient ID so that only the first and earliest date remains
AllPatientIntTEST <- AllPatientInt[(order(AllPatientInt$Patient_ID,AllPatientInt$DateCreated, decreasing=F)),]
View(AllPatientIntTEST)

#remove duplicates using pre-built function - end up with 41174 entries
PatientIntFinal <- dedupe(AllPatientIntTEST)
View(PatientIntFinal)

#PatientBirthYear is a collection of patient ID, sex, and birth years
View(PatientBirthYear)
#keep only patients who are over the age of 18, Patient_IDs stored in PatientBirthYear
#effect of inner_join is 37040 entries, because many don't have birthdates and you can't fill in data
PatientIntFinalMerged <- inner_join(PatientIntFinal, PatientBirthYear, by="Patient_ID") 
View(PatientIntFinalMerged)

#remove patients that were already diagnosed with depression, Patient_IDs stored in DepDiagnosed(see script 4)
#this method is from https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#anti_join outputs rows in A that don't have a match in B
PatientIntCohort <- anti_join(PatientIntFinalMerged, DepDiagnosed, by="Patient_ID")
View(PatientIntCohort)
#outputs 31917 entries --> valid given number of inputs 

#remove all entries that have 0 as birth year, 26284 entries
PatientIntCohort <- subset(PatientIntCohort, BirthYear != 0)
View(PatientIntCohort)
