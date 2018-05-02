#how to connect to database
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)
install.packages('plyr')
library(plyr)
library(RODBC)
res <- sqlQuery(chan,"SELECT TOP 10 * FROM [Patient_deid]")
print(res)

#changes from DELPHI to CPCSSN National 2017 --> Patient becomes Patient_deid

#first section here: exploring CPCSSN

res <- sqlQuery(chan,"SELECT count (*) FROM [Patient_deid]")
print(res)
#wow, there are 1.57 million patients here

res <- sqlQuery(chan,"SELECT TOP 10 * FROM [FamilyHistory]")
print(res)
res <- sqlQuery(chan,"SELECT count (DISTINCT Patient_ID) FROM [FamilyHistory]")
print(res)

res <- sqlQuery(chan,"SELECT TOP 10 * FROM [DiseaseCase]")
print(res)
res <- sqlQuery(chan,"SELECT COUNT (DISTINCT Patient_ID) FROM [DiseaseCase]")
print(res)
res <- sqlQuery(chan,"SELECT COUNT (*) FROM [DiseaseCase]")
print(res)

#second section here: building the DepMaster -- who has depression
DepMaster <- sqlQuery(chan,"SELECT Patient_ID, DateOfOnset FROM [DiseaseCase] WHERE Disease LIKE '%Depression%' AND DateOfOnset < '2009-01-01'")
View(DepMaster)
#I ENDED UP REBUILDING THIS, GO TO SCRIPT 19 AND THAT'S GONNA BE A NEW DEPMASTER.
#40678 entries

#Whoever had depression before cohort, we don't include them
DepMaster$DateOfOnset <- as.Date(DepMaster$DateOfOnset)
DepMaster <- subset(DepMaster, DateOfOnset < "2009-01-01")
View(DepMaster) #around 40K people

#next step, go rebuild the cohort - recopied and made changes here
#original steps and process are in Script 5

PatientBirthYear <- sqlQuery(chan,"SELECT Patient_ID, Sex, BirthYear FROM Patient_deid WHERE BirthYear < 1991")
View(PatientBirthYear)

#Put together a list of all possible encounters
#1.Meds
MedInt <- sqlQuery(chan,"SELECT Patient_ID, StartDate FROM Medication WHERE StartDate > '2008-12-31' AND
        StartDate   < '2011-01-01'")
MedInt$StartDate <- as.Date(MedInt$StartDate, format = "%Y-%m-%d")
colnames(MedInt) <- (c("Patient_ID", "DateCreated"))
View(MedInt)
#2.Encounter
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

dedupe <- function(df) {
  #returns a dataframe where only the first duplicate is retained
  df <- subset(df,!duplicated(df$Patient_ID))
  return(df)
}

CombinedPatientInt <- rbind(MedInt,ProcedureInt)
#do.call("rbind", newList <- list(MedInt, ProcedureInt))
#CombinedPatientInt<- ldply(newList, data.frame)

do.call("rbind", newList <- list(ReferralInt, EncounterInt, BillingInt))
CombinedPatientIntTwo<- ldply(newList, data.frame)

do.call("rbind", newList <- list(LabInt, ExamInt))
CombinedPatientIntThree<- ldply(newList, data.frame)

CombinedPatientIntFinal<- rbind(CombinedPatientInt,CombinedPatientIntTwo)
CombinedPatientIntFinal <- rbind(CombinedPatientIntFinal,CombinedPatientIntThree)

CombinedPatientIntFinalBACKUP <- CombinedPatientIntFinal

CombinedPatientIntFinal <- CombinedPatientIntFinal[(order(CombinedPatientIntFinal$Patient_ID,CombinedPatientIntFinal$DateCreated, decreasing=F)),]
View(CombinedPatientIntFinal)

CombinedPatientIntFinal <- dedupe(CombinedPatientIntFinal)
#around 700,000 entries
rm(CombinedPatientInt)
rm(CombinedPatientIntTwo)
rm(CombinedPatientIntThree)
rm(newList)
rm(ReferralInt, EncounterInt, BillingInt,LabInt,ExamInt,MedInt, ProcedureInt)

PatientIntFinal <- inner_join(CombinedPatientIntFinal, PatientBirthYear, by="Patient_ID")
#443,978 entries

PatientIntCohort <- anti_join(PatientIntFinal, DepMaster, by="Patient_ID")
View(PatientIntCohort)
#414,118 entries

PatientIntCohort <- subset(PatientIntCohort, BirthYear != 0)
#348,495 entries

View(PatientIntCohort)
