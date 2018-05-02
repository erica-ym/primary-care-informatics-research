#you messed up the first time, so let's do this again
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)
res <- sqlQuery(chan,"SELECT TOP 10 * FROM EncounterDiagnosis")
View(res)
library(plyr)
library(RODBC)

DepDiseaseCase <- sqlQuery(chan,"SELECT Patient_ID, DateOfOnset FROM [DiseaseCase] WHERE Disease LIKE '%Depression%' AND DateOfOnset < '2009-01-01'")
DepBilling <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
#also check in healthcondition
DepHealthCode <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
DepHealthText <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DiagnosisText_orig LIKE '%depress%' OR DiagnosisText_calc LIKE '%depress'")
#also check in encounter diagnosis - also ??@jason does my SQL look right
DepEncounterCode <- sqlQuery(chan, "SELECT Patient_ID, DateCreated FROM EncounterDiagnosis WHERE DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
#?? difference between Orig and Calc Diagnosis Text
DepEncounterText <- sqlQuery(chan, "SELECT Patient_ID, DateCreated FROM EncounterDiagnosis WHERE DiagnosisText_orig LIKE '%depress%' OR DiagnosisText_calc LIKE '%depress'")

#now i'm going to look at medications prescribed - from CPSCN Depression Definition document
DepMedication1<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%fluoxetine%' OR Name_calc LIKE '%fluoxetine%'")
DepMedication2<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%citalopram%' OR Name_calc LIKE '%citalopram%'")
DepMedication3<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%escitalopram%' OR Name_calc LIKE '%escitalopram%'")
DepMedication4<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%mirtazapine%' OR Name_calc LIKE '%mirtazapine%'")
DepMedication5<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%moclobemide%' OR Name_calc LIKE '%moclobemide%'")
DepMedication6<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%sertraline%' OR Name_calc LIKE '%sertraline%'")
DepMedication7<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%tranylcypromine%' OR Name_calc LIKE '%tranylcypromine%'")
DepMedication8<- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_orig LIKE '%amitriptyline%' OR Name_calc LIKE '%amitriptyline%'")

#rbind all medications together
DepMedTotal <- rbind(DepMedication1, DepMedication2)
DepMedTotal <- rbind(DepMedTotal, DepMedication3)
DepMedTotal <- rbind(DepMedTotal, DepMedication4)
DepMedTotal <- rbind(DepMedTotal, DepMedication5)
DepMedTotal <- rbind(DepMedTotal, DepMedication6)
DepMedTotal <- rbind(DepMedTotal, DepMedication7)
DepMedTotal <- rbind(DepMedTotal, DepMedication8)
View(DepMedTotal)

#change names on columns so that rbind fxn will work
colnames(DepHealthCode) <- (c("Patient_ID", "DateCreated"))
colnames(DepHealthText) <- (c("Patient_ID", "DateCreated"))
colnames(DepDiseaseCase) <- (c("Patient_ID", "DateCreated"))

#combine all data into one large dataframe
DepMaster <- rbind(DepBilling,DepEncounterText)
DepMaster <- rbind(DepMaster,DepEncounterCode)
DepMaster <- rbind(DepMaster, DepHealthText)
DepMaster <- rbind(DepMaster, DepHealthCode)
DepMaster <- rbind(DepMaster, DepDiseaseCase)
DepMaster <- rbind(DepMaster, DepMedTotal) #bind with all medications

#change date format and order them, backup DepMaster to another variable
DepMaster$DateCreated <- as.Date(DepMaster$DateCreated, format = "%Y-%m-%d")
DepMaster<- DepMaster[(order(DepMaster$Patient_ID,DepMaster$DateCreated)),]
DepMasterBackup <- DepMaster
DepMaster <-dedupe(DepMaster)

#Whoever had depression before cohort, we don't include them
DepMaster <- subset(DepMaster, DateCreated < "2009-01-01")
#used the one below
DepMaster <- subset(DepMaster, DateCreated < "2011-01-01")
View(DepMaster)
#107K people

#entire cohort of people is here in CombinedPatientIntFinal
View(CombinedPatientIntFinal)
View(PatientBirthYear)

PatientIntFinal <- inner_join(CombinedPatientIntFinal, PatientBirthYear, by="Patient_ID")
View(PatientIntFinal)
#443K entries

PatientIntCohort <- anti_join(PatientIntFinal, DepMaster, by="Patient_ID")
View(PatientIntCohort)
#362K entries

PatientIntCohort <- subset(PatientIntCohort, BirthYear != 0)
#362K entries

View(PatientIntCohort)

#this is a cohort with everyone who had depression before first start date removed



