#building a definition for patients with depression 
#(at the time, I didn't realize that disease cases were built so I attempted to build my own)
#through this, I learned a lot about R and SQL!

#ensure you're connected with the database
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]") # change this depending on what you want to access
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)

#do not forget to re-install packages if you restart R
install.packages("sqlQuery")

#testing to know what headings are in which table
res <- sqlQuery(chan,"SELECT TOP 10 * FROM Medication WHERE Name_orig LIKE '%fluoxetine%' OR Name_calc LIKE '%fluoxetine%'")
print(res)

#looking for all patients with depression in database
DepBilling <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
#also check in healthcondition
DepHealthCode <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
#was unable to find datecreated here, so needed to switch to DateOfOnset in SQL query (tldr always note your headings)
DepHealthText <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DiagnosisText_orig LIKE '%depress%' OR DiagnosisText_calc LIKE '%depress'")
#also check in encounter diagnosis
DepEncounterCode <- sqlQuery(chan, "SELECT Patient_ID, DateCreated FROM EncounterDiagnosis WHERE DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
#seems to be very little difference between Orig and Calc Diagnosis Text, but will check both
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

#rbind all medications together into one SINGLE data frame
DepMedTotal <- rbind(DepMedication1, DepMedication2)
DepMedTotal <- rbind(DepMedTotal, DepMedication3)
DepMedTotal <- rbind(DepMedTotal, DepMedication4)
DepMedTotal <- rbind(DepMedTotal, DepMedication5)
DepMedTotal <- rbind(DepMedTotal, DepMedication6)
DepMedTotal <- rbind(DepMedTotal, DepMedication7)
DepMedTotal <- rbind(DepMedTotal, DepMedication8)

#note that before you rbind, the column names need to be the SAME
#change names on columns so that rbind fxn will work
colnames(DepHealthCode) <- (c("Patient_ID", "DateCreated"))
colnames(DepHealthText) <- (c("Patient_ID", "DateCreated"))

#combine all data into one (VERY) large dataframe
DepMaster <- rbind(DepBilling,DepEncounterText)
DepMaster <- rbind(DepMaster,DepEncounterCode)
DepMaster <- rbind(DepMaster, DepHealthText)
DepMaster <- rbind(DepMaster, DepHealthCode)
DepMaster <- rbind(DepMaster, DepMedTotal) #bind with all medications

#change date format and order them, backup DepMaster to another variable just in case something goes wrong
DepMaster$DateCreated <- as.Date(DepMaster$DateCreated, format = "%Y-%m-%d")
DepMaster<- DepMaster[(order(DepMaster$Patient_ID,DepMaster$DateCreated)),]
DepMasterBackup <- DepMaster
View(DepMaster)

#now DepMaster is a list of all encounters, billings, medications, health conditions of 
#people who have been diagnosed with depression in all of DELPHI for all of time

