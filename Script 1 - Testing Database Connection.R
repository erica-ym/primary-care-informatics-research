#this is all me learning how to use R, get on the database, and see what's there
library(RODBC)
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)
res <- sqlQuery(chan,"SELECT TOP 100 * FROM [PatientDemographic_deid]")
print(res)
res <- sqlQuery(chan,"SELECT TOP 100 * FROM [FamilyHistory]")
View(res)
res <- sqlQuery(chan,"SELECT TOP 100 * FROM [Referral]")
View(res)

res <- sqlQuery(chan,"SELECT COUNT(*) FROM [Billing]")
print(res)
res <- sqlQuery(chan,"SELECT [DiagnosisCode_orig] FROM [Billing] WHERE [DiagnosisCode_orig] LIKE 298")
print(res)
res <- sqlQuery(chan,"SELECT COUNT(*) FROM [Medication]")
print(res)
res <- sqlQuery(chan,"SELECT COUNT(*) FROM [PatientDemographic]")
print(res)
res <- sqlQuery(chan,"SELECT TOP 10 * FROM [RiskFactor]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM RiskFactor WHERE Name_calc NOT LIKE '%NA%'")
print(res)

#print the total count in RiskFactor
res <- sqlQuery(chan,"SELECT COUNT(*) FROM [RiskFactor]")
print(res)

#tell me the people in RiskFactor who have notes about previous family cancers - around 240!
FamilyHis <- sqlQuery(chan,"SELECT Patient_ID,Name_orig FROM [RiskFactor] WHERE Name_orig LIKE '%cancer%'")
print(FamilyHis)
nrow(FamilyHis)

#explore EncounterDiagnosis table
res <- sqlQuery(chan,"SELECT TOP 10 * FROM [EncounterDiagnosis]")
print(res)
#count the number of diagnosis that aren't a custom in that column, 50214
res <- sqlQuery(chan,"SELECT COUNT(*) FROM EncounterDiagnosis WHERE DiagnosisCodeType_orig NOT LIKE '%CUSTOM%'")
print(res)
#count the calc Codes that aren't NA, 76762
res <- sqlQuery(chan,"SELECT COUNT(*) FROM EncounterDiagnosis WHERE DiagnosisCode_calc NOT LIKE '%NA%'")
print(res)
#print the number of people diagnosed with depression as per ICD - 9 code
res <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisText_calc,DiagnosisCode_calc FROM [EncounterDiagnosis] WHERE [DiagnosisCode_calc] LIKE '311%'")
print(res)

