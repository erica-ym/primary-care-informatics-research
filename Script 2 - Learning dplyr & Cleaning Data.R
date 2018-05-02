#commented these libraries out because you only need to reference them once per time you open R
#library(RODBC)
#library(dplyr)

#connect to the database again
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)

#thank you to jason for his notes and guidelines here
library(dplyr) 
#finding age of patients, cleaning the data based on patients who have no birth year
res <- sqlQuery(chan,"SELECT TOP 10 * FROM [Patient_deid]")
print(res)
ages <- sqlQuery(chan,"SELECT Patient_ID, BirthYear FROM [Patient_deid]")
ages$CurrentAge<- 2017-ages$BirthYear
#print(ages,10)

#my question at the time: how do I get filtered results into another data table?
#learning how to use dplyr and combine data sets
agesClean <- filter(ages,BirthYear>0) 
head(ages, 10)
nrow(ages)
head(agesClean, 10)
nrow(agesClean)

#grab family history, the little that I can find, from DELPHI
FamilyHisDiag <- sqlQuery(chan, "SELECT Patient_ID,DiagnosisCode_calc FROM [Billing] WHERE DiagnosisCode_calc
                          LIKE '14%' OR DiagnosisCode_calc LIKE '15%' 
                          OR DiagnosisCode_calc LIKE '16%' OR DiagnosisCode_calc 
                          LIKE '17%' OR DiagnosisCode_calc LIKE '18%' 
                          OR DiagnosisCode_calc LIKE '19%' OR DiagnosisCode_calc 
                          LIKE '20%' OR DiagnosisCode_calc LIKE '239%'")
print(FamilyHisDiag)

FamilyHis <- sqlQuery(chan,"SELECT Patient_ID,Name_orig FROM [RiskFactor] WHERE Name_orig LIKE '%cancer%'")
print(FamilyHis)
nrow(FamilyHis)

#learning how to put things together
merge (FamilyHisDiag, FamilyHis, by='Patient_ID')

