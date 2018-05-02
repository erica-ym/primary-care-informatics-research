#ensure you're connected with the database
chan <- odbcConnect("DELPHI",uid="eyarmolm",pwd="NJzQVcfTi8-jGHJUobmh")
print(chan)
res <- sqlQuery(chan,"SELECT name FROM master.dbo.sysdatabases")
print(res)
res <- sqlQuery(chan,"USE [CPCSSN National 2017]")
print(res)
res <- sqlQuery(chan,"SELECT * FROM information_schema.tables")
print(res)
library(dplyr)
library(RODBC)

#TEST
res <- sqlQuery(chan,"SELECT TOP 10 * FROM Billing WHERE DiagnosisCode_calc LIKE '491%' ")
View(res)

#backup the base dataset to another variable
CohortWithBMIBACKUP <- CohortWithBMI

CohortWithDisease <- CohortWithBMI
#testing to view the number of patients in validated disease case, compared this number to my own collected by checking every table
#for curiosity's sake:
NumberofDepPatients <- sqlQuery(chan, "SELECT Patient_ID FROM DiseaseCase WHERE Disease LIKE '%Depression%'")
View(NumberofDepPatients)
#i don't have the numbers, but I remember them being something like 15% apart which isn't bad! 

#here I am building data frames of people with each chronic disease
#around half of this code - especially for the medicines part - was provided by Jason (thank you!)
#get the data using sqlQuery, rearrange the data frame in R, make any necessary changes to formatting
#use the ifelse like to add it to the CohortWithDisease if the date of onset was earlier than date added to study for person

CCOPD <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM DiseaseCase WHERE Disease LIKE '%COPD%' AND DateOfOnset NOT LIKE 'NA'")
CCOPD <- dedupe(CCOPD)
View(CCOPD)
#uneeded line of codeCCOPD$DateOfOnset <- NULL
#CCOPD$COPD <- 1
CohortWithDisease <- left_join(CohortWithBMI,CCOPD,by="Patient_ID")
View(CohortWithDisease)
CohortWithDisease$DateCreated <- as.Date(CohortWithBMI$DateCreated, format = "%Y-%m-%d")
CCOPD$DateOfOnset <- as.Date(CCOPD$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease$CCOPD <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

CDiabetes <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM DiseaseCase WHERE Disease LIKE '%Diabetes%' AND DateOfOnset NOT LIKE 'NA'")
CDiabetes <-dedupe(CDiabetes)
View(CDiabetes)
CDiabetes$DateOfOnset <- as.Date(CDiabetes$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CDiabetes,by="Patient_ID")
CohortWithDisease$Diabetes <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

CEpilepsy <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM DiseaseCase WHERE Disease LIKE '%Epilepsy%' AND DateOfOnset NOT LIKE 'NA'")
CEpilepsy <-dedupe(CEpilepsy)
CEpilepsy$DateOfOnset <- as.Date(CEpilepsy$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CEpilepsy,by="Patient_ID")
CohortWithDisease$Epilepsy <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#all Jason's anxiety code below
CAnxiety_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '300.0%'")
CAnxiety_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND HealthCondition.DiagnosisCode_calc LIKE '300.0%'")
CAnxiety_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig NOT LIKE '%FAM%' AND HealthCondition.DiagnosisText_orig LIKE '%anxiety%'")
CAnxiety_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisCode_calc LIKE '300.0%'")
CAnxiety_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%anxiety%'")
CAnxiety_Bill$DateCreated <- as.Date(CAnxiety_Bill$DateCreated, format = "%Y-%m-%d")
CAnxiety_HC$DateCreated <- as.Date(CAnxiety_HC$DateCreated, format = "%Y-%m-%d")
CAnxiety_HCtext$DateCreated <- as.Date(CAnxiety_HCtext$DateCreated, format = "%Y-%m-%d")
CAnxiety_ED$DateCreated <- as.Date(CAnxiety_ED$DateCreated, format = "%Y-%m-%d")
CAnxiety_EDtext$DateCreated <- as.Date(CAnxiety_EDtext$DateCreated, format = "%Y-%m-%d")
CAnxiety_all <- rbind(CAnxiety_Bill,CAnxiety_HC)
CAnxiety_all <- rbind(CAnxiety_all,CAnxiety_HCtext)
CAnxiety_all <- rbind(CAnxiety_all,CAnxiety_ED)
CAnxiety_all <- rbind(CAnxiety_all,CAnxiety_EDtext)
CAnxiety_all <- filter(CAnxiety_all, DateCreated < as.Date("2011-01-01", format = "%Y-%m-%d"))
CAnxiety_all <- CAnxiety_all[(order(CAnxiety_all$Patient_ID,CAnxiety_all$DateCreated)),]
CAnxiety_all <- dedupe(CAnxiety_all)
View(CAnxiety_all)
CAnxiety_all$DateOfOnset <- as.Date(CAnxiety_all$DateOfOnset, format = "%Y-%m-%d")
colnames(CAnxiety_all) <- c("Patient_ID", "DateOfOnset")
CohortWithDisease <- left_join(CohortWithDisease,CAnxiety_all,by="Patient_ID")
CohortWithDisease$Anxiety <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#all of Jason's CVD code
CCVD_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '41%' OR DiagnosisCode_calc LIKE '42%'")
CCVD_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND (DiagnosisCode_calc LIKE '41%' OR DiagnosisCode_calc LIKE '42%')")
CCVD_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE DiagnosisText_orig NOT LIKE '%FAM%' AND (DiagnosisText_orig LIKE '%CVD%' OR DiagnosisText_orig LIKE '%cardiovascular disease%' OR DiagnosisText_orig LIKE '%coronary artery disease%' OR DiagnosisText_orig LIKE 'cad' OR DiagnosisText_orig LIKE '%heart attack%' OR DiagnosisText_orig LIKE '%myocardial infarction%' OR DiagnosisText_orig LIKE '%heart disease%')")
CCVD_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE (DiagnosisCode_calc LIKE '41%' OR DiagnosisCode_calc LIKE '42%')")
CCVD_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisText_orig LIKE '%CVD%' OR DiagnosisText_orig LIKE '%cardiovascular disease%' OR DiagnosisText_orig LIKE '%coronary artery disease%' OR DiagnosisText_orig LIKE 'cad' OR DiagnosisText_orig LIKE '%heart attack%' OR DiagnosisText_orig LIKE '%myocardial infarction%' OR DiagnosisText_orig LIKE '%heart disease%'")

CRivaroxaban <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Rivaroxaban%' OR Name_orig LIKE '%Rivaroxaban%'")
CDabigatran <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Dabigatran%' OR Name_orig LIKE '%Dabigatran%'")
CApixaban <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Apixaban%' OR Name_orig LIKE '%Apixaban%'")
CHeparin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Heparin%' OR Name_orig LIKE '%Heparin%'")
CWarfarin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Warfarin%' OR Name_orig LIKE '%Warfarin%'")
CClopidogrel <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Clopidogrel%' OR Name_orig LIKE '%Clopidogrel%'")
CDipyridamole <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Dipyridamole%' OR Name_orig LIKE '%Dipyridamole%'")
CPrasugrel <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Prasugrel%' OR Name_orig LIKE '%Prasugrel%'")
CTicagrelor <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Ticagrelor%' OR Name_orig LIKE '%Ticagrelor%'")

CCVD_Meds <- rbind(CRivaroxaban,CDabigatran)
CCVD_Meds <- rbind(CCVD_Meds,CApixaban)
CCVD_Meds <- rbind(CCVD_Meds,CHeparin)
CCVD_Meds <- rbind(CCVD_Meds,CWarfarin)
CCVD_Meds <- rbind(CCVD_Meds,CClopidogrel)
CCVD_Meds <- rbind(CCVD_Meds,CDipyridamole)
CCVD_Meds <- rbind(CCVD_Meds,CPrasugrel)
CCVD_Meds <- rbind(CCVD_Meds,CTicagrelor)

CCVD_Bill$DateCreated <- as.Date(CCVD_Bill$DateCreated, format = "%Y-%m-%d")
CCVD_HC$DateCreated <- as.Date(CCVD_HC$DateCreated, format = "%Y-%m-%d")
CCVD_HCtext$DateCreated <- as.Date(CCVD_HCtext$DateCreated, format = "%Y-%m-%d")
CCVD_ED$DateCreated <- as.Date(CCVD_ED$DateCreated, format = "%Y-%m-%d")
CCVD_EDtext$DateCreated <- as.Date(CCVD_EDtext$DateCreated, format = "%Y-%m-%d")
CCVD_Meds$DateCreated <- as.Date(CCVD_Meds$DateCreated, format = "%Y-%m-%d")

CCVD_all <- rbind(CCVD_Bill,CCVD_HC)
CCVD_all <- rbind(CCVD_all,CCVD_HCtext)
CCVD_all <- rbind(CCVD_all,CCVD_ED)
CCVD_all <- rbind(CCVD_all,CCVD_EDtext)
CCVD_all <- rbind(CCVD_all,CCVD_Meds)

CCVD_all <- CCVD_all[(order(CCVD_all$Patient_ID,CCVD_all$DateCreated)),]
CCVD_all <- dedupe(CCVD_all)
colnames(CCVD_all) <- c("Patient_ID", "DateOfOnset")
CCVD_all$DateOfOnset <- as.Date(CCVD_all$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CCVD_all,by="Patient_ID")
CohortWithDisease$CVD <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#Jason's Rhematoid Arthritis def
CRA_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '714%'")
CRA_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '714%'")
CRA_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig NOT LIKE '%FAM%' AND HealthCondition.DiagnosisText_orig LIKE '%Rheumatoid arthritis%'")
CRA_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisCode_calc LIKE '714%'")
CRA_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%rheumatoid arthritis%'")

CRA_Bill$DateCreated <- as.Date(CRA_Bill$DateCreated, format = "%Y-%m-%d")
CRA_HC$DateCreated <- as.Date(CRA_HC$DateCreated, format = "%Y-%m-%d")
CRA_HCtext$DateCreated <- as.Date(CRA_HCtext$DateCreated, format = "%Y-%m-%d")
CRA_ED$DateCreated <- as.Date(CRA_ED$DateCreated, format = "%Y-%m-%d")
CRA_EDtext$DateCreated <- as.Date(CRA_EDtext$DateCreated, format = "%Y-%m-%d")

CRA_all <- rbind(CRA_Bill,CRA_HC)
CRA_all <- rbind(CRA_all,CRA_HCtext)
CRA_all <- rbind(CRA_all,CRA_ED)
CRA_all <- rbind(CRA_all,CRA_EDtext)

CRA_all <- CRA_all[(order(CRA_all$Patient_ID,CRA_all$DateCreated)),]
CRA_all <- dedupe(CRA_all)
View(CRA_all)

colnames(CRA_all) <- c("Patient_ID", "DateOfOnset")
CRA_all$DateOfOnset <- as.Date(CRA_all$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CRA_all,by="Patient_ID")
CohortWithDisease$RhArthritis <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#Jason's Schizophrenia def
CSchizophrenia_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '295%'")
CSchizophrenia_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND HealthCondition.DiagnosisCode_calc LIKE '295%'")
CSchizophrenia_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig NOT LIKE '%FAM%' AND HealthCondition.DiagnosisText_orig LIKE '%schizo%'")
CSchizophrenia_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisCode_calc LIKE '295%'")
CSchizophrenia_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%schizo%'")

CAripiprazole <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Aripiprazole%' OR Name_orig LIKE '%Aripiprazole%' OR Name_orig LIKE '%Abilify%' OR Code_calc LIKE '%N05AX12%'")
CAsenapine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Asenapine%' OR Name_orig LIKE '%Asenapine%' OR Name_orig LIKE '%Saphris%' OR Code_calc LIKE '%N05AH05%'")
CBrexpiprazole <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Brexpiprazole%' OR Name_orig LIKE '%Brexpiprazole%' OR Name_orig LIKE '%Rexulti%' OR Code_calc LIKE '%N05AX16%'")
CCariprazine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Cariprazine%' OR Name_orig LIKE '%Cariprazine%' OR Name_orig LIKE '%Vraylar%' OR Code_calc LIKE '%N05AX15%'")
CIloperidone <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Iloperidone%' OR Name_orig LIKE '%Iloperidone%' OR Name_orig LIKE '%Fanapt%' OR Code_calc LIKE '%N05AX14%'")
CLurasidone <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Lurasidone%' OR Name_orig LIKE '%Lurasidone%' OR Name_orig LIKE '%Latuda%' OR Code_calc LIKE '%N05AE05%'")
COlanzapine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%olanzapine%' OR Name_orig LIKE '%olanzapine%' OR Name_orig LIKE '%Zyprexa%' OR Name_orig LIKE '%Ozace%' OR Name_orig LIKE '%Lanzek%' OR Name_orig LIKE '%Zypadhera%' OR Code_calc LIKE '%N05AH03%'")
CPaliperidone <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Paliperidone%' OR Name_orig LIKE '%Paliperidone%' OR Name_orig LIKE '%Invega%' OR Code_calc LIKE '%N05AX13%'")
CQuetiapine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%quetiapine%' OR Name_orig LIKE '%quetiapine%' OR Name_orig LIKE '%Seroquel%' OR Code_calc LIKE '%N05AH04%'")
CRisperidone <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%risperidone%' OR Name_orig LIKE '%risperidone%' OR Name_orig LIKE '%Risperdal%' OR Name_orig LIKE '%Zepidone%' OR Code_calc LIKE '%N05AX08%'")
CZiprasidone <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%ziprasidone%' OR Name_orig LIKE '%ziprasidone%' OR Name_orig LIKE '%Geodon%' OR Name_orig LIKE '%Zeldox%' OR Code_calc LIKE '%N05AE04%'")
CChlorpromazine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Chlorpromazine%' OR Name_orig LIKE '%Chlorpromazine%' OR Name_orig LIKE '%Largactil%' OR Name_orig LIKE '%Thorazine%' OR Code_calc LIKE '%N05AA01%'")
CFluphenazine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Fluphenazine%' OR Name_orig LIKE '%Fluphenazine%' OR Name_orig LIKE '%Prolixin%' OR Name_orig LIKE '%Modecate%' OR Code_calc LIKE '%N05AB02%'")
CHaloperidol <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%Haloperidol%' OR Name_orig LIKE '%Haloperidol%' OR Name_orig LIKE '%Haldol%' OR Code_calc LIKE '%N05AD01 %'")
CPerphenazine <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [Medication] WHERE Name_calc LIKE '%perphenazine%' OR Name_orig LIKE '%perphenazine%' OR Name_orig LIKE '%Trilafon%' OR Code_calc LIKE '%N05AB03%'")

CSchizophrenia_Meds <- rbind(CAripiprazole,CAsenapine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CBrexpiprazole)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CCariprazine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CIloperidone)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CLurasidone)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,COlanzapine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CPaliperidone)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CQuetiapine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CRisperidone)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CZiprasidone)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CChlorpromazine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CFluphenazine)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CHaloperidol)
CSchizophrenia_Meds <- rbind(CSchizophrenia_Meds,CPerphenazine)

CSchizophrenia_Bill$DateCreated <- as.Date(CSchizophrenia_Bill$DateCreated, format = "%Y-%m-%d")
CSchizophrenia_HC$DateCreated <- as.Date(CSchizophrenia_HC$DateCreated, format = "%Y-%m-%d")
CSchizophrenia_HCtext$DateCreated <- as.Date(CSchizophrenia_HCtext$DateCreated, format = "%Y-%m-%d")
CSchizophrenia_ED$DateCreated <- as.Date(CSchizophrenia_ED$DateCreated, format = "%Y-%m-%d")
CSchizophrenia_EDtext$DateCreated <- as.Date(CSchizophrenia_EDtext$DateCreated, format = "%Y-%m-%d")
CSchizophrenia_Meds$DateCreated <- as.Date(CSchizophrenia_Meds$DateCreated, format = "%Y-%m-%d")

CSchizophrenia_all <- rbind(CSchizophrenia_Bill,CSchizophrenia_HC)
CSchizophrenia_all <- rbind(CSchizophrenia_all,CSchizophrenia_HCtext)
CSchizophrenia_all <- rbind(CSchizophrenia_all,CSchizophrenia_ED)
CSchizophrenia_all <- rbind(CSchizophrenia_all,CSchizophrenia_EDtext)
CSchizophrenia_all <- rbind(CSchizophrenia_all,CSchizophrenia_Meds)

CSchizophrenia_all <- CSchizophrenia_all[(order(CSchizophrenia_all$Patient_ID,CSchizophrenia_all$DateCreated)),]
CSchizophrenia_all <- dedupe(CSchizophrenia_all)
View(CSchizophrenia_all)

colnames(CSchizophrenia_all) <- c("Patient_ID", "DateOfOnset")
CSchizophrenia_all$DateOfOnset <- as.Date(CSchizophrenia_all$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CSchizophrenia_all,by="Patient_ID")
CohortWithDisease$Schizo <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#Jason's Cancer definition
CCancer_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '14%' OR DiagnosisCode_calc LIKE '15%' OR DiagnosisCode_calc LIKE '16%' OR DiagnosisCode_calc LIKE '17%' OR DiagnosisCode_calc LIKE '18%' OR DiagnosisCode_calc LIKE '19%' OR DiagnosisCode_calc LIKE '20%' OR DiagnosisCode_calc LIKE '239%'")
CCancer_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND (DiagnosisCode_calc LIKE '14%' OR DiagnosisCode_calc LIKE '15%' OR DiagnosisCode_calc LIKE '16%' OR DiagnosisCode_calc LIKE '17%' OR DiagnosisCode_calc LIKE '18%' OR DiagnosisCode_calc LIKE '19%' OR DiagnosisCode_calc LIKE '20%' OR DiagnosisCode_calc LIKE '239%')")
CCancer_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig NOT LIKE '%FAM%' AND (HealthCondition.DiagnosisText_orig LIKE '%cancer%' OR HealthCondition.DiagnosisText_orig LIKE '%neoplasm%')")
CCancer_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisCode_calc LIKE '14%' OR DiagnosisCode_calc LIKE '15%' OR DiagnosisCode_calc LIKE '16%' OR DiagnosisCode_calc LIKE '17%' OR DiagnosisCode_calc LIKE '18%' OR DiagnosisCode_calc LIKE '19%' OR DiagnosisCode_calc LIKE '20%' OR DiagnosisCode_calc LIKE '239%'")

CMechlorethamine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%mechlorethamine%' OR Name_orig LIKE '%mechlorethamine%'")
CMelphalan <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Melphalan%' OR Name_orig LIKE '%Melphalan%'")
CChlorambucil <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Chlorambucil%' OR Name_orig LIKE '%Chlorambucil%'")
CCyclophosphamide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Cyclophosphamide%' OR Name_orig LIKE '%Cyclophosphamide%'")
CIfosfamide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Ifosfamide%' OR Name_orig LIKE '%Ifosfamide%'")
CEstramustine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Estramustine%' OR Name_orig LIKE '%Estramustine%'")
CBusulfan <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Busulfan%' OR Name_orig LIKE '%Busulfan%'")
CDacarbazine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Dacarbazine%' OR Name_orig LIKE '%Dacarbazine%'")
CTemozolomide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Temozolomide%' OR Name_orig LIKE '%Temozolomide%'")
CCarmustine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Carmustine%' OR Name_orig LIKE '%Carmustine%'")
CLomustine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Lomustine%' OR Name_orig LIKE '%Lomustine%'")
CStreptozocin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Streptozocin%' OR Name_orig LIKE '%Streptozocin%'")
CCisplatin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Cisplatin%' OR Name_orig LIKE '%Cisplatin%'")
CCarboplatin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Carboplatin%' OR Name_orig LIKE '%Carboplatin%'")
COxaliplatin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Oxaliplatin%' OR Name_orig LIKE '%Oxaliplatin%'")
CThiotepa <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Thiotepa%' OR Name_orig LIKE '%Thiotepa%'")
CMethotrexate <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Methotrexate%' OR Name_orig LIKE '%Methotrexate%'")
CRaltitrexed <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Raltitrexed%' OR Name_orig LIKE '%Raltitrexed%'")
CPemetrexed <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Pemetrexed%' OR Name_orig LIKE '%Pemetrexed%'")
CCladribine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Cladribine%' OR Name_orig LIKE '%Cladribine%'")
CFludarabine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Fludarabine%' OR Name_orig LIKE '%Fludarabine%'")
CMercaptopurine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Mercaptopurine%' OR Name_orig LIKE '%Mercaptopurine%'")
CThioguanine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Thioguanine%' OR Name_orig LIKE '%Thioguanine%'")
CAzactidine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Azactidine%' OR Name_orig LIKE '%Azactidine%'")
CCapecitabine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Capecitabine%' OR Name_orig LIKE '%Capecitabine%'")
CCytarabine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Cytarabine%' OR Name_orig LIKE '%Cytarabine%'")
CFluorouracil <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Fluorouracil%' OR Name_orig LIKE '%Fluorouracil%'")
CGemcitabine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Gemcitabine%' OR Name_orig LIKE '%Gemcitabine%'")
CBleomycin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Bleomycin%' OR Name_orig LIKE '%Bleomycin%'")
CDactinomycin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Dactinomycin%' OR Name_orig LIKE '%Dactinomycin%'")
CDaunorubicin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Daunorubicin%' OR Name_orig LIKE '%Daunorubicin%'")
CDoxorubicin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Doxorubicin%' OR Name_orig LIKE '%Doxorubicin%'")
CEpirubicin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Epirubicin%' OR Name_orig LIKE '%Epirubicin%'")
CIdarubicin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Idarubicin%' OR Name_orig LIKE '%Idarubicin%'")
CMitomycin <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Mitomycin%' OR Name_orig LIKE '%Mitomycin%'")
CMitoxantrone <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Mitoxantrone%' OR Name_orig LIKE '%Mitoxantrone%'")
CAsparaginase <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Asparaginase%' OR Name_orig LIKE '%Asparaginase%'")
CDocetaxel <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Docetaxel%' OR Name_orig LIKE '%Docetaxel%'")
CPaclitaxel <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Paclitaxel%' OR Name_orig LIKE '%Paclitaxel%'")
CVinblastine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Vinblastine%' OR Name_orig LIKE '%Vinblastine%'")
CVincristine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Vincristine%' OR Name_orig LIKE '%Vincristine%'")
CVinorelbine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Vinorelbine%' OR Name_orig LIKE '%Vinorelbine%'")
CVindesine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Vindesine%' OR Name_orig LIKE '%Vindesine%'")
CIrinotecan <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Irinotecan%' OR Name_orig LIKE '%Irinotecan%'")
CTopotecan <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Topotecan%' OR Name_orig LIKE '%Topotecan%'")
CEtoposide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Etoposide%' OR Name_orig LIKE '%Etoposide%'")
CTeniposide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Teniposide%' OR Name_orig LIKE '%Teniposide%'")
CHydroxyurea <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Hydroxyurea%' OR Name_orig LIKE '%Hydroxyurea%'")
COctreotide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Octreotide%' OR Name_orig LIKE '%Octreotide%'")
CMitotane <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Mitotane%' OR Name_orig LIKE '%Mitotane%'")
CProcarbizineHydrochloride <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Procarbizine Hydrochloride%' OR Name_orig LIKE '%Procarbizine Hydrochloride%'")
CArsenicTrioxide <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Arsenic trioxide%' OR Name_orig LIKE '%Arsenic trioxide%'")
CPofimerSodium <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Pofimer sodium%' OR Name_orig LIKE '%Pofimer sodium%'")
CAltretamine <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Medication WHERE Name_calc LIKE '%Altretamine%' OR Name_orig LIKE '%Altretamine%'")

CCancer_meds <- rbind(CMechlorethamine,CMelphalan)
CCancer_meds <- rbind(CCancer_meds,CChlorambucil)
CCancer_meds <- rbind(CCancer_meds,CCyclophosphamide)
CCancer_meds <- rbind(CCancer_meds,CIfosfamide)
CCancer_meds <- rbind(CCancer_meds,CEstramustine)
CCancer_meds <- rbind(CCancer_meds,CBusulfan)
CCancer_meds <- rbind(CCancer_meds,CDacarbazine)
CCancer_meds <- rbind(CCancer_meds,CTemozolomide)
CCancer_meds <- rbind(CCancer_meds,CCarmustine)
CCancer_meds <- rbind(CCancer_meds,CLomustine)
CCancer_meds <- rbind(CCancer_meds,CStreptozocin)
CCancer_meds <- rbind(CCancer_meds,CCisplatin)
CCancer_meds <- rbind(CCancer_meds,CCarboplatin)
CCancer_meds <- rbind(CCancer_meds,COxaliplatin)
CCancer_meds <- rbind(CCancer_meds,CThiotepa)
CCancer_meds <- rbind(CCancer_meds,CMethotrexate)
CCancer_meds <- rbind(CCancer_meds,CRaltitrexed)
CCancer_meds <- rbind(CCancer_meds,CPemetrexed)
CCancer_meds <- rbind(CCancer_meds,CCladribine)
CCancer_meds <- rbind(CCancer_meds,CFludarabine)
CCancer_meds <- rbind(CCancer_meds,CMercaptopurine)
CCancer_meds <- rbind(CCancer_meds,CThioguanine)
CCancer_meds <- rbind(CCancer_meds,CAzactidine)
CCancer_meds <- rbind(CCancer_meds,CCapecitabine)
CCancer_meds <- rbind(CCancer_meds,CCytarabine)
CCancer_meds <- rbind(CCancer_meds,CFluorouracil)
CCancer_meds <- rbind(CCancer_meds,CGemcitabine)
CCancer_meds <- rbind(CCancer_meds,CBleomycin)
CCancer_meds <- rbind(CCancer_meds,CDactinomycin)
CCancer_meds <- rbind(CCancer_meds,CDaunorubicin)
CCancer_meds <- rbind(CCancer_meds,CDoxorubicin)
CCancer_meds <- rbind(CCancer_meds,CEpirubicin)
CCancer_meds <- rbind(CCancer_meds,CIdarubicin)
CCancer_meds <- rbind(CCancer_meds,CMitomycin)
CCancer_meds <- rbind(CCancer_meds,CMitoxantrone)
CCancer_meds <- rbind(CCancer_meds,CAsparaginase)
CCancer_meds <- rbind(CCancer_meds,CDocetaxel)
CCancer_meds <- rbind(CCancer_meds,CPaclitaxel)
CCancer_meds <- rbind(CCancer_meds,CVinblastine)
CCancer_meds <- rbind(CCancer_meds,CVincristine)
CCancer_meds <- rbind(CCancer_meds,CVinorelbine)
CCancer_meds <- rbind(CCancer_meds,CVindesine)
CCancer_meds <- rbind(CCancer_meds,CIrinotecan)
CCancer_meds <- rbind(CCancer_meds,CTopotecan)
CCancer_meds <- rbind(CCancer_meds,CEtoposide)
CCancer_meds <- rbind(CCancer_meds,CTeniposide)
CCancer_meds <- rbind(CCancer_meds,CHydroxyurea)
CCancer_meds <- rbind(CCancer_meds,COctreotide)
CCancer_meds <- rbind(CCancer_meds,CMitotane)
CCancer_meds <- rbind(CCancer_meds,CProcarbizineHydrochloride)
CCancer_meds <- rbind(CCancer_meds,CArsenicTrioxide)
CCancer_meds <- rbind(CCancer_meds,CPofimerSodium)
CCancer_meds <- rbind(CCancer_meds,CAltretamine)

CCancer_Bill$DateCreated <- as.Date(CCancer_Bill$DateCreated, format = "%Y-%m-%d")
CCancer_HC$DateCreated <- as.Date(CCancer_HC$DateCreated, format = "%Y-%m-%d")
CCancer_HCtext$DateCreated <- as.Date(CCancer_HCtext$DateCreated, format = "%Y-%m-%d")
CCancer_ED$DateCreated <- as.Date(CCancer_ED$DateCreated, format = "%Y-%m-%d")
CCancer_meds$DateCreated <- as.Date(CCancer_meds$DateCreated, format = "%Y-%m-%d")

CCancer_all <- rbind(CCancer_Bill,CCancer_HC)
CCancer_all <- rbind(CCancer_all,CCancer_HCtext)
CCancer_all <- rbind(CCancer_all,CCancer_ED)
CCancer_all <- rbind(CCancer_all,CCancer_meds)

CCancer_all <- CCancer_all[(order(CCancer_all$Patient_ID,CCancer_all$DateCreated)),]
CCancer_all <- dedupe(CCancer_all)
View(CCancer_all)

colnames(CCancer_all) <- c("Patient_ID", "DateOfOnset")
CCancer_all$DateOfOnset <- as.Date(CCancer_all$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,CCancer_all,by="Patient_ID")
CohortWithDisease$Cancer <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL

#Jason's Alcohol as risk factor
Bill_Alc <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisCode_calc,DateCreated,ServiceDate FROM [Billing] WHERE Billing.DiagnosisCodeType_calc LIKE 'ICD9' AND Billing.DiagnosisCode_calc LIKE '303%' OR Billing.DiagnosisCode_calc LIKE '305.0%'")
Bill_Alc$DateCreated <- as.Date(Bill_Alc$DateCreated, format = "%Y-%m-%d")
Bill_Alc$ServiceDate <- as.Date(Bill_Alc$ServiceDate, format = "%Y-%m-%d")
Bill_Alc <- fillin(Bill_Alc,3,4)

HC_Alc <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisCode_calc,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND HealthCondition.DiagnosisCode_calc LIKE '303%' OR HealthCondition.DiagnosisCode_calc LIKE '305.0%'")

HCtext_Alc <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisText_orig,DateOfOnset FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig NOT LIKE '%FAM%' AND HealthCondition.DiagnosisText_orig LIKE '%Alcohol%'")

ED_Alc <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisCode_calc,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '303%'")

EDtext_Alc <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisText_orig,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%Alcohol%' AND EncounterDiagnosis.DiagnosisText_orig NOT LIKE '%FAM%' AND DiagnosisText_orig NOT LIKE '%NO%'")

RF_Alc <- sqlQuery(chan,"SELECT Patient_ID,Name_orig,Value_orig,StartDate FROM [RiskFactor] WHERE RiskFactor.Value_orig LIKE '%Alcohol%'")

Bill_Alc <- dedupe(Bill_Alc)
HC_Alc <- dedupe(HC_Alc)
HCtext_Alc <- dedupe(HCtext_Alc)
ED_Alc <- dedupe(ED_Alc)
EDtext_Alc <- dedupe(EDtext_Alc)
RF_Alc <- dedupe(RF_Alc)

all_Alc <- merge(Bill_Alc, HC_Alc, by = "Patient_ID", all = TRUE)
all_Alc <- merge(all_Alc, HCtext_Alc, by = "Patient_ID", all = TRUE)
all_Alc <- merge(all_Alc, ED_Alc, by = "Patient_ID", all = TRUE)
all_Alc <- merge(all_Alc, EDtext_Alc, by = "Patient_ID", all = TRUE)
all_Alc <- merge(all_Alc, RF_Alc, by = "Patient_ID", all = TRUE)
View(all_Alc)
all_Alc$DiagnosisCode_calc <- NULL
all_Alc$DiagnosisCode_calc.x <- NULL
all_Alc$DiagnosisCode_calc.y <- NULL
all_Alc$DiagnosisText_orig.x <- NULL
all_Alc$DiagnosisText_orig.y <- NULL
all_Alc$DateOfOnset <- NULL
all_Alc$DateCreated.x <- NULL
all_Alc$DateCreated.y <- NULL
all_Alc$Name_orig <- NULL
all_Alc$Value_orig <- NULL
all_Alc$StartDate <- NULL
all_Alc$DateCreated.y <- as.Date(all_Alc$DateCreated.y, format = "%Y-%m-%d")
all_Alc$ServiceDate <- as.Date(all_Alc$ServiceDate, format = "%Y-%m-%d")
all_Alc$DateOfOnset <- ifelse(is.na(all_Alc$DateCreated.y), as.character.Date(all_Alc$ServiceDate), as.character.Date(all_Alc$DateCreated.y))
all_Alc <- all_Alc[, -c(2:4)]

#assign all patientIDs a 1 just for use in the CohortWithDisease as factor, add to whole cohort
colnames(all_Alc) <- c("Patient_ID", "DateOfOnset")
all_Alc$DateOfOnset <- as.Date(all_Alc$DateOfOnset, format = "%Y-%m-%d")
CohortWithDisease <- left_join(CohortWithDisease,all_Alc,by="Patient_ID")
CohortWithDisease$Alcohol <- ifelse(CohortWithDisease$DateCreated>CohortWithDisease$DateOfOnset, 1, 0)
CohortWithDisease$DateOfOnset <- NULL
View(CohortWithDisease)

#finding smokers - not useful as a feature because not enough data from enough people
CSmoker_all <- sqlQuery(chan,"SELECT Patient_ID,DiagnosisText_orig FROM EncounterDiagnosis WHERE (DiagnosisText_orig LIKE '%smoker%' 
                OR DiagnosisText_orig LIKE '%SMOKER' 
                OR DiagnosisText_orig LIKE '%tobacco%' 
                OR DiagnosisCode_calc LIKE '%305%')
                AND DiagnosisText_orig NOT LIKE '%NONSMOKER%'
                AND DiagnosisText_orig NOT LIKE '%NO%'
              ")
View(CSmoker_all)
CSmoker_all <- dedupe(CSmoker_all)
CSmoker_all$Smoker <- 1
CSmoker_all$DiagnosisText_orig <- NULL
CohortWithDisease <- left_join(CohortWithDisease,CSmoker_all,by="Patient_ID")

#work with socioeconomic data
#upload the list of postal codes and their median income in that area for all of Canada
#needed to work with string input from a CSV
install.packages('stringr')
library(readr)
library(stringr)
dataset <- read_csv("~/Erica's Scripts/IncomeDataCSV.csv")
View(dataset)
colnames(dataset) <- c("Postal")
DemoData <- str_split_fixed(dataset$Postal, ";", 2)
colnames(DemoData) <- c("Postal", "Income")
DemoDataNew <- as.data.frame(DemoData)
DemoDataNew$Income <- as.integer(as.character(DemoDataNew$Income))
View(DemoDataNew)

#Get socioeconomic )location) data added to each individual
#use their postal code if they have it, if not, use their doctor's site id postal code
PostalCodes <- sqlQuery(chan,"SELECT Patient_ID,ResidencePostalCode FROM PatientDemographic")
colnames(PostalCodes) <- c("Patient_ID", "Postal")
View(PostalCodes)
#CohortWithDisease$ResidencePostalCode.x <- NULL just a line to delete excess coumns
#here, I'm adding the Postal codes based on patient ID
CohortWithDisease <- left_join(CohortWithDisease,PostalCodes,by="Patient_ID")
#Postal is from the Patient's house address
View(CohortWithDisease)
#some patients don't have postal codes; I'm filling in the data based on the neighbourhood of their family doctor?
SiteLocation <- sqlQuery(chan,"SELECT Site_ID,PostalCode FROM Site")
PatientSiteMatch <- sqlQuery(chan,"SELECT Site_ID, Patient_ID FROM PatientProvider")
#Postal Code is from site  address
#join the sites to each patient
CohortWithDisease <- left_join(CohortWithDisease, PatientSiteMatch, by="Patient_ID")
CohortWithDisease <-left_join(CohortWithDisease, SiteLocation, by='Site_ID')
#update the postal column by filling in from postal codes from the site
#first, you need to convert all blanks in the column to NA
library(stringr)
CohortWithDisease$Postal[] <- lapply(CohortWithDisease$Postal, str_trim)
is.na(CohortWithDisease$Postal) <- CohortWithDisease$Postal==''
#now, you fill in all the NA values in Postal with values from PostalCodes from Sites
CohortWithDisease$Postal <- ifelse(is.na(CohortWithDisease$Postal), as.character(CohortWithDisease$PostalCode), as.character(CohortWithDisease$Postal))
#removing the PostalCodes column from sites, no longer needed
CohortWithDisease$PostalCode <- NULL
#adding the income based on the postal codes and income data
CohortWithDisease <- left_join(CohortWithDisease, DemoDataNew, by="Postal")


#should now have a cohort with sex, BMI, chronic disease predictors
#smoking and income can be added, used them in DELPHI, but not when putting together CPCSSN and final model
