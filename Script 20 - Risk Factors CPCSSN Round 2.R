#mostly copied from Script 13
View(PatientIntCohort)

PatientIntCohort$Age <- 2018-PatientIntCohort$BirthYear

PatientIntCohortBACKUP <- PatientIntCohort

#BMI
colnames(CBMI) <- c("Patient_ID","BMI")
PatientIntCohort <- left_join(PatientIntCohort,CBMI,by="Patient_ID")
PatientIntCohort <- dedupe(PatientIntCohort)

#so here's what happened
#I based chronic disease definitions on JUST diseasecase table
#Need to go back and get more than that! from billing and healthcondition tables

#COPD REDONE
CCOPD_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '491%'")
CCOPD_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '491%'")
CCOPD_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig LIKE '%COPD%' OR HealthCondition.DiagnosisText_orig LIKE 'CHRONIC BRONCHITIS'")
CCOPD_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisCode_calc LIKE '491%'")
CCOPD_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%COPD%'")

CCOPD_Bill$DateCreated <- as.Date(CCOPD_Bill$DateCreated, format = "%Y-%m-%d")
CCOPD_HC$DateCreated <- as.Date(CCOPD_HC$DateCreated, format = "%Y-%m-%d")
CCOPD_HCtext$DateCreated <- as.Date(CCOPD_HCtext$DateCreated, format = "%Y-%m-%d")
CCOPD_ED$DateCreated <- as.Date(CCOPD_ED$DateCreated, format = "%Y-%m-%d")
CCOPD_EDtext$DateCreated <- as.Date(CCOPD_EDtext$DateCreated, format = "%Y-%m-%d")

CCOPD_all <- rbind(CCOPD_Bill,CCOPD_HC)
CCOPD_all <- rbind(CCOPD_all,CCOPD_HCtext)
CCOPD_all <- rbind(CCOPD_all,CCOPD_ED)
CCOPD_all <- rbind(CCOPD_all,CCOPD_EDtext)
colnames(CCOPD) <- c("Patient_ID", "DateCreated")
CCOPD_all <- rbind(CCOPD_all, CCOPD)

CCOPD_all <- CCOPD_all[(order(CCOPD_all$Patient_ID,CCOPD_all$DateCreated)),]
CCOPD_all <- dedupe(CCOPD_all)
colnames(CCOPD_all) <- c("Patient_ID", "DateOfOnset")

PatientIntCohort <- left_join(PatientIntCohort,CCOPD_all,by="Patient_ID")
PatientIntCohort$DateOfOnset <- as.Date(PatientIntCohort$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort$COPD <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL
count <- subset(PatientIntCohort, is.na(PatientIntCohort$COPD)==FALSE)
nrow(count)

#Diabetes
res <- sqlQuery(chan,"SELECT TOP 10 * FROM Billing WHERE DiagnosisCode_calc LIKE '250%' ")
View(res)

CDiabetes_Bill <- sqlQuery(chan, "SELECT Patient_ID,DateCreated FROM Billing WHERE DiagnosisCode_calc LIKE '250%'")
CDiabetes_HC <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '250%'")
CDiabetes_HCtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [HealthCondition] WHERE HealthCondition.DiagnosisText_orig LIKE '%DIABETES%'")
CDiabetes_ED <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE DiagnosisCode_calc LIKE '250%'")
CDiabetes_EDtext <- sqlQuery(chan,"SELECT Patient_ID,DateCreated FROM [EncounterDiagnosis] WHERE EncounterDiagnosis.DiagnosisText_orig LIKE '%DIABETES%'")

CDiabetes_Bill$DateCreated <- as.Date(CDiabetes_Bill$DateCreated, format = "%Y-%m-%d")
CDiabetes_HC$DateCreated <- as.Date(CDiabetes_HC$DateCreated, format = "%Y-%m-%d")
CDiabetes_HCtext$DateCreated <- as.Date(CDiabetes_HCtext$DateCreated, format = "%Y-%m-%d")
CDiabetes_ED$DateCreated <- as.Date(CDiabetes_ED$DateCreated, format = "%Y-%m-%d")
CDiabetes_EDtext$DateCreated <- as.Date(CDiabetes_EDtext$DateCreated, format = "%Y-%m-%d")

CDiabetes_all <- rbind(CDiabetes_Bill,CDiabetes_HC)
CDiabetes_all <- rbind(CDiabetes_all,CDiabetes_HCtext)
CDiabetes_all <- rbind(CDiabetes_all,CDiabetes_ED)
CDiabetes_all <- rbind(CDiabetes_all,CDiabetes_EDtext)
colnames(CDiabetes) <- c("Patient_ID", "DateCreated")
CDiabetes_all <- rbind(CDiabetes_all, CDiabetes) #may need to change headings

CDiabetes_all <- CDiabetes_all[(order(CDiabetes_all$Patient_ID,CDiabetes_all$DateCreated)),]
CDiabetes_all <- dedupe(CDiabetes_all)
View(CDiabetes_all)

colnames(CDiabetes_all) <- c("Patient_ID", "DateOfOnset")
CDiabetes_all$DateOfOnset <- as.Date(CDiabetes_all$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CDiabetes_all,by="Patient_ID")
PatientIntCohort$Diabetes <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#re-reun Script 13 code for rest
View(PatientIntCohort)
#re-run Script 14 code for other parts
