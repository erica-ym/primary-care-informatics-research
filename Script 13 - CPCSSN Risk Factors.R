#goal of this script is to add all risk factors to PatientIntCohort
#GO BACK TO SCRIPT 6 - that's where the original code is 

#backup patientintcohort
View(PatientIntCohort)

PatientIntCohort$Age <- 2018-PatientIntCohort$BirthYear

PatientIntCohortBACKUP <- PatientIntCohort
#PatientIntCohort <- PatientIntCohortBACKUP

#BMI
colnames(CBMI) <- c("Patient_ID","BMI")
PatientIntCohort <- left_join(PatientIntCohort,CBMI,by="Patient_ID")
PatientIntCohort <- dedupe(PatientIntCohort)

#COPD
PatientIntCohort <- left_join(PatientIntCohort,CCOPD,by="Patient_ID")
PatientIntCohort$DateOfOnset <- as.Date(PatientIntCohort$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort$COPD <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL
#count <- subset(PatientIntCohort, is.na(PatientIntCohort$COPD)==FALSE)
#17000 patients with COPD -- does that make sense

#Diabetes
CDiabetes$DateOfOnset <- as.Date(CDiabetes$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CDiabetes,by="Patient_ID")
PatientIntCohort$Diabetes <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL
#count <- subset(PatientIntCohort, is.na(PatientIntCohort$Diabetes)==FALSE)
#41000 patients with Diabetes

#Epilepsy
CEpilepsy$DateOfOnset <- as.Date(CEpilepsy$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CEpilepsy,by="Patient_ID")
PatientIntCohort$Epilepsy <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL
#count <- subset(PatientIntCohort, is.na(PatientIntCohort$Epilepsy)==FALSE)
#4000 epilepsy patients

#Anxiety
CAnxiety_all$DateOfOnset <- as.Date(CAnxiety_all$DateOfOnset, format = "%Y-%m-%d")
colnames(CAnxiety_all) <- c("Patient_ID", "DateOfOnset")
PatientIntCohort <- left_join(PatientIntCohort,CAnxiety_all,by="Patient_ID")
PatientIntCohort$Anxiety <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#CVD
CCVD_all$DateOfOnset <- as.Date(CCVD_all$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CCVD_all,by="Patient_ID")
PatientIntCohort$CVD <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#Rhumatoid Arthritis
colnames(CRA_all) <- c("Patient_ID", "DateOfOnset")
CRA_all$DateOfOnset <- as.Date(CRA_all$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CRA_all,by="Patient_ID")
PatientIntCohort$RhArthritis <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#Schizophrenia
CSchizophrenia_all$DateOfOnset <- as.Date(CSchizophrenia_all$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CSchizophrenia_all,by="Patient_ID")
PatientIntCohort$Schizo <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#Cancer
colnames(CCancer_all) <- c("Patient_ID", "DateOfOnset")
CCancer_all$DateOfOnset <- as.Date(CCancer_all$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,CCancer_all,by="Patient_ID")
PatientIntCohort$Cancer <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL
#count <- subset(PatientIntCohort, is.na(PatientIntCohort$Cancer)==FALSE)
#View(count)

#Alcohol 
all_Alc$DateOfOnset <- as.Date(all_Alc$DateOfOnset, format = "%Y-%m-%d")
PatientIntCohort <- left_join(PatientIntCohort,all_Alc,by="Patient_ID")
PatientIntCohort$Alcohol <- ifelse(PatientIntCohort$DateCreated>PatientIntCohort$DateOfOnset, 1, 0)
PatientIntCohort$DateOfOnset <- NULL

#socioeconomic data
PatientIntCohort$Postal <- NULL
PostalCodes <- sqlQuery(chan,"SELECT Patient_ID,ResidencePostalCode FROM PatientDemographic_deid")
colnames(PostalCodes) <- c("Patient_ID", "Postal")
View(PostalCodes)
View(PatientIntCohort)

#here, I'm adding the Postal codes based on patient ID
PatientIntCohort <- left_join(PatientIntCohort,PostalCodes,by="Patient_ID")
#Postal is from the Patient's house address
#some patients don't have postal codes; I'm filling in the data based on the neighbourhood of their family doctor?
SiteLocation <- sqlQuery(chan,"SELECT Site_ID,PostalCode FROM Site")
View(SiteLocation)
PatientSiteMatch <- sqlQuery(chan,"SELECT Site_ID, Patient_ID FROM PatientProvider")
PatientIntCohort <- left_join(PatientIntCohort,PatientSiteMatch,by="Patient_ID")
PatientIntCohort <- left_join(PatientIntCohort,SiteLocation,by="Site_ID")
PatientIntCohort$PostalCodeFinal <- ifelse(is.na(PatientIntCohort$Postal), as.character(PatientIntCohort$PostalCode), as.character(PatientIntCohort$Postal))
#adding the income based on the postal codes and income data
DemoDataNew <- read.csv(file="IncomeData.csv", header=TRUE, sep=";")
colnames(DemoDataNew) <- c("PostalCodeFinal", "Income")
View(DemoDataNew)
PatientIntCohort <- left_join(PatientIntCohort, DemoDataNew, by="PostalCodeFinal")
PatientIntCohort$Postal <- NULL
PatientIntCohort$PostalCode <- NULL

