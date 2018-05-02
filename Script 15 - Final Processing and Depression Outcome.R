#prepping for MICE, final processing, and adding depression outcomes

#DiseaseCase table SQL call, ensure to get all the way back from start date because these will be matched to patients based on their individual recruitment date
DepDiseaseCaseNew <- sqlQuery(chan,"SELECT Patient_ID, DateOfOnset FROM [DiseaseCase] WHERE Disease LIKE '%Depression%' AND DateOfOnset > '2010-12-31'")
DepHealthCodeNew <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DateOfOnset > '2010-12-31' AND DiagnosisCodeType_calc LIKE 'ICD9' AND DiagnosisCode_calc LIKE '296%' OR DiagnosisCode_calc LIKE '311%'")
DepHealthTextNew <- sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM HealthCondition WHERE DateOfOnset > '2010-12-31' AND DiagnosisText_orig LIKE '%depress%' OR DiagnosisText_calc LIKE '%depress'")

#combine all data into one large dataframe
DepPatientsAfter <- rbind(DepHealthCodeNew, DepHealthTextNew)
DepPatientsAfter <- rbind(DepPatientsAfter, DepDiseaseCaseNew)
colnames(DepPatientsAfter) <- c("Patient_ID", "DateCreated")
View(DepPatientsAfter)

DepPatientsAfter$DateCreated <- as.Date(DepPatientsAfter$DateCreated, format = "%Y-%m-%d")
DepPatientsAfter<- DepPatientsAfter[(order(DepPatientsAfter$Patient_ID,DepPatientsAfter$DateCreated)),]
DepPatientsAfterBackup <- DepPatientsAfter
DepPatientsAfter <-dedupe(DepPatientsAfter)

#DepPatientsAfter <-DepPatientsAfterBackup

View(DepPatientsAfter) #157101 entries
colnames(DepPatientsAfter)[2] <- ("DepressionDiagDate")
#make sure it's only dates that are THERE and dates that aren't past the cohort date
DepPatientsAfter$DepressionDiagDate <- as.Date(DepPatientsAfter$DepressionDiagDate, format = "%Y-%m-%d")
DepPatientsAfter <- subset(DepPatientsAfter, is.na(DepPatientsAfter$DepressionDiagDate) == FALSE)
View(DepPatientsAfter) #144023 entries

#add to existing cohort
PatientIntCohortFinal <- left_join(PatientIntCohortFinal, DepPatientsAfter, by="Patient_ID")
#reformat dates
PatientIntCohortFinal$DepressionDiagDate <- as.Date(PatientIntCohortFinal$DepressionDiagDate, format = "%Y-%m-%d")

#if their diagnosis date is before their recruitment date, then they aren't part of our study
#remove them if they got depression before their recruitment date
PatientIntCohortFinal$ToRemove <- ifelse(PatientIntCohortFinal$DateCreated >= PatientIntCohortFinal$DepressionDiagDate | PatientIntCohortFinal$DepressionDiagDate < "2010-12-31", 1, 0 )
PatientIntCohortFinal <- subset(PatientIntCohortFinal, is.na(PatientIntCohortFinal$ToRemove) | PatientIntCohortFinal$ToRemove != 1)
PatientIntCohortFinal <- subset(PatientIntCohortFinal,is.na(PatientIntCohortFinal$DepressionDiagDate) | PatientIntCohortFinal$DepressionDiagDate < "2016-01-01")
count(as.numeric(is.na(PatientIntCohortFinal$DepressionDiagDate))) #39193 patients

#add years since diagnosis date
PatientIntCohortFinal$YearsSinceDepDiag <- round((PatientIntCohortFinal$DepressionDiagDate - PatientIntCohortFinal$DateCreated)/365, digits=2)
PatientIntCohortFinal$ToRemove <- NULL

#need to turn all 0s into NAs for risk factors in all risk factor columns
PatientIntCohortFinalBACKUP <-PatientIntCohortFinal #a backup before i modify all these risk factors
PatientIntCohortFinal$COPD <- ifelse(PatientIntCohortFinal$COPD==0, NA, 1)
PatientIntCohortFinal$Diabetes <- ifelse(PatientIntCohortFinal$Diabetes==0, NA, 1)
PatientIntCohortFinal$Epilepsy <- ifelse(PatientIntCohortFinal$Epilepsy==0, NA, 1)
PatientIntCohortFinal$Anxiety <- ifelse(PatientIntCohortFinal$Anxiety==0, NA, 1)
PatientIntCohortFinal$CVD <- ifelse(PatientIntCohortFinal$CVD==0, NA, 1)
PatientIntCohortFinal$RhArthritis <- ifelse(PatientIntCohortFinal$RhArthritis==0, NA, 1)
PatientIntCohortFinal$Schizo <- ifelse(PatientIntCohortFinal$Schizo==0, NA, 1)
PatientIntCohortFinal$Cancer <- ifelse(PatientIntCohortFinal$Cancer==0, NA, 1)
PatientIntCohortFinal$Alcohol <- ifelse(PatientIntCohortFinal$Alcohol==0, NA, 1)

View(PatientIntCohortFinal)


