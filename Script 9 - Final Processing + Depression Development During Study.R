#finding the people who got depression during the case of the study
#using DiseaseCase table and the DateOfOnset
#adding this to the CohortWithDisease file
#finding out how many years since their recruitment date

#DiseaseCase table SQL call, ensure to get all the way back from your original start of recruitment period
#because these will be matched to patients based on their individual recruitment date
DepPatientsAfter <-sqlQuery(chan, "SELECT Patient_ID,DateOfOnset FROM DiseaseCase WHERE Disease LIKE '%Depression%' AND DateOfOnset > '2008-12-31'")
View(DepPatientsAfter) #5672 entries
colnames(DepPatientsAfter)[2] <- ("DepressionDiagDate")

#add to existing cohort
CohortWithDisease <- left_join(CohortWithDisease, DepPatientsAfter, by="Patient_ID")
#count number of dates (non NA values) in DepressionDiagDate, = 3613 entries
nrow(CohortWithDisease) - sum(is.na(CohortWithDisease$DepressionDiagDate))
#reformat dates
CohortWithDisease$DepressionDiagDate <- as.Date(CohortWithDisease$DepressionDiagDate, format = "%Y-%m-%d")

#looking at how many people came back via the yearsbetween colulmn
nrow(CohortWithDisease)
count(as.numeric(CohortWithDisease$YearsBetween) >= 4)
count(as.numeric(CohortWithDisease$YearsBetween) >= 3)

#another backup i think i'm just scared i might type something wrong
CohortWithDiseaseAllDatesFactors <- CohortWithDisease
#this will also be useful if we want the data i'm about to modify in:
#a to change something about final encounter dates
#b if we're looking at multimodalities

#need to remove all people who didn't show up again at doctor's office at least 4 years after
CohortWithDiseaseMOD <- subset(CohortWithDisease, !is.na(CohortWithDisease$MostRecentEncounter) & CohortWithDisease$YearsBetween >=4)
View(CohortWithDiseaseMOD)

#if their diagnosis date is before their recruitment date, then they aren't part of our study
#remove them if they got depression before their recruitment date
CohortWithDiseaseMOD$ToRemove <- ifelse(CohortWithDiseaseMOD$DateCreated >= CohortWithDiseaseMOD$DepressionDiagDate, 1, 0 )
CohortWithDiseaseMOD <- subset(CohortWithDiseaseMOD, is.na(CohortWithDiseaseMOD$ToRemove) | CohortWithDiseaseMOD$ToRemove != 1)

#need to turn all 0s into NAs for risk factors in all risk factor columns
#this means that if you got the chronic disease DURING the study you will now be NA
#because we want to know factors at baseline
CohortWithDiseaseMODBACKUP <-CohortWithDiseaseMOD #a backup before i modify all these risk factors
CohortWithDiseaseMOD$CCOPD <- ifelse(CohortWithDiseaseMOD$CCOPD==0, NA, 1)
CohortWithDiseaseMOD$Diabetes <- ifelse(CohortWithDiseaseMOD$Diabetes==0, NA, 1)
CohortWithDiseaseMOD$Epilepsy <- ifelse(CohortWithDiseaseMOD$Epilepsy==0, NA, 1)
CohortWithDiseaseMOD$Anxiety <- ifelse(CohortWithDiseaseMOD$Anxiety==0, NA, 1)
CohortWithDiseaseMOD$CVD <- ifelse(CohortWithDiseaseMOD$CVD==0, NA, 1)
CohortWithDiseaseMOD$RhArthritis <- ifelse(CohortWithDiseaseMOD$RhArthritis==0, NA, 1)
CohortWithDiseaseMOD$Schizo <- ifelse(CohortWithDiseaseMOD$Schizo==0, NA, 1)
CohortWithDiseaseMOD$Cancer <- ifelse(CohortWithDiseaseMOD$Cancer==0, NA, 1)
CohortWithDiseaseMOD$Alcohol <- ifelse(CohortWithDiseaseMOD$Alcohol==0, NA, 1)
CohortWithDiseaseMOD$Smoker <- ifelse(CohortWithDiseaseMOD$Smoker==0, NA, 1)

#add years since diagnosis date
CohortWithDiseaseMOD$YearsSinceDepDiag <- round((CohortWithDiseaseMOD$DepressionDiagDate - CohortWithDiseaseMOD$DateCreated)/365, digits=2)
CohortWithDiseaseMOD$ToRemove <- NULL
