#add final post-study encounter dates for each patient based on their most recent interaction
#first part of this collection is from Script 8

View(MedInt)
AllPatientIntFinalDates <- rbind(BillingInt,EncounterInt)
#AllPatientIntFinalDates <- rbind(AllPatientIntFinalDates,BillingInt)
#AllPatientIntFinalDates <- rbind(AllPatientIntFinalDates,ReferralInt)
#AllPatientIntFinalDates <- rbind(AllPatientIntFinalDates,ProcedureInt)
#AllPatientIntFinalDates <- rbind(AllPatientIntFinalDates,LabInt)
#AllPatientIntFinalDates <- rbind(AllPatientIntFinalDates,ExamInt)
View(AllPatientIntFinalDates)

#sorts the AllPatientInt with all interactions by date and patient ID so that only the latest date remains
AllPatientIntFinalDates <- AllPatientIntFinalDates[(order(AllPatientIntFinalDates$Patient_ID,AllPatientIntFinalDates$DateCreated, decreasing=T)),]
#remove duplicates using pre-built function
AllPatientIntFinal <- dedupe(AllPatientIntFinalDates)
colnames(AllPatientIntFinal) <- c("Patient_ID", "MostRecentEncounter")
View(AllPatientIntFinal)

#merge dates with PatientIntCohort so you have an ending date for each person
PatientIntCohort <- left_join(PatientIntCohort,AllPatientIntFinal,by="Patient_ID")
View(PatientIntCohort)

PatientIntCohort$MostRecentEncounter <- as.Date(PatientIntCohort$MostRecentEncounter, format = "%Y-%m-%d")
PatientIntCohort$YearsBetween <- round((PatientIntCohort$MostRecentEncounter - PatientIntCohort$DateCreated)/365, digits=2)

#looking at how many people came back via the yearsbetween colulmn
nrow(PatientIntCohort) #414116
count(as.numeric(PatientIntCohort$YearsBetween) >= 4) #322414 people ish
count(as.numeric(PatientIntCohort$YearsBetween) >= 5) #297948 people ish

#PatientIntCohortFinal is where I remove those people who didn't show up again
#need to remove all people who didn't show up again at doctor's office at least 4 years after
PatientIntCohortFinal <- subset(PatientIntCohort, !is.na(PatientIntCohort$MostRecentEncounter) & PatientIntCohort$YearsBetween >=4)
View(PatientIntCohortFinal)

