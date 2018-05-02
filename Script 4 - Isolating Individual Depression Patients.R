#see script 3 note for what DepMaster is coming into this script
#everyone who already HAS depression is not part of our cohort
#therefore, we need to remove everyone who has a diagnosis before the end of recruitment
#recruitment period is Jan 1 2009 to Dec 31 2010, following people Jan 2011 - Jan 2016
#so i will put all these patient IDs in a dataframe so they can be removed from cohort later
#via patient ID

#first, I need to remove duplicates; since I've pulled data from multiple tables
#then, need to find if they were diagnosed with depression BEFORE Jan 1 2009
#if so, they will not be allowed into our cohort
View(newDepMaster)

newDepMaster <- DepMaster[(order(DepMaster$Patient_ID,DepMaster$DateCreated, decreasing=F)),]

library(dplyr)
#jason built and showed me for to use this function, essentially order them first based on what you want 
#then run dedupe, ta-da no more duplicates
dedupe <- function(df) {
  #returns a dataframe where only the first duplicate is retained
  df <- subset(df,!duplicated(df$Patient_ID))
  return(df)
}

#now returns only the EARLIEST date they were diagnosed with depression , now 10433 entries
newDepMaster <- dedupe(newDepMaster)
View(newDepMaster)

#take out everyone from BEFORE our first recruitment date, definitely had depression 
DepDiagnosed <- subset(newDepMaster, DateCreated < "2009-01-01")
View(DepDiagnosed)

#DepDiagnosed has 2992 patients who were diagnosed with depression (*interacted - read prev note)
#before the Jan 1 2009 recruitment date where we start tracking our patients
#these patients need to be removed from the cohort
