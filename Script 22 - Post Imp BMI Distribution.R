#test if BMI imputation in R was successful

library(mice)
library(lattice)
library(VIM)
library(caret)
library(ggplot2)

#CohortWithDiseaseMICE is pre- imputation
View(CohortWithDiseaseMICE)
PreBMI <- CohortWithDiseaseMICE[,c("Age", "BMI")]
View(PreBMI)
PreBMI <- as.matrix(PreBMI)
md.pattern(PreBMI)

#try plotting this kind as suggested by data analytics blogs
aggr_plot <- aggr(PreBMI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(PreBMI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
View(aggr_plot)
#23% missing data

PreImpBMI <- marginplot(PreBMI)
View(PreImpBMI)

#completed dataset is post-imputation
df2 <- complete(tempData, 2)
View(df2)

PostBMI <- df2[,c("Age", "BMI")]
PostBMI <- as.matrix(PostBMI)
View(PostBMI)
PostImpBMI<- marginplot(PostBMI, width=10, height=6)
marginplot(PostBMI)
#not working well, going to do it in Excel
#will need to graph the imputed vs the original - ended up doing it with Excel bc faster

View(PreBMI)
View(PostBMI)

write.csv(PreBMI, "preBMI.csv")
write.csv(PostBMI, "postBMI.csv")

#calculate women and men BMI distribution 

#% of women above BMI = 25
count(CohortWithDiseaseMICE$BMI>25 & CohortWithDiseaseMICE$Sex == "F")
#63805
count(CohortWithDiseaseMICE$Sex == "F")
#144326
63805/144326
#0.44

#% of men above BMI = 25
count(CohortWithDiseaseMICE$BMI>25 & CohortWithDiseaseMICE$Sex == "M")
#94992
count(CohortWithDiseaseMICE$Sex == "M")
#171031
94992/171031
#0.555


