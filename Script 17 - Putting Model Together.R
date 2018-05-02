#data is in tempData
#huge shoutout to Jason for this code; Jason you're awesome! 

#create empty variable
model_list <- NULL

for (i in 1:tempData$m) {
  #Extract imputed dataset i
  df <- complete(tempData, i)
  #Train the model and store it in the model list.
  model <- glm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=test, family=binomial(link='logit'))
  model_list[[i]] <- model
}
View(model_list)

#Convert the model list into a mira object (from the mice package), then perform the pool function to pool the models.
mira.model_list <- as.mira(model_list)
pooled.model_list <- pool(mira.model_list)

#Creates a copy of the last model created above, then alters the coefficients to match those of the pooled model.
pooled.model <- model
pooled.model$coefficients[1] <- pooled.model_list$qbar[[1]]
pooled.model$coefficients[2] <- pooled.model_list$qbar[[2]]
pooled.model$coefficients[3] <- pooled.model_list$qbar[[3]]
pooled.model$coefficients[4] <- pooled.model_list$qbar[[4]]
pooled.model$coefficients[5] <- pooled.model_list$qbar[[5]]
pooled.model$coefficients[6] <- pooled.model_list$qbar[[6]]

View(pooled.model) #type is a large GLM that can now be applied to a data set

#get each  dataset out of MICE
df1 <- complete(tempData, 1)
df2 <- complete(tempData, 2)
df3 <- complete(tempData, 3)
df4 <- complete(tempData, 4)
df5 <- complete(tempData, 5)
summary(pooled.model)

#remove the GotDepression column, and run the pooled model
View(df2)
#df2$COPD<-as.numeric(df2$COPD)
Outcome1 <- df2$GotDepression
View(Outcome1)
df2$GotDepression <- NULL
df2<-as.data.frame(df2)
#predict using the model 
Pred1 <- predict(pooled.model,newdata=df2, type="response")
View(Pred1)
OutcomePred<-cbind(as.numeric(as.character(Outcome1)), Pred1)
#combine outcome and pred
colnames(OutcomePred) <- c("Outcome", "Pred")
OutcomePred <-as.data.frame(OutcomePred)
View(OutcomePred)

#find calibration
OutcomePred$Outcome <- as.factor(OutcomePred$Outcome)
cal <- calibration(OutcomePred$Outcome~(1-OutcomePred$Pred))
plot(cal, main="Calibration of Linear Regression Model for Depression", xlab = "Predicted Class", ylab= "Observed Event Class")
View(cal)

library(pROC)

#calculate ROC on entire data frame - did not work
roc <- roc(OutcomePred$Outcome, OutcomePred$Pred)
View(roc)
plot(roc(OutcomePred$Outcome, OutcomePred$Pred, direction="<"),
     col="blue", xlab="Specificity - False Positive Rate", ylab = "Sensitivity - True Positive Rate", lwd=3, main="ROC Curve Test")

#split up OutcomePred dataframe, then try to calculate ROC again
OutcomePredShort <- OutcomePred[sample(1:nrow(OutcomePred), 100000, replace=FALSE),]
View(OutcomePredShort)
roc <- roc(OutcomePredShort$Outcome, OutcomePredShort$Pred, ci=TRUE, smooth = TRUE)
plot(roc(OutcomePredShort$Outcome, 1 - OutcomePredShort$Pred, direction="<"),
     col="blue", xlab="Specificity - False Positive Rate", ylab = "Sensitivity - True Positive Rate", lwd=3, main="ROC Curve Test")
View(roc)
