#attempting other models to see if there's a better predictor or higher ROC

df1 <- complete(tempData, 1)

#set up training and test sets
View(df1)
smp_size <- floor(0.75 * nrow(df1))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)

train <- df1[train_ind, ]
test <- df1[-train_ind, ]
View(train)
View(test)

TestOutcomes <- test$GotDepression 
View(TestOutcomes)
testNoOutcomes <- test
testNoOutcomes$GotDepression <- NULL
View(testNoOutcomes)


#look here https://stats.stackexchange.com/questions/2234/alternatives-to-logistic-regression-in-r

install.packages(c("gam", "randomForest", "gbm", "rpart", "arm"))
library(randomForest)
library(gbm)
library(rpart)
library(arm)
library(gam)

#comparison model, GLM
#model <- glm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=test, family=binomial(link='logit'))
#AUC ~~ 0.59 

#GAMs
modelGAM <- gam(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=train, family=binomial(link='logit'))
View(modelGAM)
predGAM <- predict(modelGAM,newdata=testNoOutcomes, type="response")
View(predGAM)
OutcomePredGAM<-cbind(as.numeric(as.character(TestOutcomes)), predGAM)
colnames(OutcomePredGAM) <- c("Outcome", "Pred")
OutcomePredGAM <-as.data.frame(OutcomePredGAM)
View(OutcomePredGAM)
#GAM ROC = 0.593
roc(OutcomePredGAM$Outcome, OutcomePredGAM$Pred)

#Decision trees - rpart
modelTree <- rpart(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=train, method="class", control=rpart.control(minsplit=2, cp=0))
View(modelTree)
printcp(modelTree)
plotcp(modelTree)
plot(modelTree)
predTree <- predict(modelTree,newdata=testNoOutcomes, type="prob")
View(predTree)
predTree <- as.data.frame(predTree)
colnames(predTree) <- c("Zero", "Pred")
predTree$Zero <- NULL
OutcomePredTree<-cbind(as.numeric(as.character(TestOutcomes)), predTree)
colnames(OutcomePredTree) <- c("Outcome", "Pred")
OutcomePredTree <-as.data.frame(OutcomePredTree)
View(OutcomePredTree)
#ROC = 0.5404
roc(OutcomePredTree$Outcome, OutcomePredTree$Pred)

#randomForest
modelRandomForest <- randomForest(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, data=train, ntree = 500)
View(modelRandomForest)
predRandomForest <- predict(modelRandomForest,newdata=testNoOutcomes, type="prob")
predRandomForest <- as.data.frame(predRandomForest)
colnames(predRandomForest) <- c("Zero", "Pred")
predRandomForest$Zero <- NULL
OutcomePredRF<-cbind(as.numeric(as.character(TestOutcomes)), predRandomForest)
colnames(OutcomePredRF) <- c("Outcome", "Pred")
OutcomePredRF <-as.data.frame(OutcomePredRF)
View(OutcomePredRF)
#ROC for RandomForest = 0.531
roc(OutcomePredRF$Outcome, OutcomePredRF$Pred)

#gradient boosting - gbm
modelGBM <- gbm(GotDepression~BMI+Age+Sex+COPD+Diabetes+Epilepsy+Anxiety+CVD+RhArthritis+Schizo+Cancer+Alcohol, distribution = "bernoulli", data = train,verbose = FALSE)
View(modelGBM)
#not sure how to run
predGBM <- predict(modelGBM,newdata=testNoOutcomes, type="prob")
