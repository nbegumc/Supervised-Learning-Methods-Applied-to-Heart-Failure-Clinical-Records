### RANDOM FOREST ###
str(data_clean)
library(randomForest)

set.seed(123)
split = sample.split(data_clean$DEATH_EVENT, SplitRatio = 0.75)
data.train = subset(data_clean, split == TRUE)
data.test = subset(data_clean, split == FALSE)
# bagging classifer

set.seed (123)

bag.hf =randomForest(DEATH_EVENT~.,data.train ,
                           mtry=12,ntree=1000, importance =TRUE)
bag.hf

bag.pred=predict (bag.hf ,data.test ,type ="class")
confusionMatrix(bag.pred,data.test[,13],positive = "Yes")
library(vip)
vip::vip(bag.hf, num_features = 15)


# increasing ntree does not lead to overfitting


#random forest classifier

# Fit the model on the training set
set.seed(123)

rf = train(DEATH_EVENT ~., data = data.train, method = "rf",
              trControl = trainControl("cv", number = 10,sampling = "up"),
              metric = performance_metric,tuneGrid=data.frame(.mtry=c(2:6)),
              n.tree=1000,importance = TRUE)



# Best tuning parameter
rf$bestTune #The optimal number of variables sampled at each split is 2
# Final model
rf$finalModel
# Make predictions on the test data
predicted.classes <- rf %>% predict(data.test)
rf
# Compute model accuracy rate

#For a given tree, the out-of-bag (OOB) error is the model error in predicting 
#the data left out of the training set for that tree (P. Bruce and Bruce 2017). 
#OOB is a very straightforward way to estimate the test error of a bagged model, 
#without the need to perform cross-validation or the validation set approach.

# variable importance
importance(rf$finalModel)

par(mfrow=c(1,1))
# Plot MeanDecreaseAccuracy
varImpPlot(rf$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(rf$finalModel, type = 2)
varImp(rf)
confusionMatrix(predicted.classes,data.test[,13],positive='Yes')



#hyperparameters of rf
#1. node size:
#The minimum size for terminal nodes (leaves in the tree). The default is 1 for
#classification and 5 for regression.
#2.maxnodes
#The maximum number of nodes in each decision tree. By default, there is no
#limit and the largest tree will be fit subject to the constraints of nodesize.

#### BOOSTING ###

set.seed(123)
split = sample.split(data$DEATH_EVENT, SplitRatio = 0.75)
data.train = subset(data, split == TRUE)
data.test = subset(data, split == FALSE)
library (gbm) # response variable has to be numeric(1,0)
set.seed (1)
boost.hf =gbm(DEATH_EVENT~.,data=data.train, distribution=
                      "bernoulli",n.trees =5000 , interaction.depth=4)
summary(boost.hf) # relative influence plot and relative inf stats

#We can also produce partial dependence plots for these two variables. These plots
#illustrate the marginal effect of the selected variables on the response after
#integrating out the other variables.

par(mfrow =c(1,2))
plot(boost.hf,i="time")
plot(boost.hf,i="serum_creatinine")

boost.hf =gbm(DEATH_EVENT~.,data=data.train, distribution=
                    "bernoulli",n.trees =5000, interaction.depth =4, shrinkage =0.2,
              verbose =F)
yhat.boost=predict (boost.hf ,newdata =data.test,n.trees =5000)
table(yhat.boost,data.test[,13])

## ADABOOST ###
library(fastAdaboost)
test_real_adaboost <- real_adaboost(DEATH_EVENT~., data.train, 10)
pred <- predict(test_real_adaboost,newdata=data.test)
print(pred$error)
print( table(pred$class,data.test$DEATH_EVENT) )
(46+17)/(46+17+12)


#####
library(adabag)
hf.adaboost <- boosting(DEATH_EVENT ~.,data=data.train,mfinal=10, coeflearn="Breiman",
                             control=rpart.control(maxdepth=5))
hf.adaboost.pred <- predict.boosting(hf.adaboost,newdata=data.test)
hf.adaboost.pred$confusion
hf.adaboost.pred$error
print(hf.adaboost)
#comparing error evolution in training and test set
errorevol(hf.adaboost,newdata=data.train)->evol.train
errorevol(hf.adaboost,newdata=data.test)->evol.test

plot.errorevol(evol.test,evol.train)
