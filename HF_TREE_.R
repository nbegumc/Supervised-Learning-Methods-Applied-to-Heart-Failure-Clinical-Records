# Decision Trees
library(rpart)

# Build a tree model using GINI index
tree_gini = rpart(DEATH_EVENT ~ .,
                  data = data.train,
                  method = "class",
                  control = rpart.control(cp = 0.01),
                  parms = list(split="gini"))

# Build a tree model using Entropy index
tree_entropy = rpart(DEATH_EVENT ~ .,
                  data = data_oversample,
                  method = "class",
                  control = rpart.control(cp=0.01),
                  parms = list(split="information"))

# Ploting tree
library(rattle)
fancyRpartPlot(tree_gini,
               sub = "Tree based on GINI Index")
fancyRpartPlot(tree_entropy,
               sub = "Tree based on Entropy Index")


#predictions:

pred_gini = predict(tree_gini,
                    data.test,
                    type = "class")


pred_ent = predict(tree_entropy,
                    data.test,
                    type = "class")
# assesing the trees

cm_gini = table(data.test$DEATH_EVENT, pred_gini)
cm_ent = table(data.test$DEATH_EVENT, pred_ent)

cf_gini=confusionMatrix(pred_gini,data.test$DEATH_EVENT, positive = "Yes")# %82
cf_ent=confusionMatrix(pred_ent,data.test$DEATH_EVENT, positive = "Yes") #85.33
cf_gini
cf_ent

## Possible pruning
# To avoid over-fitting  prune trees based on the value of 
# cp that minimizes cross-validated error.

# Cp table for tree_gini
printcp(tree_gini)
printcp(tree_entropy)
# Cross-valiedation error plot for d1
par(mfrow=c(1,2))
plotcp(tree_entropy, sub = "Entropy")
plotcp(tree_gini, sub = "Gini")

# Pruning tree for d1
pruned_gini = prune(tree_gini, cp = tree_gini$cptable[which.min(tree_gini$cptable[,"xerror"]),"CP"])
pruned_ent = prune(tree_entropy, cp = tree_entropy$cptable[which.min(tree_entropy$cptable[,"xerror"]),"CP"])

par(mfrow = c(1,2))
# Ploting trees for Gini 
fancyRpartPlot(tree_gini, 
               sub = "Gini")
fancyRpartPlot(pruned_gini, 
               sub = "Pruned Gini")

# Ploting trees for Entropy 
fancyRpartPlot(tree_entropy, 
               sub = "Entropy")
fancyRpartPlot(pruned_ent, 
               sub = "Pruned Entropy")

# Assesing pruned trees

# Using our model to predict the test set
pred_pruned_g = predict(pruned_gini,
                           data.test,
                           type = "class")

pred_pruned_e = predict(pruned_ent,
                           data.test, 
                           type = "class")

# Construct the confusion matrices
cf_pruned_g=confusionMatrix(pred_pruned_g,data.test$DEATH_EVENT, positive = "Yes")# %82
cf_pruned_e=confusionMatrix(pred_pruned_e,data.test$DEATH_EVENT, positive = "Yes") #81
cf_pruned_g$overall['Accuracy']
cf_pruned_e$overall['Accuracy']

# ROC Curves
## Gini Index
pred_gini = predict(tree_gini,
                    data.test,
                    type = "prob")[, 2]
test_roc = roc(data.test$DEATH_EVENT ~ pred_gini, plot = TRUE, print.auc = TRUE,
               main = "tree")
as.numeric(test_roc$auc)

pred_pruned_g =predict(pruned_gini,
                       data.test,
                       type = "prob")[, 2]
test_roc = roc(data.test$DEATH_EVENT ~ pred_pruned_g, plot = TRUE, print.auc = TRUE,
               main = "pruned_ tree")
as.numeric(test_roc$auc)

## Entropy Index

pred_ent = predict(tree_entropy,
                    data.test,
                    type = "prob")[, 2]
test_roc = roc(data.test$DEATH_EVENT ~ pred_ent, plot = TRUE, print.auc = TRUE,
               main = "tree")
as.numeric(test_roc$auc)

pred_pruned_e =predict(pruned_ent,
                       data.test,
                       type = "prob")[, 2]
test_roc = roc(data.test$DEATH_EVENT ~ pred_pruned_e, plot = TRUE, print.auc = TRUE,
               main = "pruned_ tree")
as.numeric(test_roc$auc)




