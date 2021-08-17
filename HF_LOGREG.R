
## Logistic Regression ##

data_clean <- as.data.frame(
  cbind(scale(data_clean[,c(1,3,5,7,8,9,12)]),data_clean[,c(2,4,6,10,11,13)]))
str(data_clean)
summary(data_clean)

###Divide the dataset
set.seed(123)
split = sample.split(data_clean$DEATH_EVENT, SplitRatio = 0.75)
data.train = subset(data_clean, split == TRUE)
data.test = subset(data_clean, split == FALSE)
performance_metric="Accuracy"

# Check class balance
table(data_clean$DEATH_EVENT, dnn="Overall")

table(data.train$DEATH_EVENT, dnn="training")

table(data.test$DEATH_EVENT, dnn="testing")



## Logistic Regression

# train the model on training set
lr1 <- train(DEATH_EVENT ~ .,
               data = data.train,
               trControl = trainControl(method = "cv", number = 10),metric = performance_metric,
               method = "glm",
               family=binomial())

lr_pred1 =lr1 %>% predict(data.test)

# Model accuracy
observed.classes <- data.test$DEATH_EVENT
table(Predicted = lr_pred1, Actual = data.test$DEATH_EVENT)
mean(predicted.classes == observed.classes)

summary(lr1)

# Test
confusionMatrix(
  as.factor(lr_pred1),
  as.factor(data.test$DEATH_EVENT),
  positive = "Yes" )

# ROC 
test_roc = roc(data.test$DEATH_EVENT ~ probabilities, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)


######################
# relation with days
p23 <- ggplot(data_clean, aes(x = time , fill = DEATH_EVENT)) +
  geom_histogram() +
  labs(x = "days",
       title = "Death rate by days")
p23
# relation with days ejection_fraction"
p24 <- ggplot(data_clean, aes(x = ejection_fraction , fill = DEATH_EVENT)) +
  geom_histogram() +
  labs(x = "ejection_fraction",
       title = "Death rate by ejection_fraction")
p24
#relation with days serum_creatinine
p25 <- ggplot(data_clean, aes(x = serum_creatinine , fill = DEATH_EVENT)) +
  geom_histogram() +
  labs(x = "serum_creatinine",
       title = "Death rate by serum_creatinine")
p25
figure <- ggarrange(p23,p24,p25,
                    ncol = 2, nrow = 2,
                    common.legend = TRUE)
figure



# Penalized Logistic Regression - LASSO

# Dumy code categorical predictor variables
x <- model.matrix(DEATH_EVENT~., data.train)[,-13]
# Convert the outcome (class) to a numerical variable
y <- ifelse(data.train$DEATH_EVENT == "Yes", 1, 0)

##LASSO

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min

###
lambda <- 10^seq(-5, 5, length = 100)
set.seed(123)
lasso.fit= train(
  DEATH_EVENT ~., data = data.train, method = "glmnet",
  trControl = trainControl("cv", number = 10),metric = performance_metric,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso.fit$finalModel, lasso.fit$bestTune$lambda)
lasso.fit$bestTune$lambda
# Make predictions on the test data
lasso.fit
predicted.classes =lasso.fit%>% predict(data.test)

# Model accuracy
observed.classes <- data.test$DEATH_EVENT
table(Predicted = predicted.classes, Actual = data.test$DEATH_EVENT)

# Test
confusionMatrix(
  as.factor(predicted.classes),
  as.factor(observed.classes),
  positive = "Yes" )


#####################################################
# Logistic regression diagnostics
##Linearity assumption
model <- glm(DEATH_EVENT ~., data = data_clean, 
             family = binomial)
# Predict the probability (p) of death event
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")

# Select only numeric predictors
mydata <- data_clean %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#2. Create the scatter plots:

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Influential values

plot(model, which = 4, id.n = 3)


library(broom)
# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
#Plot the standardized residuals:

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = diabetes), alpha = .5) +
  theme_bw()
#Filter potential influential data points
model.data %>% 
  filter(abs(.std.resid) > 3)
#There is no influential observations in our data.

# Multicollinearity
car::vif(model)

#In our example, 
#there is no collinearity: all variables have a value of VIF well below 5.


##Linear Discriminat Analysis

set.seed(123)
lda= train(
  DEATH_EVENT ~., data = data.train, method = "lda",metric = performance_metric,
  trControl = trainControl("cv", number = 10)
)
# Make predictions on the test data

predicted.classes =lda %>% predict(data.test)

# Model accuracy
observed.classes <- data.test$DEATH_EVENT
table(Predicted = predicted.classes, Actual = data.test$DEATH_EVENT)

# Test
confusionMatrix(
  as.factor(predicted.classes),
  as.factor(observed.classes),
  positive = "Yes" )
plot(lda$finalModel)

##############################################################################

