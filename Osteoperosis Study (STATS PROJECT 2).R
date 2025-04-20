#install.packages("aplore3")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("car")
#install.packages("pROC")
#install.packages("glmnet")

library(aplore3)
library(caret)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(car)
library(pROC)
library(glmnet)

#?glow_bonemed
data("glow_bonemed")

# ===============================
# OBJECTIVE 1
# ===============================

# Converting any relevant variables to the proper data types
glow_bonemed$priorfrac <- as.factor(glow_bonemed$priorfrac)
glow_bonemed$premeno <- as.factor(glow_bonemed$premeno)
glow_bonemed$momfrac <- as.factor(glow_bonemed$momfrac)
glow_bonemed$armassist <- as.factor(glow_bonemed$armassist)
glow_bonemed$bonetreat <- as.factor(glow_bonemed$bonetreat)
glow_bonemed$smoke <- as.factor(glow_bonemed$smoke)
glow_bonemed$raterisk <- as.factor(glow_bonemed$raterisk)
glow_bonemed$fracture <- as.factor(glow_bonemed$fracture)

# Check for missing values (NAs and "N/A")
missing_data <- glow_bonemed %>%
  filter(if_any(everything(), is.na) | if_any(everything(), ~ . == "N/A"))
missing_data

missing_counts <- colSums(is.na(glow_bonemed) | glow_bonemed == "N/A")
missing_counts <- missing_counts[missing_counts > 0]
missing_counts

# Basic stats
str(glow_bonemed)
summary(glow_bonemed)
is.na(glow_bonemed)
colSums(is.na(glow_bonemed))

set.seed(123)

# Creating a 70-30 split (the target variable here is 'fracture')
trainIndex <- createDataPartition(glow_bonemed$fracture, p = 0.7, list = FALSE)
trainData <- glow_bonemed[trainIndex, ]
testData <- glow_bonemed[-trainIndex, ]

table(trainData$fracture)

# We removed the sub_id, site_id, and phy_id variables because they don't apply to the actual dataset (just identifiers)
# We also removed the bonemed_fu variable since the description of the glow_bonemed dataset said it was a follow up (indicating that it's after the fracture)

# Continuous (Numeric) Variables: age, weight, height, bmi, fracscore
continuous_vars <- c("age", "weight", "height", "bmi", "fracscore")

# Ensures numeric
for (var in continuous_vars) {
  trainData[[var]] <- as.numeric(trainData[[var]])
}

# Saves all the plots in this 'pdf()...dev.off()' block (>O_O)>
pdf("my_plots.pdf", width = 7, height = 5)

for (var in continuous_vars) {
  p <- ggplot(trainData, aes_string(x = var, fill = "factor(fracture)")) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    labs(title = paste(var, "Distribution by Fracture"), fill = "Fracture")
  print(p)
  
  p2 <- ggplot(trainData, aes_string(x = "factor(fracture)", y = var)) +
    geom_boxplot() +
    labs(title = paste(var, "by Fracture Outcome"), x = "Fracture", y = var)
  print(p2)
}

# Binary (Factor) Variables: priorfrac, premeno, momfrac, armassist, smoke, bonemed, bonetreat
binary_vars <- c("priorfrac", "premeno", "momfrac", "armassist", "smoke", "bonemed", "bonetreat")

for (var in binary_vars) {
  p <- ggplot(trainData, aes_string(x = paste0("factor(", var, ")"), fill = "factor(fracture)")) +
    geom_bar(position = "fill") +
    labs(x = var, y = "Proportion", fill = "Fracture", title = paste(var, "by Fracture Outcome"))
  print(p)
}

# Ordinal Variable: raterisk
ggplot(trainData, aes(x = factor(raterisk), fill = factor(fracture))) +
  geom_bar(position = "fill") +
  labs(x = "Risk Rating", y = "Proportion", fill = "Fracture", title = "Self-Reported Risk by Fracture Outcome")

numeric_vars <- glow_bonemed %>%
  select(age, weight, height, bmi, fracscore)

# Creating a pairwise plot matrix with all the numeric variables (as shown above) 
ggpairs(numeric_vars)

dev.off()

# Fitting the logistic regression model
logit_model <- glm(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat + raterisk + bonemed + armassist,
                   data = glow_bonemed, family = binomial)

summary(logit_model)
confint(logit_model)

# Odds ratios
exp(coef(logit_model))

# Odds ratios with confidence intervals
exp(cbind(OR = coef(logit_model), confint(logit_model)))

# AIC for model comparison
AIC(logit_model)

# VIF
vif(logit_model)


# ===============================
# OBJECTIVE 2
# ===============================

# Reduced Logistic Regression Model: Compared variables from first model (Obj. 1) using p-values and removed the variables that had high p-values and were not significant

set.seed(123)

# Creating a 70-30 split (the target variable here is 'fracture')
trainIndex2 <- createDataPartition(glow_bonemed$fracture, p = 0.7, list = FALSE)
trainData2 <- glow_bonemed[trainIndex2, ]
testData2 <- glow_bonemed[-trainIndex2, ]

# Fitting the Reduced Logistic Regression Model (these were the variables that had a strong significance)
reduced_model <- glm(fracture ~ fracscore + raterisk + bonemed,
                     data = trainData2, family = binomial)

summary(reduced_model)
vif(reduced_model)


# Predicting on the Validation Set
# type = "response" tells R to return predicted probabilities of fracture = 1 (Yes)
valid_probs <- predict(reduced_model, newdata = testData2, type = "response")

# Converts the predicted probabilities into binary class predictions
# If valid_probs > 0.5, it assigns 1 (predicts fracture = Yes), otherwise, it assigns 0 (predicts fracture = No)
valid_preds <- ifelse(valid_probs > 0.5, 1, 0)

# Confusion Matrix
# Make sure both vectors are factors with same levels
confusionMatrix(
  factor(valid_preds, levels = c(0, 1), labels = c("No", "Yes")),
  testData2$fracture,
  positive = "Yes"
)

# ROC Curve & AUC
roc_obj <- roc(testData2$fracture, valid_probs)
plot(roc_obj, main = "ROC Curve for Reduced Logistic Model")
auc(roc_obj)


# Two types of Feature Selection are being implemented below

# -----------------------------------
# Stepwise AIC - Feature Selection
# -----------------------------------

# Full model as the starting point
full_model <- glm(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat + raterisk + bonemed + armassist,
                  data = trainData2, family = binomial)

# Stepwise selection using both directions (forward + backward)
step_model <- step(full_model, direction = "both")

summary(step_model)

# Viewing selected variables
formula(step_model)
# Line above yielded this model: fracture ~ fracscore + priorfrac + bonetreat + raterisk + bonemed

# Evaluating the model on the test data

# Fitting stepwise-selected model
step_model <- glm(fracture ~ fracscore + priorfrac + bonetreat + raterisk + bonemed,
                  data = trainData2, family = binomial)

# Predicting on the test data
step_probs <- predict(step_model, newdata = testData2, type = "response")
step_preds <- ifelse(step_probs > 0.5, 1, 0)

# Evaluating
confusionMatrix(
  factor(step_preds, levels = c(0, 1), labels = c("No", "Yes")),
  testData2$fracture,
  positive = "Yes"
)

# ROC and AUC
step_roc <- roc(testData2$fracture, step_probs)
plot(step_roc, main = "ROC Curve: Stepwise Model")
cat("AUC (Stepwise): ", auc(step_roc), "\n\n")

# -----------------------------------
# LASSO - Feature Selection
# -----------------------------------

# Preparing the data
# Converting factors to numeric dummy variables for glmnet
# Note: model.matrix automatically handles factor encoding
x_train <- model.matrix(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat + raterisk + bonemed + armassist,
                        data = trainData2)[, -1]  # Remove intercept column
y_train <- as.numeric(trainData2$fracture) - 1     # Convert 'No'/'Yes' to 0/1

# Fitting Lasso model (L1 penalty, alpha = 1)
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Plotting cross-validation curve
plot(lasso_model)

# Lambda that gives minimum mean cross-validated error
lambda_min <- lasso_model$lambda.min

# Coefficients of the lasso model at lambda.min
coef(lasso_model, s = "lambda.min")

# Evaluating the model on the test data

# Preparing predictors for glmnet
x_test <- model.matrix(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat + raterisk + bonemed + armassist,
                       data = testData2)[, -1]

# Use the same x_train and y_train from the previous step
x_train <- model.matrix(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat + raterisk + bonemed + armassist,
                        data = trainData2)[, -1]
y_train <- as.numeric(trainData2$fracture) - 1

# Fitting lasso with same seed
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Predicting on test set
lasso_probs <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
lasso_preds <- ifelse(lasso_probs > 0.3, 1, 0)

# Evaluating
confusionMatrix(
  factor(lasso_preds, levels = c(0, 1), labels = c("No", "Yes")),
  testData2$fracture,
  positive = "Yes"
)

# ROC and AUC
lasso_roc <- roc(testData2$fracture, as.numeric(lasso_probs))
plot(lasso_roc, main = "ROC Curve: Lasso Model")
cat("AUC (Lasso): ", auc(lasso_roc), "\n")

# Should I try messing with the threshold, to improve the sensitivity of this model? The code below helps find the best threshold...
coords(lasso_roc, "best", ret = "threshold")
# Result: Changing the threshold to .3 brought the sensitivity up with the trade of specificity and accuracy but these two are more balanced now.

# Now using this new and improved LASSO model, we will add interaction terms


# Fitting a complex logistic regression model with interaction terms
complex_model <- glm(fracture ~ fracscore + bmi + priorfrac + momfrac + bonetreat +
                       raterisk + bonemed + armassist +
                       fracscore:bonemed +
                       priorfrac:raterisk +
                       bmi:bonetreat +
                       fracscore * armassist,
                     data = trainData2, family = binomial)

summary(complex_model)

# Predicting on the test set
complex_probs <- predict(complex_model, newdata = testData2, type = "response")

# Using the improved threshold (0.3 rather than 0.1)
complex_preds <- ifelse(complex_probs > 0.3, 1, 0)

# Confusion Matrix to evaluate performance
library(caret)
confusionMatrix(
  factor(complex_preds, levels = c(0, 1), labels = c("No", "Yes")),
  testData2$fracture,
  positive = "Yes"
)

# ROC Curve and AUC
library(pROC)
complex_roc <- roc(testData2$fracture, complex_probs)
plot(complex_roc, main = "ROC Curve: Complex Logistic Model")
cat("AUC (Complex Model): ", auc(complex_roc), "\n")

# -----------------------------------
# Random Forest Model
# -----------------------------------
# Load libraries
library(randomForest)
library(caret)

set.seed(123)

# Creating a 75-25 split (the target variable here is 'fracture')
trainIndex <- createDataPartition(glow_bonemed$fracture, p = 0.75, list = FALSE)
trainData <- glow_bonemed[trainIndex, ]
testData <- glow_bonemed[-trainIndex, ]

table(trainData$fracture)

trainData <- trainData[, c("fracture","fracscore","bmi","priorfrac","momfrac","bonetreat","raterisk","bonemed","armassist")]
testData <- testData[, c("fracture", "fracscore","bmi","priorfrac","momfrac","bonetreat","raterisk","bonemed","armassist")]


# Train the Random Forest model
rf_model <- randomForest(fracture ~ ., 
                         data = trainData, 
                         ntree = 100,              # Number of trees
                         mtry = sqrt(ncol(trainData) - 1), # Selectors per split
                         importance = TRUE)

# Step 5: Predict on test data
# Ensure testData factor levels match training data
for (col in names(testData)) {
  if (is.factor(testData[[col]]) && col %in% names(trainData)) {
    testData[[col]] <- factor(testData[[col]], levels = levels(trainData[[col]]))
  }
}
predictions <- predict(rf_model, testData)

# Step 6: Evaluate
conf_mat <- confusionMatrix(predictions, testData$fracture)

# Step 7: Print results
print(rf_model)
print(conf_mat)

# ROC and AUC
# Get class probabilities (specifically for class "yes")
rf_probs <- predict(rf_model, testData, type = "prob")[, "Yes"]

library(pROC)

# Actual labels
actual <- testData$fracture

# ROC curve
roc_obj <- roc(actual, rf_probs)

# Plot ROC
plot(roc_obj, col = "blue", main = "ROC Curve - Random Forest")

# AUC value
auc_value <- auc(roc_obj)
cat("AUC (Random Forest): ", auc_value, "\n")
