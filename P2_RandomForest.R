
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

#rf_rmse <- sqrt(mean((testData$fracture - predictions)^2))
#cat("RF RMSE:", rf_rmse, "\n")

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
