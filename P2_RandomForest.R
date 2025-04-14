###OBJECTIVE 2 RANDOM FOREST MODEL
# Install required packages if not already installed


# Load libraries
library(randomForest)
library(caret)
library(ggplot2)

set.seed(123)

# Creating a 70-30 split (the target variable here is 'fracture')
trainIndex <- createDataPartition(glow_bonemed$fracture, p = 0.75, list = FALSE)
trainData <- glow_bonemed[trainIndex, ]
testData <- glow_bonemed[-trainIndex, ]

table(trainData$fracture)

# #omit missing data
# trainData <- na.omit(trainData)
# testData <- na.omit(testData)

#Omit two categorical variables so model can still function well
trainData <- trainData[, !names(trainData) %in% c("sub_id","site_id","phy_id","bonemed_fu")]
testData <- testData[, !names(testData) %in% c("sub_id","site_id","phy_id","bonemed_fu")]


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


# Plot variable importance
importance_df <- data.frame(Variable = rownames(importance(rf_model)), 
                            Importance = importance(rf_model)[, 1])

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  coord_flip() +
  ggtitle("Variable Importance in Random Forest") +
  xlab("Variables") + ylab("Importance")

# Hyperparameter tuning with Grid Search (Optional)
tune_rf <- train(fracture ~ ., 
                 data = trainData, 
                 method = "rf", 
                 tuneGrid = expand.grid(mtry = c(2, 5, 10)),
                 trControl = trainControl(method = "cv", number = 5))

print(tune_rf)