#install.packages("aplore3")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("car")

library(aplore3)
library(caret)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(car)

data("glow_bonemed")

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

for (var in continuous_vars) {
  trainData[[var]] <- as.numeric(trainData[[var]])  # ensure numeric
}

# Continuous (Numeric) Variables: age, weight, height, bmi, fracscore, bonemed
continuous_vars <- c("age", "weight", "height", "bmi", "fracscore", "bonemed")

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

# Binary (Factor) Variables: priorfrac, premeno, momfrac, armassist, bonetreat
binary_vars <- c("priorfrac", "premeno", "momfrac", "armassist", "bonetreat", "smoke")

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

