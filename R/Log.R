library(dplyr)
library(scales)
library(ggplot2)  # For plotting
library(pROC)
library(caret)

nflhistats <- readRDS("D:/R code/NFL Prediction/nflhistats.rds")

# Extract X5 as the dependent variable (y)
y <- nflhistats$X5

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(-X5)

# Handle missing values in X2 (replacing NAs with 4)
x$X2[is.na(x$X2)] <- 4

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

combo <- data.frame(y, x_scaled)

combo[] <- lapply(combo, function(combo) as.numeric(as.character(combo)))

vars <- c(11:17, 20:26, 28)

formula_str <- paste("y ~", paste("log(X", vars, "+1)", collapse = " + "))

# Remove all spaces using gsub
formula_str <- gsub("\\s", "", formula_str)

# Convert the string to a formula
formula <- as.formula(formula_str)

# Fit the logarithmic regression model
model <- glm(formula, data = combo)

summary(model)

save(model, file = "glm_model.RData")

# Generate GLM predictions
glm_preds <- predict(model, newdata = x_scaled, type = "response")

# You can convert them to binary predictions if needed
glm_binary_preds <- ifelse(glm_preds > 0.5, 1, 0)

# Add GLM predictions to the data
combo$glm_preds <- glm_binary_preds


# Compare predictions to actual values (y)
actual_binary <- combo$y  # Assuming y is the binary outcome variable

# Create a confusion matrix to compare actual vs predicted values
table(Predicted = glm_binary_preds, Actual = actual_binary)

# Compute accuracy
accuracy <- mean(glm_binary_preds == actual_binary)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%", sep = ""))

conf_matrix <- confusionMatrix(factor(glm_binary_preds), factor(actual_binary))
print(conf_matrix)

roc_curve <- roc(actual_binary, glm_binary_preds)
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)  # This will give you the AUC (Area Under the Curve)
library(dplyr)
library(scales)
library(ggplot2)  # For plotting
library(pROC)
library(caret)

nflhistats <- readRDS("D:/R code/NFL Prediction/nflhistats.rds")

# Extract X5 as the dependent variable (y)
y <- nflhistats$X5

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(-X5)

# Handle missing values in X2 (replacing NAs with 4)
x$X2[is.na(x$X2)] <- 4

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

combo <- data.frame(y, x_scaled)

combo[] <- lapply(combo, function(combo) as.numeric(as.character(combo)))

vars <- c(11:17, 20:26, 28)

formula_str <- paste("y ~", paste("log(X", vars, "+1)", collapse = " + "))

# Remove all spaces using gsub
formula_str <- gsub("\\s", "", formula_str)

# Convert the string to a formula
formula <- as.formula(formula_str)

# Fit the logarithmic regression model
model <- glm(formula, data = combo)

summary(model)

save(model, file = "glm_model.RData")

# Generate GLM predictions
glm_preds <- predict(model, newdata = x_scaled, type = "response")

# You can convert them to binary predictions if needed
glm_binary_preds <- ifelse(glm_preds > 0.5, 1, 0)

# Add GLM predictions to the data
combo$glm_preds <- glm_binary_preds


# Compare predictions to actual values (y)
actual_binary <- combo$y  # Assuming y is the binary outcome variable

# Create a confusion matrix to compare actual vs predicted values
table(Predicted = glm_binary_preds, Actual = actual_binary)

# Compute accuracy
accuracy <- mean(glm_binary_preds == actual_binary)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%", sep = ""))

conf_matrix <- confusionMatrix(factor(glm_binary_preds), factor(actual_binary))
print(conf_matrix)

roc_curve <- roc(actual_binary, glm_binary_preds)
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)  # This will give you the AUC (Area Under the Curve)
