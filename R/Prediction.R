library(dplyr)
library(scales)
library(reticulate)
library(keras)

nflhistats <- readRDS("nflhistats24.rds")

nflhistats <- nflhistats %>% filter(Team == team_number)

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(5:11,14:20,22)

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

x_scaled <- as.data.frame(lapply(x_scaled, function(x_scaled) log(x_scaled + 1)))

x_scaled <- x_scaled[complete.cases(x_scaled), ]

load(file = "glm_model.RData")

# Generate GLM predictions
glm_preds <- predict(model, newdata = x_scaled, type = "response")

x_combined <- cbind(x_scaled, glm_preds = glm_preds)

X_test <- as.matrix(colMeans(x_combined, na.rm = TRUE))

X_test <- t(X_test)

use_condaenv("tf_env", conda = "E:/New folder/Anaconda/condabin/conda")

loaded_model <- load_model_hdf5("updated_model.h5")

score <- loaded_model %>% predict(X_test)
print(score)



