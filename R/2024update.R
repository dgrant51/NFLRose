library(dplyr)
library(keras)
library(scales)
library(reticulate)

# Load the data
nflhistats <- readRDS("D:/R code/NFL Prediction/nflhistats24.rds")

# Extract X5 as the dependent variable (y)
y <- nflhistats$Outcome

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(5:11,14:20,22)

y <- na.omit(y)

x <- na.omit(x)

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

x_combined <- cbind(x_scaled, glm_preds)

combo <- data.frame(y = y, x_combined)

combo <- combo[complete.cases(combo), ]

# Split the data into training and testing sets
set.seed(123)
train_data <- combo

X_new <- as.matrix(train_data[, -1])  # Exclude target variable (y)
y_new <- train_data$y

X_test <- array(as.numeric(X_test), dim = c(nrow(X_test), 16))

X_test <- as.matrix(colMeans(x_combined, na.rm = TRUE))

X_test <- t(X_test)

use_condaenv("tf_env", conda = "E:/New folder/Anaconda/condabin/conda")

loaded_model <- load_model_hdf5("my_model.h5")

# Continue training the model with new data (X_new and y_new)
currentSeason <- loaded_model %>% fit(
  X_new, y_new,             # New training data
  epochs = 40,              # Number of additional epochs
  batch_size = 5,          # Batch size
  verbose = 1               # Show training progress
)

save_model_hdf5(loaded_model, "updated_model.h5")



