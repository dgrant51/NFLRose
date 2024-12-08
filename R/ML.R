library(dplyr)
library(keras)
library(scales)
library(reticulate)

# Load the data
nflhistats <- readRDS("D:/R code/NFL Prediction/nflhistats.rds")

# Extract X5 as the dependent variable (y)
y <- nflhistats$X5

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(X11:X17,X20:X26,X28)

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

glm_preds <- as.numeric(unlist(glm_preds))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

# Combine GLM predictions with the rest of the features
x_combined <- cbind(x_scaled, glm_preds = glm_preds)

combo <- data.frame(y = y, x_combined)

combo <- combo[complete.cases(combo), ]

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(combo), 0.8 * nrow(combo))
train_data <- combo[train_indices, ]
test_data <- combo[-train_indices, ]

X_train <- as.matrix(train_data[, -1])  # Exclude target variable (y)
y_train <- train_data$y

X_test <- as.matrix(test_data[, -1])
y_test <- test_data$y

use_condaenv("tf_env", conda = "E:/New folder/Anaconda/condabin/conda")

early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 10)

# Define the LSTM model
model <- keras_model_sequential() %>%
  layer_dense(units=130, activation = 'leaky_relu') %>%
  layer_dense(units = 80, activation = 'relu')%>%
  layer_dense(units = 50, activation = 'tanh') %>%
  layer_dense(units = 1, activation = 'sigmoid')  # Sigmoid output for binary classification

# Compile the model with Adam optimizer and binary cross-entropy loss
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 1e-5),
  metrics = c('accuracy')
)

# Train the model
history <- model %>% fit(
  X_train, y_train,
  epochs = 60,          # Number of epochs
  batch_size = 60,       # Batch size
  validation_split = 0.2,  # Use 20% of training data for validation
  verbose = 1,
  callbacks = list(early_stopping)
)

save_model_hdf5(model, "my_model.h5")

# Evaluate the model on the test set
score <- model %>% evaluate(X_test, y_test)
print(score)



