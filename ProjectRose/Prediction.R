library(dplyr)
library(scales)

nflhistats <- readRDS("nflhistats24.rds")

nflhistats <- nflhistats %>% filter(Team == team)

# Extract all other columns except X5 as independent variables (x)
x <- nflhistats %>% select(5:11,14:20,22)

# Convert all columns to numeric
x[] <- lapply(x, function(x) as.numeric(as.character(x)))

# Rescale the features to the range [0, 1]
x_scaled <- as.data.frame(lapply(x, function(x) rescale(x, to = c(0, 1))))

x_scaled <- as.data.frame(lapply(x_scaled, function(x_scaled) log(x_scaled + 1)))

x_scaled <- x_scaled[complete.cases(x_scaled), ]

# Combine GLM predictions with the rest of the features
x_combined <- cbind(x_scaled, glm_preds = glm_preds)

#X_test <- array(as.numeric(x_combined))

X_test <- as.matrix(colMeans(x_combined, na.rm = TRUE))

X_test <- t(X_test)

score <- model %>% predict(X_test)
print(score)



