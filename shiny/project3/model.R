# install.packages("caret", dependencies = TRUE)
library(randomForest)
library(caret)
data(iris)
set.seed(123)  # for reproducibility
# Assuming 'data' is your dataframe and 'target' is the name of the response variable
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Train the model with parameter tuning
tuned_model <- train(Species ~ ., data = trainData, method = "rf",
                     trControl = train_control,
                     tuneLength = 10)  # tuneLength controls the number of different parameter sets to try

# Model summary
print(tuned_model)

# Make predictions and evaluate the model
predictions <- predict(tuned_model, testData)
confusionMatrix(predictions, testData$Species)
