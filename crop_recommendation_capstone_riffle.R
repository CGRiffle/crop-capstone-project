
title: "Crop Recommendation Capstone"
author: "C.G. Riffle"
date: "2023-03-19"
output: pdf_document
toc: TRUE
toc_float: TRUE
toc_number: TRUE


 # Automatically load packages needed for analysis
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(factoMineR)) install.packages("FactoMineR", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")

# load libraries
library(readr)
library(ggplot2)
library(corrplot)
library(dplyr)
library(caret)
library(tidyverse)  
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(class)
library(randomForest)
library(nnet)


# Load the Data Set
crop_data <- read_csv("https://raw.github.com/CGRiffle/crop-capstone-project/main/Crop_recommendation.csv")


# Split the data set into a train and test set.
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = crop_data$label, times = 1, p = 0.1, list = FALSE)

train_crop <- crop_data[-test_index,]
test_crop <- crop_data[test_index,]

# Data Exploration and Visualization 
# The entire crop recommendation data set was used to explore and visualize the data prior to model development. 
# Basic code functions were used to understand the structure, dimensions, and features fo the data set. 
# Data processing and cleaning was then performed to change 3 integer columns to numeric values.  
#The "label" column representing the crops is converted to a factor later in the analysis. 

# Data Structure
# The basic data structure was determined using the str() function for the entire data set.  
# Each variable has the data type (class). 
# Explore the data structure
str(crop_data)

# Summarize basic statistics for each variable
summary(crop_data)

crop_number <- n_distinct(crop_data$label)

crop_data |> group_by(label) |>
  summarize(count = n()) |>
  knitr::kable(col.names = c("Crop Name", "Number of Observations"), caption = "**Distribution of Observations for Each Crop**")

# Check for null values
colSums(is.na(crop_data))

head(crop_data)

# convert the first three columns of the training and test sets to numeric values.
train_crop[,1:3] <- lapply(train_crop[, 1:3] , as.numeric)
test_crop[,1:3] <- lapply(test_crop[, 1:3] , as.numeric)


# Check replacement
str(train_crop)
str(test_crop)

# Macromolecule Variables
# The macromolecules (N, P, K) were examined to understand the number of distinct values and basic statistics for each crop. 

# Number of individual values for each macromolecule
train_crop |> 
  summarize(n_K = n_distinct(K),
            n_P = n_distinct(P),
            n_N  = n_distinct(N))

# Mean and standard deviation for each crop
train_crop |>
  group_by(label) |>
  summarize(ave_N = mean(N), sd_n = sd(N), ave_k = mean(K), sd_k = sd(K), ave_p = mean(P), sd_p = sd(P)) |>
  rename(Crops = label) |> 
  knitr::kable(col.names = c("Crops", "Mean N", "SD N", "Mean K", "SD K", "Mean P", "SD P"), caption = "**Basic Macromolecule Statistics for Crops**") 

# Nitrogen (N)
# Plot of the Nitrogen distribution with vertical lines for the mean and median across all crops.
ggplot(data = train_crop) +
  geom_point(aes(N, label, color = N)) +
  ggtitle("Nitrogen Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$N), color = "red") +
  geom_vline(xintercept = median(train_crop$N), color = "black")


# Representative qplot for Nitrogen values
qplot(train_crop$N, bins = 30, fill = train_crop$label, color = I("white")) +
  xlab("Nitrogen ") +
  labs(fill = "Crops")


# Phosphorous (P)
# Plot of Phosphorous distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(P, label, color = P)) +
  ggtitle("Phosphorous Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$P), color = "red") +
  geom_vline(xintercept = median(train_crop$P), color = "black")

# Potassium (K)
# Plot of Potassium distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(K, label, color = K)) +
  ggtitle("Potassium Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$K), color = "red") +
  geom_vline(xintercept = median(train_crop$K), color = "black")

# pH
# Number of individual values for pH
train_crop |> 
  summarize(n_ph = n_distinct(ph))

# pH mean and standard deviation for each crop
train_crop |>
  group_by(label) |>
  summarize(ave_ph = mean(ph), sd_ph = sd(ph)) |>
  rename(Crops = label) |>
  knitr::kable(col.names = c("Crops", "Mean", "SD"), caption = "**Soil pH Mean and Standard Deviation for Crops**") 

# Plot of pH distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(ph, label, color = ph)) +
  ggtitle("pH Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$ph), color = "red") +
  geom_vline(xintercept = median(train_crop$ph), color = "black")


# Representative qplot for pH values
qplot(train_crop$ph, bins = 30, fill = train_crop$label, color = I("white")) +
  xlab("pH") +
  labs(fill = "Crops")

# Climate Variables
# Number of individual values for each climate variable
train_crop |> 
  summarize(n_temp = n_distinct(temperature),
            n_humidity = n_distinct(humidity),
            n_rain  = n_distinct(rainfall))

# Mean and standard deviation for each crop
train_crop |>
  group_by(label) |>
  summarize(ave_temp = mean(temperature), sd_temp = sd(temperature), ave_hum = mean(humidity), sd_hum = sd(humidity), ave_rain = mean(rainfall), sd_rain = sd(rainfall)) |>
  rename(Crops = label) |>
  knitr::kable(col.names = c("Crops", "Mean Temperature (C)", "SD Temperature (C)", "Mean %RH", "SD %RH", "Mean Rainfall (mm)", "SD Rainfall (mm)"), caption = "**Basic Statistics for Crops**") 

# Temperature
# Plot of temperature distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(temperature, label, color = temperature)) +
  ggtitle("Temperature Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$temperature), color = "red") +
  geom_vline(xintercept = median(train_crop$temperature), color = "black")


# Humidity
# Plot of humidity distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(humidity, label, color = humidity)) +
  ggtitle("Humidity Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$humidity), color = "red") +
  geom_vline(xintercept = median(train_crop$humidity), color = "black")


# Representative qplot for humidity
qplot(train_crop$humidity, bins = 30, fill = train_crop$label, color = I("white")) +
  xlab("Humidity") +
  labs(fill = "Crops")


# Rainfall
# Plot of rainfall distribution with vertical lines for the mean and median across all crops
ggplot(data = train_crop) +
  geom_point(aes(rainfall, label, color = rainfall)) +
  ggtitle("Rainfall Data Distribution Range") +
  geom_vline(xintercept = mean(train_crop$rainfall), color = "red") +
  geom_vline(xintercept = median(train_crop$rainfall), color = "black")


# **Model Development**
# **Model 1 Random Forest**

# Set the K-fold Cross Validation using the trainControl() function
control_rf <- trainControl(method = "cv", # resample data
                           number = 10,  # folds
                           search = "grid") 

# Convert label to a factor in train and test data sets
train_crop2 <- train_crop
train_crop2$label <- factor(train_crop$label)

test_crop2 <- test_crop
test_crop2$label <- factor(test_crop$label)


# Base Random Forest model using all 7 predictors
# Train model
set.seed(1, sample.kind = "Rounding")

rf_model <- train(label~.,
                  data = train_crop2,
                  method = "rf",
                  control_rf = control_rf,
                  ntree = 100,
                  metric = "Accuracy")


print(rf_model)
rf_model$bestTune

# Option to look at the plot
ggplot(rf_model)


# Check Accuracy of Model 1 with the Test Data
rf_preds <- predict(rf_model, test_crop2)
mean(rf_preds == test_crop2$label)


# **Model 2 Random Forest - Reduced Variables**
# The importance of predictors for Model 1 was checked using the varImp() function. 
varImp(rf_model)

# The top 4 variables were selected for both the train and test data set.
train_crop3 <- train_crop2 |> select(rainfall, humidity, K, P, label)
test_crop3 <- train_crop2 |> select(rainfall, humidity, K, P, label)

# Train Model 2
set.seed(1, sample.kind = "Rounding")

rf_model2 <- train(label~.,
                   data = train_crop3,
                   method = "rf",
                   control_rf = control_rf,
                   ntree = 100,
                   metric = "Accuracy")


print(rf_model2)
rf_model2$bestTune


# Option to plot
plot(rf_model2)

# Check Accuracy of Model 2 - Test Data
rf_preds2 <- predict(rf_model2, test_crop3)
mean(rf_preds2 == test_crop3$label)


# Model 3 Principal Component Analysis

# Check Correlation of Independent Variables
# First, normalize numeric data in the data set using scaling (columns 1:7)
train_numeric <- train_crop[,1:7]

train_normal <- scale(train_numeric)
head(train_normal)


# Correlation matrix 
train_corr_matrix <- cor(train_normal)

# The correlation plot can be interpreted by higher positive values having a higher correlation and the negative values closest to -1.0 are the most negatively correlated. 
ggcorrplot(train_corr_matrix, hc.order = TRUE, lab = TRUE, colors = c("darkred", "white", "steelblue"))



# PCA*
train_pca <- prcomp(train_crop[,1:7], center = TRUE, scale = TRUE)
summary(train_pca)

# Plot importance. 
plot(summary(train_pca)$importance[3,])

print(train_pca)

# Scree plot to show contribution of components to variance
fviz_eig(train_pca, addlabels = TRUE) +
  xlab("Principal Component")

print(train_pca)

# Eigenvalues 
eig.val <- get_eigenvalue(train_pca)
eig.val


# Bi-Plot
fviz_pca_var(train_pca, col.var = "cos2",
             gradient.cols = c("black", "blue", "green"),
             repel = TRUE)

# Re-check correlation 
check <-cor(train_pca$x)
ggcorrplot(check, hc.order = TRUE, lab = TRUE, colors = c("darkred", "white", "steelblue"))


# Multinomial Regression using Principal Components for Prediction**
# Mulitnomial logistic regression was performed with the first 4 PC's. 
#The crop data was added back into bothteh train and test data sets.  

# Add crop information back into the data sets
train <- predict(train_pca, train_crop)
train <- data.frame(train, train_crop[8])
test <- predict(train_pca, test_crop)
test <- data.frame(test, test_crop[8])


# Train using 4 PC's
train$label <- factor(train$label, ordered=FALSE)
train$label <- relevel(train$label, ref = "apple")
multinomial_model <- multinom(label~PC1+PC2+PC3+PC4+PC5, data = train)
summary(multinomial_model)

# Evaluate the test data.frame
multinomial_pred <- predict(multinomial_model, test)
multinomial_table <- table(multinomial_pred, test$label)

# Error associated with the test set
multinomial_model_error <- 1 - sum(diag(multinomial_table)/sum(multinomial_table))
multinomial_model_error










