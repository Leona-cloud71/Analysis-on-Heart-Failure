

install.packages("viridis")
install.packages("plotly")

# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(viridis)
library(ggthemes)
library(plotly)



###############Load the data

Leo<- read_csv("C:/Users/DE LEO/Desktop/Heart Failure/heart_failure_clinical_records_dataset.csv")

# Convert age to a numeric column if it's not already
#Leo$age <- as.numeric(Leo$age)

summary(Leo)

# Create the histogram using ggplot2

ggplot(Leo, aes(x = age)) +
  geom_histogram(bins = 25, fill = "purple", color = "black") +
  labs(title = "Distribution of Ages", x = "Age", y = "Count") +
  theme_minimal()


# Calculate the normalized value counts and rename the levels
Leo_summary <- Leo %>%
  count(DEATH_EVENT) %>%
  mutate(
    proportion = n / sum(n),
    DEATH_EVENT = factor(DEATH_EVENT, levels = c(0, 1), labels = c("Alive", "Died"))
  )

# Create the bar plot

ggplot(Leo_summary, aes(x = DEATH_EVENT, y = proportion, fill = DEATH_EVENT)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Distribution of DEATH_EVENT Class", x = "Classes", y = "Proportion") +
  scale_fill_manual(values = c("Alive" = "lightblue", "Died" = "orange")) +
  theme_minimal()




# Calculate the correlation matrix
cor_matrix <- cor(Leo, use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap using ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", direction = -1) +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
  labs(title = "Correlation Heatmap", x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))





library(dplyr)
library(ggplot2)

################# Filter data based on diabetes status

diabetes_yes <- Leo %>%
  filter(diabetes == 1)
diabetes_no <- Leo %>%
  filter(diabetes == 0)

# Filter further based on survival status
diabetes_yes_survive <- diabetes_yes %>%
  filter(DEATH_EVENT == 0)
diabetes_yes_not_survive <- diabetes_yes %>%
  filter(DEATH_EVENT == 1)
diabetes_no_survive <- diabetes_no %>%
  filter(DEATH_EVENT == 0)
diabetes_no_not_survive <- diabetes_no %>%
  filter(DEATH_EVENT == 1)

# Calculate counts and percentages
total <- nrow(Leo)
percentages <- c(
  nrow(diabetes_yes_survive) / total * 100,
  nrow(diabetes_yes_not_survive) / total * 100,
  nrow(diabetes_no_survive) / total * 100,
  nrow(diabetes_no_not_survive) / total * 100
)

# Create a dataframe for plotting
pie_data <- data.frame(
  labels = c('Diabetes Yes - Survived', 'Diabetes Yes - Not Survived', 'Diabetes No - Survived', 'Diabetes No - Not Survived'),
  values = percentages
)

# Plot pie chart with percentages using ggplot2
pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1, color = "red") +
  coord_polar("y", start = 0) +
  labs(title = "Analysis on Survival - Diabetes", fill = "") +
  geom_text(aes(label = paste0(round(values, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# Show the pie chart
print(pie_chart)





################# Filter data based on Anaemia status

anaemia_yes <- Leo %>%
  filter(anaemia == 1)
anaemia_no <- Leo %>%
  filter(anaemia == 0)

# Filter further based on survival status
anaemia_yes_survive <- anaemia_yes %>%
  filter(DEATH_EVENT == 0)
anaemia_yes_not_survive <- anaemia_yes %>%
  filter(DEATH_EVENT == 1)
anaemia_no_survive <- anaemia_no %>%
  filter(DEATH_EVENT == 0)
anaemia_no_not_survive <- anaemia_no %>%
  filter(DEATH_EVENT == 1)

# Calculate counts and percentages
total <- nrow(Leo)
percentages <- c(
  nrow(anaemia_yes_survive) / total * 100,
  nrow(anaemia_yes_not_survive) / total * 100,
  nrow(anaemia_no_survive) / total * 100,
  nrow(anaemia_no_not_survive) / total * 100
)

# Create a dataframe for plotting
pie_data <- data.frame(
  labels = c('Anaemia Yes - Survived', 'Anaemia Yes - Not Survived', 'Anaemia No - Survived', 'Anaemia No - Not Survived'),
  values = percentages
)

# Plot pie chart with percentages using ggplot2
pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1, color = "red") +
  coord_polar("y", start = 0) +
  labs(title = "Analysis on Survival - Anaemia", fill = "") +
  geom_text(aes(label = paste0(round(values, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# Show the pie chart
print(pie_chart)


################# Filter data based on HBP

HBP_yes <- Leo %>%
  filter(high_blood_pressure == 1)
HBP_no <- Leo %>%
  filter(high_blood_pressure == 0)

# Filter further based on survival status
HBP_yes_survive <- HBP_yes %>%
  filter(DEATH_EVENT == 0)
HBP_yes_not_survive <- HBP_yes %>%
  filter(DEATH_EVENT == 1)
HBP_no_survive <- HBP_no %>%
  filter(DEATH_EVENT == 0)
HBP_no_not_survive <- HBP_no %>%
  filter(DEATH_EVENT == 1)

# Calculate counts and percentages
total <- nrow(Leo)
percentages <- c(
  nrow(HBP_yes_survive) / total * 100,
  nrow(HBP_yes_not_survive) / total * 100,
  nrow(HBP_no_survive) / total * 100,
  nrow(HBP_no_not_survive) / total * 100
)

# Create a dataframe for plotting
pie_data <- data.frame(
  labels = c('HBP Yes - Survived', 'HBP Yes - Not Survived', 'HBP No - Survived', 'HBP No - Not Survived'),
  values = percentages
)

# Plot pie chart with percentages using ggplot2
pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1, color = "red") +
  coord_polar("y", start = 0) +
  labs(title = "Analysis on Survival - HBP", fill = "") +
  geom_text(aes(label = paste0(round(values, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# Show the pie chart
print(pie_chart)



################# Filter data based on Smoking

smoking_yes <- Leo %>%
  filter(smoking == 1)
smoking_no <- Leo %>%
  filter(smoking == 0)

# Filter further based on survival status
smoking_yes_survive <- smoking_yes %>%
  filter(DEATH_EVENT == 0)
smoking_yes_not_survive <- smoking_yes %>%
  filter(DEATH_EVENT == 1)
smoking_no_survive <- smoking_no %>%
  filter(DEATH_EVENT == 0)
smoking_no_not_survive <- smoking_no %>%
  filter(DEATH_EVENT == 1)

# Calculate counts and percentages
total <- nrow(Leo)
percentages <- c(
  nrow(smoking_yes_survive) / total * 100,
  nrow(smoking_yes_not_survive) / total * 100,
  nrow(smoking_no_survive) / total * 100,
  nrow(smoking_no_not_survive) / total * 100
)

# Create a dataframe for plotting
pie_data <- data.frame(
  labels = c('Smoking Yes - Survived', 'Smoking Yes - Not Survived', 'Smoking No - Survived', 'Smoking No - Not Survived'),
  values = percentages
)

# Plot pie chart with percentages using ggplot2
pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1, color = "red") +
  coord_polar("y", start = 0) +
  labs(title = "Analysis on Survival - Smoking", fill = "") +
  geom_text(aes(label = paste0(round(values, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# Show the pie chart
print(pie_chart)


################### check the relationship for other non-binary number

# Load necessary libraries
library(ggplot2)
library(dplyr)

# List of features to analyze

features <- c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium", "time")


################## I want Density a diiferent color


for (feature in features) {
  p <- ggplot(Leo, aes(x = .data[[feature]], fill = as.factor(DEATH_EVENT))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", feature, "by DEATH_EVENT"),
         x = feature,
         y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("0" = "green", "1" = "red"))
  
  print(p)
}


################ Histogram

for (feature in features) {
  p <- ggplot(Leo, aes(x = .data[[feature]], fill = as.factor(DEATH_EVENT))) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    facet_wrap(~DEATH_EVENT) +
    labs(title = paste("Histogram of", feature, "by DEATH_EVENT"),
         x = feature,
         y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("0" = "Green", "1" = "red"))
  
  print(p)
}

######################################Boxplot


for (feature in features) {
  p <- ggplot(Leo, aes(x = as.factor(DEATH_EVENT), y = .data[[feature]])) +
    geom_boxplot(aes(fill = as.factor(DEATH_EVENT)), alpha = 0.5) +
    geom_jitter(aes(color = as.factor(DEATH_EVENT)), width = 0.2, alpha = 0.5) +
    labs(title = paste("Analysis of", feature, "by DEATH_EVENT"),
         x = "DEATH_EVENT",
         y = feature) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))
  
  print(p)
}


##############################BUILDING THE MODEL



library(caret)
library(dplyr)
library(randomForest)

# Define features
features <- c('time', 'ejection_fraction', 'serum_creatinine', "age", "serum_sodium")
x <- Leo %>% select(all_of(features))
y <- Leo$DEATH_EVENT

# Split data into training and testing sets
set.seed(2)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
x_train <- x[trainIndex, ]
x_test <- x[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Convert target variable to factor
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

# Initialize accuracy list
accuracy_list <- list()



# Train logistic regression model
logistic_model <- train(x_train, y_train, method = "glm", family = "binomial")

# Train random forest model
random_forest_model <- train(x_train, y_train, method = "rf")

# Predict on the test set using logistic regression
logistic_predictions <- predict(logistic_model, x_test)

# Predict on the test set using random forest
random_forest_predictions <- predict(random_forest_model, x_test)

# Ensure predictions are factors with the same levels as y_test
logistic_predictions <- factor(logistic_predictions, levels = levels(y_test))
random_forest_predictions <- factor(random_forest_predictions, levels = levels(y_test))

# Calculate accuracy for logistic regression
logistic_confusionMatrix <- confusionMatrix(logistic_predictions, y_test)
logistic_accuracy <- logistic_confusionMatrix$overall['Accuracy']

# Calculate accuracy for random forest
random_forest_confusionMatrix <- confusionMatrix(random_forest_predictions, y_test)
random_forest_accuracy <- random_forest_confusionMatrix$overall['Accuracy']

# Store accuracies in the list
accuracy_list[['Logistic Regression']] <- logistic_accuracy
accuracy_list[['Random Forest']] <- random_forest_accuracy

# Print the accuracies
print(accuracy_list)

###########################Plot the confusion matrix

install.packages("ggplot2")
install.packages("pheatmap")

library(reshape2)
library(caret)
library(ggplot2)
library(pheatmap)


# Load necessary libraries
library(caret)
library(ggplot2)

# Compute confusion matrices
logistic_confusionMatrix <- confusionMatrix(logistic_predictions, y_test)
random_forest_confusionMatrix <- confusionMatrix(random_forest_predictions, y_test)

# Extract confusion matrices
logistic_cm <- logistic_confusionMatrix$table
random_forest_cm <- random_forest_confusionMatrix$table

# Convert confusion matrices to data frames
logistic_cm_df <- as.data.frame(as.table(logistic_cm))
random_forest_cm_df <- as.data.frame(as.table(random_forest_cm))

# Plot confusion matrix for Logistic Regression
ggplot(logistic_cm_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.0f", Freq)), color = "black", size = 5, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Logistic Regression", x = "Predicted", y = "Actual") +
  theme_minimal()

# Plot confusion matrix for Random Forest
ggplot(random_forest_cm_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.0f", Freq)), color = "black", size = 5, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Random Forest", x = "Predicted", y = "Actual") +
  theme_minimal()




# Load necessary libraries
library(caret)
library(pheatmap)

# Compute confusion matrices
logistic_confusionMatrix <- confusionMatrix(logistic_predictions, y_test)
random_forest_confusionMatrix <- confusionMatrix(random_forest_predictions, y_test)

# Extract confusion matrices
logistic_cm <- logistic_confusionMatrix$table
random_forest_cm <- random_forest_confusionMatrix$table

# Plot confusion matrix for Logistic Regression using pheatmap
pheatmap(logistic_cm, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE,
         main = "Confusion Matrix for Logistic Regression")

# Plot confusion matrix for Random Forest using pheatmap
pheatmap(random_forest_cm, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE,
         main = "Confusion Matrix for Random Forest")








####################Let me try to build another model and see which is better


