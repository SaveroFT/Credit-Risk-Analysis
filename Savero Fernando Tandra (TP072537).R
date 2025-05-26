library(ggplot2)
library(VIM)
library(caret)
library(randomForest)
library(plyr)
library(dplyr)
library(reshape2)
library(pROC)
library(caTools)
library(tidyverse)

# Data Import
df = read.csv("clean_data.csv")

# View data
View(df)

# 1. What is the distribution between checking status and purpose with credit class?

# Contingency Table
cs_table = table(df$checking_status, df$class)
cs_table

purpose_table = table(df$purpose, df$class)
purpose_table

# Create a proportion table
cs_proportion <- df %>% 
  group_by(checking_status, class) %>% # Group the data by checking_status and class columns
  summarize(Frequency = n()) %>% # Summarize the frequency
  mutate(Proportion = Frequency / sum(Frequency)) %>% # Add proportion column by divide the frequency with total frequency
  arrange(desc(class), desc(Frequency)) # Arrange the class and Frequency by descending

cs_proportion

purpose_proportion <- df %>% 
  group_by(purpose, class) %>% # Group the data by purpose and class columns
  summarize(Frequency = n()) %>% # Summarize the frequency
  mutate(Proportion = Frequency / sum(Frequency)) %>% # Add proportion column by divide the frequency with total frequency
  arrange(desc(class), desc(Frequency)) # Arrange the class and Frequency by descending

purpose_proportion

# Heatmap
heatmap <- as.data.frame(table(df$checking_status, df$purpose, df$class))

# Change column name
names(heatmap) <- c("checking_status", "purpose", "class", "frequency") 

# Check variable for heatmap credit class
unique(heatmap$class)

# Change from factor to character
heatmap$class <- as.character(heatmap$class)

# Filter data for good credit class
heatmap_good <- subset(heatmap, class == "good")

# Create the heatmap for good credit class
ggplot(heatmap_good, aes(x = checking_status, y = purpose, fill = frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Checking Status", y = "Purpose", fill = "Frequency", title = "Distribution of Checking Status and Purpose Getting Good Credit Class") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Filter data for bad credit class
heatmap_bad <- subset(heatmap, class == "bad")

# Create the heatmap for bad credit class
ggplot(heatmap_bad, aes(x = checking_status, y = purpose, fill = frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Checking Status", y = "Purpose", fill = "Frequency", title = "Distribution of Checking Status and Purpose Getting Bad Credit Class") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# 2. Is checking status and purpose of loan a strong predictor for credit class?
# Chi-squared test
cs_relationship = chisq.test(cs_table)
cs_relationship

purpose_relationship = chisq.test(purpose_table)
purpose_relationship

# Converting the class column to binary format (0 and 1)
df$class <- factor(df$class, levels = c("bad", "good"), labels = c("0", "1"))

# Train Test Split
# Split the data into 80:20
split <- sample.split(df$class, SplitRatio = 0.8)

# Split for training
training_set = subset(df, split == T)

# Split for test
test_set = subset(df, split == F)

# Logistic Regression
classifier <- glm(class ~ checking_status + purpose, family = binomial, data = training_set)

# Summary of Logistic Regression
summary(classifier)

# Extract coefficients from the model
coefficients <- coef(classifier)

# Exponentiate the coefficients to obtain the odds ratio
odd_ratio <- exp(coefficients)
odd_ratio

# Create tibble(data frame)
odds_data <- tibble(Predictor = names(odd_ratio), OddsRatio = odd_ratio)

# Create a new column "Effect" to indicate if the odd ratio increase or decrease the odds of getting good credit class
tidy_odd_data <- odds_data %>% 
  mutate(Effect = case_when(
    OddsRatio > 1.05 ~ "Increase Odds", # If odds ratio is greater than 1.05, indicates the predictor as decreasing the odd
    OddsRatio < 1 ~ "Decrease Odds", # If odds ratio is less than 1, indicates the predictor as increasing the odd
    TRUE ~ "No Effect")) %>% # If odds ratio is near 1, indicates the predictor as no effect
  arrange(desc(OddsRatio)) # Arrange Odds Ratio descending

# Print the tidy data
tidy_odd_data

# Random Forest
rf_classifier = randomForest(class ~ checking_status + purpose, data = training_set, ntree = 500)

# Plot the Random Forest
varImpPlot(rf_classifier)

# 3. How well do checking status and purpose of loan predict credit class?
# Logistic Regression
classifier <- glm(class ~ checking_status + purpose, family = binomial, data = training_set)

# Predict the probability for the test set
pred_prob_test = predict(classifier, type = "response", test_set[,-21])

# Convert predicted probabilities to binary label (0 and 1) using a threshold of 0.5
pred_class_test = ifelse(pred_prob_test > 0.5, 1,0)

# Compare predicted classes with actual classes using confusion matrix
confusionMatrix(table(pred_class_test, test_set$class))

# Random Forest
rf_classifier = randomForest(class ~ checking_status + purpose, data = training_set, ntree = 500)

# Predicting class labels for the test set using Random Forest
pred_rf = predict(rf_classifier, newdata = test_set[-21])

# Predicting class probabilities for the positive class (class = 1) for AUC-ROC
pred_prob_rf = predict(rf_classifier, newdata = test_set[-21], type = "prob")[,2]

# Compare predicted classes with actual classes using confusion matrix
confusionMatrix(table(pred_rf, test_set$class))

# ROC(Receiver Operating Characteristic) for Logistic Regression
roc_lg = roc(test_set$class, pred_prob_test)

# ROC(Receiver Operating Characteristic) for Random Forest
roc_rf <- roc(test_set$class, pred_prob_rf, levels = rev(levels(test_set$class)))

# Plot the ROC curve for the Random Forest
plot(roc_rf, col = "blue", lwd = 2, main = "Logistic Regression vs Random Forest")

# Add the ROC curve for Logistic Regression
lines(roc_lg, col = "red", lwd = 2)

# Add legend or label to the bottom right of ROC curves
legend("bottomright", legend = c("Random Forest", "Logistic Regression"), col = c("blue", "red"), lwd = 2)

# AUC(Area Under Curve) for Logistic Regression
auc_lg = auc(roc_lg)
auc_lg

# AUC(Area Under Curve) for Random Forest
auc_rf = auc(roc_rf)
auc_rf