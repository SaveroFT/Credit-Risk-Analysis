library(DataExplorer)
library(missForest)

# Data Import
file = read.csv("C:\\Asia Pasific University\\Year 2\\Sem 1\\Programming for Data Analysis\\Assignment\\5. credit_risk_classification.csv")

# Check if there are blank space
blank_counts <- sapply(file, function(x) if (is.character(x)) sum(x == "" & !is.na(x)) else 0)
print(blank_counts)

# Change empty data become NA
for (column in names(file)){
  file[[column]][file[[column]] == ""] <- NA
}

# Check if there are NA in the dataset
sum(is.na(file))

# Change none to no
file$own_telephone = replace(file$own_telephone, file$own_telephone == "none", "no")

# Correcting 500<=X<10000 to 500<=X<1000
file$savings_status <- replace(file$savings_status, file$savings_status == "500<=X<10000", "500<=X<1000")

# Change all character become factor to run random forest imputation
for (column in names(file)){
  if (is.character(file[[column]]) == TRUE)
  {
    file[[column]] <- as.factor(file[[column]])
  }
}

# Check data type
glimpse(file)

# Set seed
set.seed(123)

# MissForest to fill in all NA
data_imp <- missForest(file)

# Change variable name
df <- data_imp$ximp

# View Data After Random Forest
View(df)

# Change data type from factor to character 
for (column in names(df)){
  if (is.factor(df[[column]]) == TRUE)
  {
    df[[column]] <- as.character(df[[column]])
  }
}

# Change duration data type to int
df$duration = as.integer(df$duration)

# Change residence_since data type to int
df$residence_since = as.integer(df$residence_since)

# Change age data type from dbl to int
df$age = as.integer(df$age)

# Change existing_credits data type to int
df$existing_credits = as.integer(df$existing_credits)

# Change num_dependents data type to int
df$num_dependents = as.integer(df$num_dependents)

# Round credit_amount to 2 decimal places
df$credit_amount = round(df$credit_amount,2)

# Round installment_commitment to 2 decimal places
df$installment_commitment = as.integer(df$installment_commitment)

# Change data type from character to factor
for (column in names(df)){
  if (is.character(df[[column]]) == TRUE)
  {
    df[[column]] <- as.factor(df[[column]])
  }
}

# Check data type
glimpse(df)

##################################################################################################################################################
# Data Validation

# Sum all NA
sum(is.na(df))

# Plot for checking missing value
plot_missing(df)

# Check Outlier
summary(df)

# Data Export
write.csv(df, "clean_data.csv", row.names = FALSE)
