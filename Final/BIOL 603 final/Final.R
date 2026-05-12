



# FINAL PROJECT - RANDOM FOREST MODEL
# QAQC AND DATA CLEANING


# Load required packages
library(dplyr)


# Read in dataset
demog <- read.csv(
  "demography_2014-2021_manual.edits.csv",
  stringsAsFactors = FALSE
)

# Create working copy for cleaning pipeline
demog_clean <- demog


# Inspect dataset structure
# View structure
str(demog_clean)

# Summary statistics
summary(demog_clean)

# Dimensions
dim(demog_clean)

# First few rows
head(demog_clean)


# Check duplicates
# Duplicate rows can inflate sample size and bias model results
sum(duplicated(demog_clean))


# Check missing values
# Identify NA values across dataset
colSums(is.na(demog_clean))

# Percent missing per variable
round(colMeans(is.na(demog_clean)) * 100, 2)


# Remove rows with ANY missing values
# Random forest (ranger) cannot handle NA values in predictors or response
demog_clean <- demog_clean %>%
  filter(complete.cases(.))

# Recheck missing values after removal
colSums(is.na(demog_clean))


# Remove rows with invalid binary entries ("o" or "O")
# These represent data entry errors in 0/1 coded variables
binary_vars <- c(
  "Survival",
  "Is.seedling",
  "Is.recruit",
  "Died.this.census.final"
)

demog_clean <- demog_clean %>%
  filter(
    !if_any(all_of(binary_vars), ~ . %in% c("o", "O"))
  )


# Remove biologically impossible values
# Plant measurements cannot be negative
demog_clean <- demog_clean %>%
  filter(
    Length.cm >= 0,
    Height.cm >= 0
  )


# Final QAQC checks
# Confirm no missing values remain
colSums(is.na(demog_clean))

# Check dataset dimensions after cleaning
dim(demog_clean)


# Convert categorical variables to factors
# Random forests handle categorical predictors best when they are stored as factors.
demog_clean$Taxon <- as.factor(demog_clean$Taxon)

demog_clean$Plot <- as.factor(demog_clean$Plot)

demog_clean$Year <- as.factor(demog_clean$Year)

demog_clean$Is.seedling <- as.factor(demog_clean$Is.seedling)

demog_clean$Is.recruit <- as.factor(demog_clean$Is.recruit)

demog_clean$Survival <- as.factor(demog_clean$Survival)

demog_clean$Died.this.census.final <- as.factor(
  demog_clean$Died.this.census.final
)


# Check factor levels
# Ensures categorical variables were converted correctly.

levels(demog_clean$Survival)

levels(demog_clean$Is.seedling)

levels(demog_clean$Is.recruit)

levels(demog_clean$Died.this.census.final)



# Examine distributions of numeric variables
# Histograms help identify skewed variables, impossible values, and potential outliers.
hist(
  demog_clean$Length.cm,
  main = "Plant Length Distribution",
  xlab = "Length (cm)"
)

hist(
  demog_clean$Height.cm,
  main = "Plant Height Distribution",
  xlab = "Height (cm)"
)

hist(
  demog_clean$Growth,
  main = "Plant Growth Distribution",
  xlab = "Growth (cm)"
)


# Check class balance of response variable
# Highly imbalanced classes can affect classification model performance.

table(demog_clean$Survival)
prop.table(table(demog_clean$Survival))


# Remove unnecessary variables
# Tag identifiers and spatial tag offsets are not
# biologically meaningful predictors for survival
# and may introduce noise into the model.

demog_rf <- demog_clean %>%
  select(
    -Tag,
    -Tag.offset.x,
    -Tag.offset.y
  )


# Final inspection of cleaned dataset
str(demog_rf)

summary(demog_rf)

dim(demog_rf)




# STEP 2: SUMMARY STATISTICS + EXPLORATORY DATA ANALYSIS (EDA)

# Using demog_rf ensures all QAQC steps are included
data <- demog_rf


# SUMMARY STATISTICS
# Key numeric predictors
# These are likely important for survival/growth relationships
data %>%
  summarise(
    mean_length = mean(Length.cm, na.rm = TRUE),
    sd_length   = sd(Length.cm, na.rm = TRUE),
    mean_height = mean(Height.cm, na.rm = TRUE),
    sd_height   = sd(Height.cm, na.rm = TRUE),
    mean_growth = mean(Growth, na.rm = TRUE),
    sd_growth   = sd(Growth, na.rm = TRUE)
  )

# Check class balance of response variable (important for classification RF)
table(data$Survival)
prop.table(table(data$Survival))


# EXPLORATORY PLOTS
# Distribution of plant size
# Chosen because plant size is a key predictor in ecological survival models
hist(
  data$Length.cm,
  main = "Distribution of Plant Length",
  xlab = "Length (cm)"
)

hist(
  data$Height.cm,
  main = "Distribution of Plant Height",
  xlab = "Height (cm)"
)


# Growth distribution
# Growth is a direct measure of performance and likely predictive of survival
hist(
  data$Growth,
  main = "Distribution of Plant Growth",
  xlab = "Growth (cm)"
)


# Survival vs plant size
# Boxplot chosen to compare distribution differences between survival classes
boxplot(
  Length.cm ~ Survival,
  data = data,
  main = "Plant Length by Survival Status",
  xlab = "Survival (0 = dead, 1 = alive)",
  ylab = "Length (cm)"
)

boxplot(
  Height.cm ~ Survival,
  data = data,
  main = "Plant Height by Survival Status",
  xlab = "Survival (0 = dead, 1 = alive)",
  ylab = "Height (cm)"
)


# Growth vs Survival
# Helps assess whether growth differences separate survival outcomes
boxplot(
  Growth ~ Survival,
  data = data,
  main = "Growth by Survival Status",
  xlab = "Survival (0 = dead, 1 = alive)",
  ylab = "Growth (cm)"
)



# Survival is a binary outcome (0 = dead, 1 = alive), so this is a classification problem. Because the relationships in the data are likely nonlinear and ecologically complex, and the predictors include both continuous and categorical variables, a Random Forest (using the ranger package) is the best fit. Compared to logistic regression or a single decision tree, it handles interactions better, reduces overfitting through averaging, and generally performs well without heavy tuning.





# STEP 3: CHECK MODELING ASSUMPTIONS (RANDOM FOREST)
# 1. Assumption: No missing values
# Random forests (ranger) cannot handle NA values directly
colSums(is.na(data))
# Result:
# Assumption is satisfied after QAQC cleaning



# 2. Assumption: Response variable is correctly defined
# Random forest classification requires a categorical/binary response
table(data$Survival)
# Result:
# Shows binary classification structure



# 3. Assumption: Predictors are not constant (zero variance)
# Variables with no variation provide no information to the model
sapply(data, function(x) length(unique(x)))
# Result:
# All predictors used in the model have >1 unique value (Year, Plot, Length.cm, Taxon, Height.cm, Length.cm.prev, x.Capitulesences, Growth)



# 4. Assumption: Predictors are appropriately formatted
# Categorical variables should be factors for proper splitting
str(data)
# Result:
# Year, Plot, Length.cm, Taxon, Height.cm, Length.cm.prev, x.Capitulesences, Growth variables should be factors or numeric



# 5. Assumption: No extreme data entry errors
# (already addressed in QAQC step, but re-checked here)
summary(data$Length.cm)
summary(data$Height.cm)
# Result:
# Values should be biologically realistic (no negatives, no impossible spikes)



# 6. Assumption: Observations are independent 
# Random forests assume independent observations, but: This dataset includes repeated
# measures of individuals (Tag over years),
# so strict independence is violated.
# However, random forests are robust to this and still perform well,
# Results should be interpreted with caution.



# FINAL CHECK SUMMARY 
# ---------------------------------------------------------
# 1. No missing data after QAQC - satisfied
# 2. Binary response variable confirmed - satisfied
# 3. Predictors show variation - satisfied
# 4. Variables are properly formatted - satisfied
# 5. Biological plausibility confirmed - satisfied
# 6. Independence assumption violated (repeated measures), but acceptable for predictive modeling using random forests




# STEP 4: RANDOM FOREST MODEL BUILDING
# Response: Survival (0/1 classification)
# Method: Random Forest (ranger)
library(ranger)

rf_data <- demog_rf


# Train/Test split 
set.seed(123)

train_index <- sample(1:nrow(rf_data), 0.7 * nrow(rf_data))

train <- rf_data[train_index, ]
test  <- rf_data[-train_index, ]


# BASELINE RANDOM FOREST MODEL
# Start with default settings from Chapter 11
# This gives a benchmark model before tuning
n_features <- length(setdiff(names(train), "Survival"))

rf_base <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,              
  mtry = floor(sqrt(n_features)),           
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

# View baseline performance (OOB error)
rf_base


# FIRST REFINEMENT (adjust mtry)
# Try a slightly wider range around default mtry as in Chapter 11
rf_mtry_low <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,
  mtry = floor(n_features / 3),
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

rf_mtry_high <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,
  mtry = floor(n_features / 2),
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

# Compare OOB errors
rf_base$prediction.error
rf_mtry_low$prediction.error
rf_mtry_high$prediction.error


# SECOND REFINEMENT (tree complexity)
# Adjust node size (controls tree depth)
# Smaller nodes = deeper trees

rf_nodesize_1 <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,
  mtry = floor(n_features / 3),
  min.node.size = 1,
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

rf_nodesize_5 <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,
  mtry = floor(n_features / 3),
  min.node.size = 5,
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

rf_nodesize_10 <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 10,
  mtry = floor(n_features / 3),
  min.node.size = 10,
  importance = "impurity",
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

# Compare performance
rf_nodesize_1$prediction.error
rf_nodesize_5$prediction.error
rf_nodesize_10$prediction.error



# Compare performance between all models in a table
model_comparison <- data.frame(
  Model = c(
    "Base",
    "mtry_low",
    "mtry_high",
    "nodesize_1",
    "nodesize_5",
    "nodesize_10"
  ),
  OOB_Error = c(
    rf_base$prediction.error,
    rf_mtry_low$prediction.error,
    rf_mtry_high$prediction.error,
    rf_nodesize_1$prediction.error,
    rf_nodesize_5$prediction.error,
    rf_nodesize_10$prediction.error
  )
)

# Results sorted from best to worst model
model_comparison <- model_comparison %>%
  arrange(OOB_Error)

model_comparison





# STEP 5: FINAL SELECTED MODEL
# Based on OOB comparison, mtry_high performed best
# Refit final model using best hyperparameter setting

rf_final <- ranger(
  Survival ~ .,
  data = train,
  num.trees = n_features * 15,        # increase trees for stability
  mtry = floor(n_features / 2),       # corresponds to mtry_high model
  min.node.size = 1,                  # kept from best-performing refinement
  importance = "permutation",        # final model uses permutation importance
  respect.unordered.factors = "order",
  probability = TRUE,
  seed = 123
)

# View final model output
rf_final



# STEP 6: TEST SET EVALUATION
# Predict class probabilities on test set
pred <- predict(rf_final, data = test)$predictions

# Convert probabilities to class labels using 0.5 threshold
pred_class <- ifelse(pred[,2] > 0.5, 1, 0)

# Convert actual values to numeric if needed for comparison
actual_class <- as.numeric(as.character(test$Survival))

# Confusion matrix to evaluate model performance
table(
  Predicted = pred_class,
  Actual = actual_class
)

# Overall accuracy
mean(pred_class == actual_class)






# STEP 7: MODEL DIAGNOSTICS (RANDOM FOREST FIT)
# 1. Out-of-Bag error
# OOB error is an internal cross-validation estimate
# It tells us how well the model generalizes without needing a test set
rf_final
# Interpretation:
# Lower OOB error indicates better predictive performance.
# Since the OOB error is stable and relatively low, the model is not overfitting.


# 2. Confusion matrix (test set performance)
# Compare predicted vs actual survival status
pred <- predict(rf_final, data = test)$predictions
pred_class <- ifelse(pred[,2] > 0.5, 1, 0)
actual_class <- as.numeric(as.character(test$Survival))

conf_matrix <- table(
  Predicted = pred_class,
  Actual = actual_class
)

conf_matrix
# Interpretation:
# This seems to be an extremely strong model. We see high values on the diagonal and no misrepresentation values. 


# 3. Overall accuracy
accuracy <- mean(pred_class == actual_class)
accuracy
# Interpretation:
# Accuracy provides a simple summary of classification performance.
# Higher values indicate better model fit to unseen data. In this case this model has a   1:1 fit 


# 4. Class balance check in predictions
table(pred_class)
# Interpretation:
# Ensures the model is not predicting only one class (e.g., all 0s or all 1s),
# which would indicate poor fit or class imbalance issues.
# There does seem to be some imbalance between survival 0 and 1 predictions here. 


# 5. Probability distribution check (model confidence)
hist(pred[,2],
     main = "Predicted Probability of Survival",
     xlab = "Probability (Class = 1)")
# Interpretation:
# Well-separated probabilities in this case suggest strong class separation




# STEP 8: INTERPRETATION OF MODEL OUTPUT

# Type: Probability estimation
# This means the model is doing classification by estimating the probability
# of survival (0 = dead, 1 = alive), instead of just assigning a single class label.
# This is useful because it gives more information about how confident the model is.

# Number of trees: 195
# This is how many decision trees are included in the forest.
# Each tree is built from a bootstrap sample of the training data.
# More trees generally improve stability by reducing variance across predictions.

# Sample size: 5505
# This is the number of observations used to train each tree.
# Each tree uses a bootstrap sample of the full dataset, which adds randomness
# and helps reduce overfitting.

# Number of independent variables: 13
# This is the total number of predictor variables in the model,
# including plant traits, environmental variables, and spatial/competition metrics.

# mtry: 6
# At each split, the model randomly selects 6 out of the 13 predictors to consider.
# This reduces correlation between trees and improves generalization.

# Target node size: 1
# This controls how deep each tree can grow.
# A value of 1 means trees are fully grown and can capture fine-scale patterns,
# increasing flexibility but relying on bagging to control overfitting.

# Variable importance mode: permutation
# Variable importance is calculated by randomly shuffling each predictor
# and measuring the drop in model accuracy.
# Larger drops indicate more important variables.

# Splitrule: gini
# The Gini impurity is used to choose splits within each tree.
# It selects splits that best separate survival classes at each node.

# OOB prediction error (Brier score): 6.33423e-06
# This is the out-of-bag error estimate based on unseen data during training.
# The Brier score measures how accurate the predicted probabilities are.
# Lower values indicate better performance and calibration.

# Overall interpretation
# Overall, the model performs very well with very low OOB error.
# The ensemble structure (195 trees) combined with random feature selection (mtry = 6)
# helps balance flexibility and generalization.
# Permutation importance also allows for ecological interpretation of key predictors.








# STEP 9: REFINED FIGURES
library(ggplot2)


# FIGURE 1: Survival vs Key Continuous Predictors
# I chose this because random forest performance depends heavily on
# how predictors differ between classes (0 vs 1 survival)
p1 <- ggplot(data, aes(x = Survival, y = Length.cm, fill = Survival)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +  # colorblind-safe palette
  labs(
    title = "Plant Length by Survival Status",
    x = "Survival (0 = dead, 1 = alive)",
    y = "Length (cm)"
  ) +
  theme_minimal()

p2 <- ggplot(data, aes(x = Survival, y = Growth, fill = Survival)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Plant Growth by Survival Status",
    x = "Survival (0 = dead, 1 = alive)",
    y = "Growth (cm)"
  ) +
  theme_minimal()

# Display side-by-side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)




# FIGURE 2: Environmental structure (within available data)
# I chose this to explore whether survival differs across:
# species (Taxon)
# space (Plot)
# time (Year)
p3 <- ggplot(data, aes(x = Taxon, fill = Survival)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Survival Proportion by Species (Taxon)",
       x = "Taxon",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(data, aes(x = Year, fill = Survival)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Survival Proportion by Year",
       x = "Year",
       y = "Proportion") +
  theme_minimal()

grid.arrange(p3, p4, ncol = 2)








