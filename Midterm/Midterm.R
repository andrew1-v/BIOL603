


# QA/QC PROCESSING FOR NEON TICK DATASET
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(maps)


# 1. IMPORT DATA
# Load main dataset
data <- read.csv("Midterm_data.csv", stringsAsFactors = FALSE)

# Load category codes 
codes <- read.csv("Data category codes.csv", stringsAsFactors = FALSE)

# RENAME COLUMNS FOR CONSISTENCY
colnames(data) <- c(
  "uid",
  "location_name",
  "domain_id",
  "site_id",
  "plot_id",
  "plot_type",
  "landcover_class",
  "latitude",
  "longitude",
  "datum",
  "coord_uncertainty",
  "elevation_m",
  "elev_uncertainty",
  "sampling_impractical",
  "biophysical_criteria",
  "collection_date",
  "event_id",
  "sample_id",
  "sample_code",
  "sampling_method",
  "sampled_area",
  "target_taxa_present",
  "adult_count",
  "nymph_count",
  "larva_count",
  "sample_condition",
  "protocol_version",
  "measured_by",
  "remarks"
)

# Check names
colnames(data)

# 2. INITIAL DATA INSPECTION
# View structure and column types
str(data)

# View first few rows
head(data)

# Summary statistics
summary(data)

# Comment:
# The initial inspection I did helps identify incorrect data types, missing values,
# and general structure of the dataset before cleaning.


# 3. CHECK AND FIX DATA TYPES
# Convert date column 
data$collection_date <- as.Date(data$collection_date)

# Create total tick abundance from life stages
data$total_count <- rowSums(
  data[, c("adult_count", "nymph_count", "larva_count")],
  na.rm = TRUE
)
data$total_count <- as.numeric(data$total_count)

# Convert categorical variables to factors
data$plot_type <- as.factor(data$plot_type)
data$sampling_method <- as.factor(data$sampling_method)

# Comment:
# I am ensuring correct data types for accurate analysis and modeling.


# 4. HANDLE MISSING VALUES
# Count missing values per column
colSums(is.na(data))

# Inspect rows with missing values
data_na <- data %>% filter(if_any(everything(), is.na))
head(data_na)

# Comment:
# The missing values might represent either true absence or failed sampling.
# These should be distinguished before removal.

# Remove rows where key variables are missing
data_clean <- data %>%
  filter(!is.na(total_count))


# 5. CHECK FOR DUPLICATES
# Identify duplicate rows
duplicates <- data_clean %>%
  duplicated()

sum(duplicates)
# No duplicates

# Comment:
# I checked this because duplicate records can bias abundance estimates and should be removed.


# 6. VALIDATE VALUES (OUTLIERS / ERRORS)
# Check for negative counts
data_clean %>% filter(total_count < 0)

# Check distribution of counts
summary(data_clean$total_count)

# Visual check for outliers
boxplot(data_clean$total_count)

# Comment:
# Counts should probably not be negative. Extremely large values should be reviewed
# to make sure that they are biologically realistic and not data entry errors.


# 7. CLEAN CATEGORICAL VARIABLES
# Standardize text 
data_clean$plot_type <- tolower(trimws(data_clean$plot_type))
data_clean$sampling_method <- tolower(trimws(data_clean$sampling_method))

# Comment:
# Standardizing the categorical variables makes sure there is consistency and prevents
# duplication of categories due to formatting differences.



# 8. CHECK SAMPLING STRUCTURE
# Count number of observations per event
sampling_check <- data_clean %>%
  group_by(event_id) %>%
  summarise(n_samples = n())

summary(sampling_check$n_samples)

# Comment:
# Sampling design usually expects a consistent number of plots per event.
# END OF QA/QC SECTION





# 1. SUMMARY STATISTICS
# Summary of total tick abundance
summary(data_clean$total_count)

# Mean and standard deviation
mean(data_clean$total_count, na.rm = TRUE)
sd(data_clean$total_count, na.rm = TRUE)

# Comment:
# These statistics provide a general understanding of tick abundance,
# including central tendency (mean) and variability (standard deviation).


# Summary by life stage
summary(data_clean$adult_count)
summary(data_clean$nymph_count)
summary(data_clean$larva_count)

# Comment:
# Examining life stage counts separately helps identify which stage
# contributes most to overall abundance.



# 2. SUMMARY BY GROUPS
# Mean abundance by sampling method
data_clean %>%
  group_by(sampling_method) %>%
  summarise(mean_abundance = mean(total_count, na.rm = TRUE))

# Comment:
# This evaluates whether differences in sampling method potentially influence observed counts.


# 3. HISTOGRAM OF TICK ABUNDANCE
hist(data_clean$total_count,
     main = "Distribution of Total Tick Abundance",
     xlab = "Total Tick Count",
     breaks = 20)

# Comment:
# I am using a histogram to visualize the distribution of tick counts,
# which helps identify skewness and the presence of many zero or low values.




# 4. BOXPLOT BY SAMPLING METHOD
boxplot(total_count ~ sampling_method,
        data = data_clean,
        main = "Tick Abundance by Sampling Method",
        xlab = "Sampling Method",
        ylab = "Total Tick Count")

# Comment:
# This plot observes whether sampling method may introduce bias or show large distribution differences in observed tick abundance.



# 5. SCATTER PLOT
plot(data_clean$longitude,
     data_clean$latitude,
     pch = 16,
     xlab = "Longitude",
     ylab = "Latitude",
     main = "Sampling Locations")

# Comment:
# A scatter plot of coordinates visualizes the spatial distribution
# of sampling points across the study area.


# 6. LOCATION MAP
# Get US state map data
states <- map_data("state")

# Filter AL
al <- subset(states, region == "alabama")

ggplot() +
  geom_polygon(data = al,
               aes(x = long, y = lat, group = group),
               fill = "gray90",
               color = "black") +
  geom_point(data = data_clean,
             aes(x = longitude, y = latitude),
             color = "red",
             size = 2,
             alpha = 0.7) +
  coord_fixed(1.3) +
  labs(title = "Sampling Locations",
       x = "Longitude",
       y = "Latitude")

# Comment: 
# A map showing the location of the field site helps understand biological tick distributions and relevance in data 



# 7. ABUNDANCE VS ELEVATION
plot(data_clean$elevation_m,
     data_clean$total_count,
     pch = 16,
     xlab = "Elevation (m)",
     ylab = "Total Tick Count",
     main = "Tick Abundance vs Elevation")

# Comment:
# This plot explores whether elevation may influence tick abundance,
# which is ecologically relevant for species distribution.

# END OF SUMMARY STATISTICS SECTION



# ZERO-INFLATED MODELING APPROACH 

# MODEL ASSUMPTION CHECKS FOR ZERO-INFLATED NEGATIVE BINOMIAL

# 1. CHECK RESPONSE VARIABLE TYPE
summary(data_clean$total_count)

# Check that values are whole numbers and non-negative
all(data_clean$total_count >= 0)

# Comment:
# The response variable represents total count data (tick abundance),
# which is non-negative and will work for a count-based model.



# 2. CHECK FOR OVERDISPERSION
mean_tc <- mean(data_clean$total_count)
var_tc <- var(data_clean$total_count)
mean_tc
var_tc

# Comment:
# The variance is much larger than the mean, this indicates overdispersion,
# supporting the use of a Negative Binomial model instead of Poisson.



# 3. CHECK ZERO-INFLATION
zero_prop <- mean(data_clean$total_count == 0)
zero_prop

# Comment:
# This has a high proportion of zero values which suggests zero inflation,
# meaning a standard count model (Poisson or NB) may be insufficient.



# 4. VISUAL CHECK OF DISTRIBUTION
hist(data_clean$total_count,
     main = "Distribution of Tick Abundance",
     xlab = "Total Tick Count",
     breaks = 20)

# Comment:
# The histogram is used to visually assess skewness and the presence of excess zeros.


# 5. CHECK INDEPENDENCE OF OBSERVATIONS
table(data_clean$event_id)

# Comment:
# Each event represents an independent sampling unit.


# 6. CHECK FOR IMPOSSIBLE VALUES
data_clean %>% filter(total_count < 0)

# Comment:
# No negative values  exist in a valid count dataset.
# This confirms biological and logical validity of the response variable.





# ZERO-INFLATED NEGATIVE BINOMIAL MODEL BUILDING
# Load required package
library(glmmTMB)


# 1. BASE (NULL) MODEL
# Start with intercept-only model to establish baseline fit
model_null <- glmmTMB(
  total_count ~ 1,
  ziformula = ~1,
  family = nbinom2,
  data = data_clean
)

summary(model_null)

# Comment:
# This null model estimates overall mean abundance without other predictors.
# It serves as a baseline for comparing more complex models.



# 2. ADD SINGLE ECOLOGICAL PREDICTOR (ELEVATION)
model_elev <- glmmTMB(
  total_count ~ elevation_m,
  ziformula = ~1,
  family = nbinom2,
  data = data_clean
)

summary(model_elev)

# Comment:
# Elevation is included as a biologically relevant predictor,
# as tick abundance is often influenced by environmental gradients such as elevation.



# 3. ADD HABITAT STRUCTURE (LANDCOVER CLASS)
data_clean$landcover_class <- as.factor(data_clean$landcover_class)

model_habitat <- glmmTMB(
  total_count ~ elevation_m + landcover_class,
  ziformula = ~1,
  family = nbinom2,
  data = data_clean
)

summary(model_habitat)

# Comment:
# Landcover class is included as a categorical predictor to account for
# differences in habitat type that may influence tick abundance. 



# 4. ADD SAMPLING EFFECTS
model_full <- glmmTMB(
  total_count ~ elevation_m + landcover_class + sampling_method,
  ziformula = ~1,
  family = nbinom2,
  data = data_clean
)

summary(model_full)

# Comment:
# Sampling method is included to control for potential sampling bias
# introduced by different field collection techniques.



# 5. ADD ZERO-INFLATION PREDICTOR STRUCTURE
model_zinb_final <- glmmTMB(
  total_count ~ elevation_m + landcover_class + sampling_method,
  ziformula = ~ elevation_m,
  family = nbinom2,
  data = data_clean
)

summary(model_zinb_final)

# Comment:
# A zero-inflation component is added using elevation,
# This allows for the probability of structural zeros to vary across environment.
# This also reflects ecological reality where some sites may be unsuitable for ticks.



# 6. MODEL COMPARISON (LIKELIHOOD-BASED)
AIC(model_null, model_elev, model_habitat, model_full, model_zinb_final)

# Comment:
# AIC is used to compare models; lower values indicate better fit



# 7. FINAL MODEL SELECTION
summary(model_zinb_final)

# Comment:
# The final selected model is the zero-inflated negative binomial model
# with elevation, habitat type, and sampling method as predictors,
# and elevation included in the zero-inflation component.




# 8. MODEL DIAGNOSTICS
# Check convergence of the model
model_zinb_final$sdr$pdHess

# Comment:
# This value indicates whether the Hessian matrix is positive definite (TRUE).
# A TRUE result suggests the model converged without major numerical issues
# during optimization. However, convergence alone doesn’t mean the model is
# necessarily the best fit for the data, so it should still be interpreted
# alongside other diagnostics (like AIC and residual checks).


# Check residuals (a basic diagnostic check)
res <- resid(model_zinb_final)
hist(res,
     main = "Residual Distribution (ZINB Model)",
     xlab = "Residuals")

# Comment:
# The residuals are approximately centered around zero.
# If there were strong skew or extreme values, it may indicate poor model fit or unaccounted structure in the data.


# Fitted vs observed values
plot(fitted(model_zinb_final),
     data_clean$total_count,
     xlab = "Fitted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted Tick Counts")

abline(0, 1, col = "red")

# Comment:
# This plot evaluates how well the model predicts observed counts.
# The points close to the 1:1 line indicate good model fit, which is seen in the lower values. 


# Check overdispersion
mean(data_clean$total_count)
var(data_clean$total_count)

# Comment:
# The variance is still substantially greater than the mean,
# So some overdispersion may remain, but in this case is typically handled by the negative binomial structure.


# Check proportion of zeros vs fitted expectation
mean(data_clean$total_count == 0)

# Comment:
# A high observed zero proportion compared to model expectation supports the use of a zero inflated structure.






# FINAL MODEL OUTPUT INTERPRETATION
summary(model_zinb_final)
# The model I ended up using is a zero-inflated negative binomial (ZINB) with a log link.
# I chose this because the response variable is count data with a lot of zeros and clear overdispersion.

# The dispersion parameter (0.138) shows pretty strong overdispersion in the data,
# which is why a negative binomial model makes more sense here than a Poisson model.

# Model fit statistics:
# AIC = 771.5, BIC = 796.6, logLik = -377.8
# These values suggest the model is fitting the data reasonably well,
# and the lower AIC compared to simpler models supports the added complexity.


# CONDITIONAL MODEL (COUNT PROCESS)
# (Intercept) = 2.78963 (p < 0.001)
# This is the baseline expected log tick count when predictors are at their reference levels.
# It’s highly significant, so there is a clear baseline level of tick abundance.

# elevation_m = -0.01164 (p = 0.515)
# Elevation doesn’t look like it has a significant effect on tick abundance in this model.

# landcover_class (Woody Wetlands) = -1.45197 (p = 0.00322 **)
# Woody wetlands have significantly lower tick abundance compared to the reference class,
# which suggests habitat type is an important factor.

# sampling_method (drag and flag) = 2.62162 (p < 0.001 ***)
# This method gives significantly higher observed counts, likely because it is more effective
# at collecting ticks.

# sampling_method (flag) = -2.63601 (p = 0.362)
# This isn’t statistically significant, so there isn’t strong evidence that it differs from the reference.


# ZERO-INFLATION MODEL (STRUCTURAL ZEROS)
# Intercept = -7.4181 (p = 1)
# Elevation_m = -0.9377 (p = 1)
# Neither of these are significant, so there isn’t strong evidence that elevation explains
# where structural zeros are occurring in the dataset.


# OVERALL INTERPRETATION
# Overall, tick abundance seems to be mainly driven by landcover type and sampling method.
# Elevation doesn’t have a meaningful effect in either the count or zero-inflation part of the model.
# The strong overdispersion supports using a negative binomial structure,
# and the zero-inflated part helps account for the large number of zeros in the data.

# In general, the ZINB model seems to fit the structure of the dataset well,
# especially given the overdispersion and high frequency of zero counts.







# FINAL FIGURE: MODEL-BASED PREDICTED TICK ABUNDANCE
# Create prediction grid across key variables
newdata <- expand.grid(
  elevation_m = seq(min(data_clean$elevation_m, na.rm = TRUE),
                    max(data_clean$elevation_m, na.rm = TRUE),
                    length.out = 100),
  landcover_class = unique(data_clean$landcover_class),
  sampling_method = unique(data_clean$sampling_method)
)

# Predict from model (response scale = actual counts)
newdata$predicted <- predict(model_zinb_final,
                             newdata = newdata,
                             type = "response")

# Plot
ggplot(newdata,
       aes(x = elevation_m,
           y = predicted,
           color = landcover_class)) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(~ sampling_method) +
  
  scale_color_brewer(palette = "Dark2") +
  
  labs(
    title = "Predicted Tick Abundance Across Elevation",
    x = "Elevation (m)",
    y = "Predicted Total Tick Count",
    color = "Landcover Class"
  ) +
  
  theme_minimal()

# Comment:
# I used model predictions instead of the raw data because it gives a clearer
# picture of what the ZINB model is actually estimating.
#
# Elevation is treated as a continuous variable, while landcover class and
# sampling method are used to separate out ecological differences and
# potential sampling effects.
#
# This helps cut through the noise from all the zeros in the dataset and
# makes the overall trends from the model easier to see and interpret.








