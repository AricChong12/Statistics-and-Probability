# --- Question 4 ---
# --- Load Data ---
# This will open a window - select your 'transport_dataset.csv' file
transport_df <- read.csv(file.choose())

# --- ACTUAL ANSWER FOR PART A ---

# Set a seed so your random selection is the same every time you run it
set.seed(123) 

# Select 200 observations randomly without replacement
sample_data <- transport_df[sample(nrow(transport_df), 200, replace = FALSE), ]

# Define variables for easier access
y <- sample_data$Travel_Time
x1 <- sample_data$Distance_km
x2 <- sample_data$Avg_Speed
x3 <- sample_data$Traffic_Density

# --- PART B: Scatter Plots ---

# Setup a 1 row, 3 column viewing area so plots appear side-by-side
par(mfrow=c(1,3)) 

# Plot 1: Time vs Distance (x1)
plot(x1, y,
     main = "Travel Time vs Distance (y vs x1)",
     xlab = "Distance (km)", ylab = "Travel Time (min)",
     col = "blue", pch = 19)

# Plot 2: Time vs Avg Speed (x2)
plot(x2, y,
     main = "Travel Time vs Avg Speed (y vs x2)",
     xlab = "Avg Speed (km/h)", ylab = "Travel Time (min)",
     col = "red", pch = 19)

# Plot 3: Time vs Traffic Density (x3)
plot(x3, y,
     main = "Travel Time vs Traffic Density (y vs x3)",
     xlab = "Traffic Density", ylab = "Travel Time (min)",
     col = "green", pch = 19)

# Reset viewing area
par(mfrow=c(1,1))

# --- PART C: Fit Linear Models ---

# Model 1: Distance
model_distance <- lm(y ~ x1)
summary(model_distance)

# Plot the points
plot(x1, y,
     main = "Travel Time vs Distance (y vs x1)",
     xlab = "Distance (km)", ylab = "Travel Time (min)",
     col = "blue", pch = 19)

# Add the regression line
abline(model_distance, col="red", lwd=2)

# Model 2: Speed
model_speed <- lm(y ~ x2)
summary(model_speed)

# Plot the points
plot(x2, y,
     main = "Travel Time vs Avg Speed (y vs x2)",
     xlab = "Avg Speed (km/h)", ylab = "Travel Time (min)",
     col = "red", pch = 19)

# Add the regression line
abline(model_speed, col="blue", lwd=2)

# Model 3: Traffic Density
model_density <- lm(y ~ x3)
summary(model_density)

# Plot the points
plot(x3, y,
     main = "Travel Time vs Traffic Density (y vs x3)",
     xlab = "Traffic Density", ylab = "Travel Time (min)",
     col = "green", pch = 19)

# Add the regression line
abline(model_density, col="red", lwd=2)

# --- PART D: Interpret slope coefficients and test for significance ---
# Define a custom function to print the test results automatically
test_hypothesis <- function(model, model_name) {
  
# Get the summary of the model
summ <- summary(model)
  
# Extract the coefficients table
# Row 2 contains the slope (independent variable), Col 4 contains the p-value
slope_estimate <- summ$coefficients[2, "Estimate"]
p_value <- summ$coefficients[2, "Pr(>|t|)"]
  
# Print the setup
cat("TESTING MODEL:", model_name, "\n")
cat("Estimated Slope (b):", slope_estimate, "\n")
cat("P-value:", p_value, "\n")
  
# Make the Decision
alpha <- 0.05
  
if (p_value < alpha) {
  cat("Result: P-value < 0.05\n")
  cat("DECISION: REJECT the Null Hypothesis (H0).\n")
  cat("CONCLUSION: The variable is a statistically significant predictor of Travel Time.\n")
  } 
else {
  cat("Result: P-value > 0.05\n")
  cat("DECISION: FAIL TO REJECT the Null Hypothesis (H0).\n")
  cat("CONCLUSION: The variable is not a statistically significant predictor of Travel Time.\n")
  }
}

test_hypothesis(model_distance, "Distance")
test_hypothesis(model_speed, "Average Speed")
test_hypothesis(model_density, "Traffic Density")

# --- PART E: r and r-squared ---

# Correlations
r1 <- cor(y, x1)
r2 <- cor(y, x2)
r3 <- cor(y, x3)

# Print values
cat("Correlation (r) for Distance:", r1, "\n")
cat("Correlation (r) for Speed:", r2, "\n")
cat("Correlation (r) for Traffic:", r3, "\n")

# R-squared values can be read directly from the summary(model) outputs
cat("R-squared for Distance:", summary(model_distance)$r.squared, "\n")
cat("R-squared for Speed:", summary(model_speed)$r.squared, "\n")
cat("R-squared for Traffic:", summary(model_density)$r.squared, "\n")

