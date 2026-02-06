
# ============================================================
# QUESTION 2a: SERVER PERFORMANCE ANALYSIS
# ============================================================

df <- read.csv("ServerResponseTimes.csv")
head(df)

# ------------------------------------------------------------
# Question 2a
# ------------------------------------------------------------

# Set seed for reproducibility (so you get the same random sample every time)
set.seed(1234)

# Randomly select 50 distinct indices for each service
# sample() selects without replacement by default, ensuring no duplicates
indices_X <- sample(1:nrow(df), 50)
indices_Y <- sample(1:nrow(df), 50)
indices_Z <- sample(1:nrow(df), 50)

# Extract the values (Note: R replaces spaces in column names with dots)
sample_X <- df$Server.X[indices_X]
sample_Y <- df$Server.Y[indices_Y]
sample_Z <- df$Server.Z[indices_Z]

# Show the first few values to confirm
cat("First 5 values of Sample X:", head(sample_X), "\n")


# ------------------------------------------------------------
# Question 2b
# ------------------------------------------------------------

# Calculate Mean and Standard Deviation
mean_X <- mean(sample_X)
sd_X   <- sd(sample_X)

mean_Y <- mean(sample_Y)
sd_Y   <- sd(sample_Y)

mean_Z <- mean(sample_Z)
sd_Z   <- sd(sample_Z)

# Print Descriptive Statistics
cat("\n--- Descriptive Statistics ---\n")
cat(sprintf("Service X: Mean = %.2f ms, SD = %.2f\n", mean_X, sd_X))
cat(sprintf("Service Y: Mean = %.2f ms, SD = %.2f\n", mean_Y, sd_Y))
cat(sprintf("Service Z: Mean = %.2f ms, SD = %.2f\n", mean_Z, sd_Z))

# Create a Boxplot to compare the three services
boxplot(sample_X, sample_Y, sample_Z,
        names = c("Service X", "Service Y", "Service Z"),
        main = "Comparison of API Response Times (n=50)",
        xlab = "Microservices",
        ylab = "Response Time (ms)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Add a red dashed line at 400ms (the performance target)
abline(h = 400, col = "red", lty = 2, lwd = 2)
legend("topright", legend="Target (400ms)", col="red", lty=2, lwd=2)


# ------------------------------------------------------------
# Question 2c
# ------------------------------------------------------------
# H0: mu >= 400 (The server is slow/average)
# H1: mu < 400  (The server is significantly fast)

cat("\n--- Hypothesis Test Results (alpha = 0.05) ---\n")

# Test for Service X
test_X <- t.test(sample_X, mu = 400, alternative = "less", conf.level = 0.95)
print(test_X)

# Test for Service Y
test_Y <- t.test(sample_Y, mu = 400, alternative = "less", conf.level = 0.95)
print(test_Y)

# Test for Service Z
test_Z <- t.test(sample_Z, mu = 400, alternative = "less", conf.level = 0.95)
print(test_Z)

