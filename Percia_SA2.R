# Load required libraries
install.packages("ggplot2")
install.packages("anytime")
library(anytime)
library(ggplot2)

# Load Bitcoin trading data (Example data provided)
bitcoin_data <- read.csv("C:/Users/john/Downloads/what/SA2_1_data.csv")

# Preprocess data (remove NAs, convert timestamps, etc.)
bitcoin_data <- na.omit(bitcoin_data)
bitcoin_data$Timestamp <- anytime(bitcoin_data$Timestamp)

# Calculate daily returns
bitcoin_data$Mid <- (bitcoin_data$High - bitcoin_data$Low) / 2 + bitcoin_data$Low

# Remove NA or NaN values from returns
bitcoin_data <- bitcoin_data[!is.na(bitcoin_data$Mid), ]

# Calculate returns
bitcoin_data$Return <- c(NA, diff(bitcoin_data$Mid) / bitcoin_data$Mid[-nrow(bitcoin_data)])

# Remove NA or NaN values from returns
bitcoin_data <- bitcoin_data[!is.na(bitcoin_data$Return), ]

# Plot histogram of returns
library(ggplot2)
ggplot(bitcoin_data, aes(x = Return)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Bitcoin Returns",
       x = "Returns", y = "Frequency")

# QQ plot
qqnorm(bitcoin_data$Return)
qqline(bitcoin_data$Return)

# Fit Normal Distribution
normal_fit <- tryCatch({
  mean_return <- mean(bitcoin_data$Return)
  sd_return <- sd(bitcoin_data$Return)
  list(mean = mean_return, sd = sd_return)
}, error = function(e) { NULL })

if (!is.null(normal_fit)) {
  print("Normal Distribution:")
  print(normal_fit)
} else {
  print("Failed to fit Normal distribution.")
}

# Fit Student's T Distribution
student_fit <- tryCatch({
  # Manually estimate parameters (degrees of freedom)
  df <- 10 # Example starting value
  list(df = df)
}, error = function(e) { NULL })

if (!is.null(student_fit)) {
  print("Student's T Distribution:")
  print(student_fit)
} else {
  print("Failed to fit Student's T distribution.")
}

# Fit Laplace Distribution
laplace_fit <- tryCatch({
  # Manually estimate parameters (mean and scale)
  mean_return <- median(bitcoin_data$Return)
  scale <- median(abs(bitcoin_data$Return - mean_return))
  list(mean = mean_return, scale = scale)
}, error = function(e) { NULL })

if (!is.null(laplace_fit)) {
  print("Laplace Distribution:")
  print(laplace_fit)
} else {
  print("Failed to fit Laplace distribution.")
}

# Fit Tsallis Distribution (if available)
# Function for Tsallis distribution fitting manually (if available)
tsallis_fit <- tryCatch({
  # Manually estimate parameters
  # You may need to provide your own implementation or use a different method
  NULL
}, error = function(e) { NULL })

if (!is.null(tsallis_fit)) {
  print("Tsallis Distribution:")
  print(tsallis_fit)
} else {
  print("Tsallis distribution fitting not available.")
}

# Fit Power Law Distribution (if available)
# Function for Power Law distribution fitting manually (if available)
powerlaw_fit <- tryCatch({
  # Manually estimate parameters
  # You may need to provide your own implementation or use a different method
  NULL
}, error = function(e) { NULL })

if (!is.null(powerlaw_fit)) {
  print("Power Law Distribution:")
  print(powerlaw_fit)
} else {
  print("Power Law distribution fitting not available.")
}

# Conclusion
# Provide a summary of the findings and discuss any limitations or implications
