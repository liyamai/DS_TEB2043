# Activity 1: BMI Calculator

# Get weight and height from user
weight <- as.numeric(readline(prompt = "Enter weight (kg): "))
height <- as.numeric(readline(prompt = "Enter height (m): "))
# Calculate BMI
bmi <- weight / (height^2)

# Determine BMI category
underweight <- bmi <= 18.4
normal <- bmi >= 18.5 & bmi <= 24.9
overweight <- bmi >= 25.0 & bmi <= 39.9
obese <- bmi >= 40.0

# Display results
cat("Underweight:", underweight, "\n")
cat("Normal:", normal, "\n")
cat("Overweight:", overweight, "\n")
cat("Obese:", obese, "\n")

# Additional: Display BMI value
cat("\nBMI:", round(bmi, 2), "\n")
