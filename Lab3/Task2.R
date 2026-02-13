# Create a list 
student_records <- list(
  names = c("Robert", "Hemsworth", "Scarlett", "Evans", "Pratt", 
           "Larson", "Holland", "Paul", "Simu", "Renner"),
  scores = c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59)
)

# Display the list structure
cat("Student Records List:\n")
print(student_records)
cat("\n")

# Calculate statistics
highest_score <- max(student_records$scores)
lowest_score <- min(student_records$scores)
average_score <- mean(student_records$scores)

# Find students with highest and lowest scores
highest_students <- student_records$names[student_records$scores == highest_score]
lowest_students <- student_records$names[student_records$scores == lowest_score]

# Display results
cat("Highest Score:", highest_score, "\n")
cat("Lowest Score:", lowest_score, "\n")
cat("Average Score:", round(average_score, 2), "\n")
cat("Student(s) with highest score:", paste(highest_students, collapse = ", "), "\n")
cat("Student(s) with lowest score:", paste(lowest_students, collapse = ", "), "\n")
