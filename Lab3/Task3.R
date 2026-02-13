# Create an extended list with Chemistry and Physics scores
extended_records <- list(
  names = c("Robert", "Hemsworth", "Scarlett", "Evans", "Pratt", 
           "Larson", "Holland", "Paul", "Simu", "Renner"),
  exam_score = c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59),
  chemistry = c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59),
  physics = c(89, 86, 65, 52, 60, 67, 40, 77, 90, 61)
)

# Display the extended list
cat("Extended Student Records:\n")
print(data.frame(
  Student = extended_records$names,
  Exam_Score = extended_records$exam_score,
  Chemistry = extended_records$chemistry,
  Physics = extended_records$physics
))
cat("\n")

# Count students who failed Chemistry and Physics (<=49)
chem_fail <- sum(extended_records$chemistry <= 49)
physics_fail <- sum(extended_records$physics <= 49)

cat("Number of students who failed Chemistry (<=49):", chem_fail, "\n")
cat("Number of students who failed Physics (<=49):", physics_fail, "\n")
cat("\n")

# Find highest scores for both subjects
chem_highest <- max(extended_records$chemistry)
physics_highest <- max(extended_records$physics)

# Find students with highest scores in Chemistry
chem_top_students <- extended_records$names[extended_records$chemistry == chem_highest]

# Find students with highest scores in Physics
physics_top_students <- extended_records$names[extended_records$physics == physics_highest]

cat("Highest Chemistry Score:", chem_highest, "\n")
cat("Student(s) with highest Chemistry score:", paste(chem_top_students, collapse = ", "), "\n")
cat("\n")
cat("Highest Physics Score:", physics_highest, "\n")
cat("Student(s) with highest Physics score:", paste(physics_top_students, collapse = ", "), "\n")
