exam_scores <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54)

# Display 
cat("Input vector:", exam_scores, "\n\n")

# Count by grade
grade_A <- sum(exam_scores >= 90 & exam_scores <= 100)
grade_B <- sum(exam_scores >= 80 & exam_scores <= 89)
grade_C <- sum(exam_scores >= 70 & exam_scores <= 79)
grade_D <- sum(exam_scores >= 60 & exam_scores <= 69)
grade_E <- sum(exam_scores >= 50 & exam_scores <= 59)
grade_F <- sum(exam_scores <= 49)

# Summary table
grade_summary <- data.frame(
  Score = c("90-100", "80-89", "70-79", "60-69", "50-59", "<=49"),
  Grade = c("A", "B", "C", "D", "E", "F"),
  Number_of_students = c(grade_A, grade_B, grade_C, grade_D, grade_E, grade_F)
)

cat("Grade Distribution:\n")
print(grade_summary)
cat("\n")

# Check if each student passed (>49) or not
passed <- exam_scores > 49
cat("Pass/Fail status (TRUE = Pass, FALSE = Fail):\n")
print(passed)
cat("\n")

# Count pass/fail
num_passed <- sum(passed)
num_failed <- length(exam_scores) - num_passed
cat("Number of students passed:", num_passed, "\n")
cat("Number of students failed:", num_failed, "\n")
