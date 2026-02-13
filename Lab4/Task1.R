age <- c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60,
        52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58)

age_factor <- factor(age) # To find out there are 10 levels

print(levels(age_factor))

# Make table
age_table <- table(age_factor)
print(age_table)

age_range <- cut(age, breaks = c(50, 52, 54, 56, 58, 60), include.lowest = TRUE, right = TRUE)

age_range_table <- table(age_range)
print(age_range_table)
