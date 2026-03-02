# Q1: Create data frame for table
# Create vectors for each column
names <- c("Anastasia", "Dima", "Michael", "Matthew", "Laura", "Kevin", "Jonas")
score <- c(12.5, 9.0, 16.5, 12.0, 9.0, 8.0, 19.0)
attempts <- c(1, 3, 2, 3, 2, 1, 2)

# Create data frame
df <- data.frame(
  name = names, # Column name -> data stored
  score = score,
  attempts = attempts
)

# Display the data frame
print("Data Frame:")
print(df)
cat("\n")

# Q2: Add new column -> qualify
# Create the qualify vector
qualify <- c("yes", "no", "yes", "no", "no", "no", "yes")

# Add column to df
# $ : used to access, add, or modify columns in a data frame -> (dataframe_name$column_name)
df$qualify <- qualify

# Display updated data frame
print("Updated Data Frame With Qualify Column:")
print(df)
cat("\n")

# Q3: Add new row
# rbind stacks data frames vertically, adding new rows at the bottom.
new_row <- data.frame(
  name = "Emily",
  score = 14.5,
  attempts = 1,
  qualify = "yes"
)

# Add row to data frame
df <- rbind(df, new_row)

# Display final data frame
print("With new row added:")
print(df)
cat("\n")

# Q4: Display structures, summary, and dimensions

# Display structure
# str(): displays the internal structure of an R object: object type, num of row and columns, name and data type, first few values

print("Structure of the data frame:")
str(df)
cat("\n")

# Display summary (before changing data types)
print("Summary (before changing):")
summary(df)
cat("\n")

# Convert qualify to factor for better summary
# A factor is a special data type in R used to store categorical data - data that can be divided into distinct groups or categories. Think of it as R's way of handling "labels" or "categories" efficiently.

df$qualify <- as.factor(df$qualify)

# Display summary after type conversion
print("Summary (after converting qualify to a factor data type):")
summary(df)
cat("\n")
