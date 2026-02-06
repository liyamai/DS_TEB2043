# Activity 2: String Comparison

cat("This program compares 2 strings.\n")

# Get two strings from user
string1 <- readline(prompt = "Enter string 1: ")
string2 <- readline(prompt = "Enter string 2: ")

# Compare strings case-insensitively
are_similar <- tolower(string1) == tolower(string2)

# Display result
cat("Both inputs are similar:", are_similar, "\n")
