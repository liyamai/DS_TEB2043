# Activity 3: Name and Phone Number Formatter

# Get name and phone number from user
name <- readline(prompt = "Enter your name: ")
phone <- readline(prompt = "Enter your phone number: ")

# Convert name to uppercase
name_upper <- toupper(name)

# Extract first 3 and last 4 digits from phone number
# Remove any non-digit characters first
phone_digits <- gsub("[^0-9]", "", phone)

if (nchar(phone_digits) >= 7) {
  first_three <- substr(phone_digits, 1, 3)
  last_four <- substr(phone_digits, nchar(phone_digits)-3, nchar(phone_digits))
  formatted_phone <- paste(first_three, "- xxxxx", last_four)
} else {
  formatted_phone <- "Invalid phone number"
}

# Display formatted output
cat("Hi,", name_upper, ". A verification code has been sent to", formatted_phone, "\n")
