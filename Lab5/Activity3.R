# Checking Armstrong Number 

# Get input
number_input <- readline(prompt="Input an integer: ")
number <- as.numeric(number_input)

# Check if valid input
if(is.na(number) || number < 0) {
  print("Invalid. Please enter a positive integer")
} else {
  
  # Splitting into digits
  num_str <- as.character(number) # Convert to char
  digits <- as.numeric(strsplit(num_str, "")[[1]]) # Split & reconvert to num
  n_digits <- length(digits) # Get n of digits to determine n of power
  
  # Find sum of powers
  sum_of_powers <- sum(digits^n_digits)
  
  # Check result
  if(sum_of_powers == number) {
    print(paste(number, "is an Armstrong number!"))
    
    # Calculation for reference
    calculation <- paste(digits, "^", n_digits, sep="", collapse=" + ")
    print(paste(calculation, "=", number))
    
  } else {
    print(paste(number, "is NOT an Armstrong number."))
    print(paste("Sum =", sum_of_powers))
  }
}

