library(dplyr)
# Read the file
titanic <- read.csv("titanic.csv")

# Remove NA values
sum(is.na(titanic)) # Check how many NA values 
titanic_cleaned <- na.omit(titanic) # Remove NA values
dim(titanic) # Check dimensions before
dim(titanic_cleaned) # Dimensions after

# Arrange values by ascending fare & save the file
titanic_sortbyfare <- arrange(titanic_cleaned, Fare)
View(titanic_sortbyfare)
write.csv(titanic_sortbyfare, "Titanic_SortByFare_Ascending.csv")

# Number of survivors
survivors <- titanic_cleaned %>% filter(Survived==1)
print("Number of survivors are: ")
nrow(survivors)

# Survivors' Ages Histogram
range(survivors$Age)
hist(survivors$Age,
     main = "Histogram of Survivors' Ages",
     xlab = "Age",
     ylab = "Frequency",
     col = "violet",
     border = "black")

# Attempt Summary and Make Necessary Changes
summary(titanic_cleaned)

# Need to convert to factor: survived, Pclass, Sex, Embarked
titanic_cleaned$Survived <- as.factor(titanic_cleaned$Survived)
titanic_cleaned$Pclass <- as.factor(titanic_cleaned$Pclass)
titanic_cleaned$Sex <- as.factor(titanic_cleaned$Sex)
titanic_cleaned$Embarked <- as.factor(titanic_cleaned$Embarked)

# Updated Summary for Cleaned Dataset
print("Titanic Passengers Data Summary:")
summary(titanic_cleaned)

# Summary for Survivors
survivors$Survived <- as.factor(survivors$Survived)
survivors$Pclass <- as.factor(survivors$Pclass)
survivors$Sex <- as.factor(survivors$Sex)
survivors$Embarked <- as.factor(survivors$Embarked)
print("Titanic Survivors Data Summary:")
summary(survivors)
