x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Create relation using linear model, lm()
relation <- lm(y~x)
print(relation)

# Result/Output:
# Coefficients:
#  (Intercept)            x  
#     -38.4551       0.6746  

print(summary(relation))

# Find weight if person weigh 170
a <- data.frame(x = 170)
result <- predict(relation, a)
print(result)

# Visualise
plot (x, y, xlab= "Height in cm" , ylab= "Weight in kg", col="lightblue", cex=2,
      main="Height & Weight Regression", abline(lm(y~x)), pch=16 )

# ----------------------------------------------------------------------------------------

# Using Built-In Dataset

x <- cars$speed
y <- cars$dist
model1 <- lm(y~x)
print(summary(model1))

# Visualise
plot(x, y, xlab = "Speed", ylab = "Distance",
     col="violet", pch=16, cex=1,
     main = "Speed vs. Distance Regression",
     abline(lm(y~x)))

# Predict
value_to_predict <- data.frame(x = c(26, 27, 28))
prediction <- predict(model1, value_to_predict)
print(prediction)

?lm
?predict

# -------------------------------------------------------------------------------------------

# KNN Analysis

install.packages("e1071")
install.packages("caTools")
install.packages("class")

data(iris)
head(iris)

split <- sample.split(iris, SplitRatio = 0.7)
train_set <- subset(iris, split == 'TRUE')
test_set <- subset(iris, split == 'FALSE')

head(train_scale)
head(train_set)

train_scale <- scale(train_set[, 1:4]) # Only scale numeric values (columns 1-4, for all rows)
test_scale <- scale(test_set[, 1:4])

knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k=1)
print(knn_model)

# Confusion Matrix
cm <- table(test_set$Species, knn_model) # What are the two values you want in the table?
print(cm)

# Sample error
misclassError <- mean(knn_model!=test_set$Species)
accuracy <- 1-misclassError
print(accuracy)

# Make k = 3
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k=3)
print(knn_model)

cm <- table(test_set$Species, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Species)
print(1-misclassError)

# Accuracy: 0.933

# K=5
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k = 5 )
print(knn_model)

cm <- table(test_set$Species, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Species)
print(1-misclassError)

# K=7
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k = 7 )
print(knn_model)

cm <- table(test_set$Species, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Species)
print(1-misclassError)

# K=15
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k = 7 )
print(knn_model)

cm <- table(test_set$Species, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Species)
print(1-misclassError)

# K = 19
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Species,
                 k = 7 )
print(knn_model)

cm <- table(test_set$Species, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Species)
print(1-misclassError)




      
