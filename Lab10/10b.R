data("Theoph")


# Activity 1: Predict dose based on relation

y <- Theoph$Dose
x <- Theoph$Wt

relation <- lm(y~x)
print(summary(relation))

# R value is 97.94% which is very good, very low P value, nearly 0 residuals

a <- data.frame(x=c(90, 95, 100))
predict(relation, a)

# Activity 2: 
data(ChickWeight)
head(ChickWeight)

split_sets <- sample.split(ChickWeight, SplitRatio=0.7)
train_set <- subset(ChickWeight, split == 'TRUE')
test_set <- subset(ChickWeight, split == 'FALSE')

head(train_set) # Only columns 1 and 2 are numeric
head(test_set)

train_scale <- scale(train_set[,1:2]) 
test_scale <- scale(test_set[,1:2])

# KNN Model
knn_model <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_set$Diet,
                 k=3)
print(knn_model)

# Confusion Matrix
cm <- table(test_set$Diet, knn_model)
print(cm)

misclassError <- mean(knn_model!=test_set$Diet)
print(paste('Accuracy: ', 1-misclassError))

      
