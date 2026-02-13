V1 <- c(2,3,1,5,4,6,8,7,9)

matrix1 <- matrix(V1, nrow =3, ncol =3, byrow=TRUE)
rownames(matrix1) <- c("Row 1", "Row 2", "Row 3")
colnames(matrix1) <- c("Col 1", "Col 2", "Col 3")
print("Matrix-1:")
print(matrix1)

matrix2 <- t(matrix1)
rownames(matrix2) <- c("Row1", "Row2", "Row3")
colnames(matrix2) <- c("Col1", "Col2", "Col3")
print("Matrix-2 (Transpose of Matrix-1):")
print(matrix2)

print("Addition (Matrix-1 + Matrix-2):")
print(matrix1 + matrix2)

print("Substraction: M1 - M2")
print(matrix1 - matrix2)

print("Multiplication (M1 * M2):") 
print(matrix1 * matrix2)

print("Division (M1 / M2):") 
print(matrix1 / matrix2)

print("Matrix multiplication (M1 %*% M2):")
print(matrix1 %*% matrix2)

