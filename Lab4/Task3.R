# First array: 4 columns, 2 rows, 3 tables
# Create values for first array
values1 <- 1:24  # 2 * 4 * 3 = 24 values
array1 <- array(values1, dim = c(2, 4, 3))
print("Array1:")
print(array1)

# Second array: 2 columns, 3 rows, 5 tables
# Create values for second array
values2 <- 25:54  # 3 rows * 2 columns * 5 tables = 30 values, values are 25 to 54
array2 <- array(values2, dim = c(3, 2, 5))
print("Array2:")
print(array2)

second_row_second_matrix <- array1[2, , 2]
print("The second row of the second matrix of the array:")
print(second_row_second_matrix)

element_3row_3col_firstmatrix <- array2[3, 2, 1]  # Row 3, Column 2, Table 1
print("The element in the 3rd row and 3rd column of the 1st matrix:")
print(element_3row_3col_firstmatrix)
