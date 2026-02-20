number <- as.integer(readline(prompt="Input an integer: "))
list<- 1:number

for (x in list){
  cube <- x^3
  print(paste("Number is:", x, "and the cube is:", cube))
}


