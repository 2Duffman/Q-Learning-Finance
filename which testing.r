array_dim <- c(100, 10, 1, 3, 10)
my_array <- array(sample(1:100, prod(array_dim), replace = TRUE), dim = array_dim)
print(my_array)
