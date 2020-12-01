# 1a
# find product of two entries that sum to 2020

v <- scan("input1a.txt")

# outer sum
# t() is transpose
# so if v = c(1, 2), the outer sum is:
# 2 3
# 3 4
# Here x is a 200 x 200 matrix
x <- outer(v, t(v), FUN = "+")

# Are there elements equal to 2020.  Yes two symmetric sets.
# the subset operator [] pulls out TRUE values, in this case
# value that sum to 2020
# If the return was numeric(0) it would indicate no cases of the sum == 2020
x[x == 2020] 

# What are the row, column indices of those?
# x == 2020 is a 200 x 200 matrix of TRUE/FALSE
# which returns the indices of the array x == 2020 that are TRUE
# the odd thing here is that there are 3 dimensions.. Need to figure that out still

r <- which(x == 2020, arr.ind = TRUE)

# Again using the subsetting operator, I use the indices found
# and stored in r to pull out the values in those locations
v[r[1,1]] + v[r[1,3]] # sum is 2020
v[r[1,1]] * v[r[1,3]] # answer to part 1

# 1b
# find product of three entries that sum to 2020

# outer sum of out sum
# so this is a cube

x <- outer(outer(v, t(v), FUN = "+"), t(v), FUN = "+")
x[x == 2020] # there is a solution
r <- which(x == 2020, arr.ind = TRUE)
v[r[1,1]] + v[r[1,3]] + v[r[1,5]] # sum is 2020
v[r[1,1]] * v[r[1,3]] * v[r[1,5]] # answer to part 2

