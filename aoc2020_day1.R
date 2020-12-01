# 1a
# find product of two entries that sum to 2020

v <- scan("input1a.txt")

x <- outer(v, t(v), FUN = "+")

x[x == 2020] # there is a solution
r <- which(x == 2020, arr.ind = TRUE)
v[r[1,1]] + v[r[1,3]] # sum is 2020
v[r[1,1]] * v[r[1,3]] # answer to part 1

# 1b
# find product of three entries that sum to 2020

x <- outer(outer(v, t(v), FUN = "+"), t(v), FUN = "+")
x[x == 2020] # there is a solution
r <- which(x == 2020, arr.ind = TRUE)
v[r[1,1]] + v[r[1,3]] + v[r[1,5]] # sum is 2020
v[r[1,1]] * v[r[1,3]] * v[r[1,5]] # answer to part 2

