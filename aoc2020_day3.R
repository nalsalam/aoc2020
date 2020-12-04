# Day 3
# How many trees on any give rational slope?

# Some thoughts
# A slope of right 3, down 1 means that columns 2 & 3 are irrelevant.
# So why not eliminate them and turn the problem into a right 1 down 1 problem
# Then pull out diagonal and count trees
# A: because it wraps e.g., the thing about repeating to the right.

# readr::read_fwf() reads the data into a data frame of length 1 characters
mat_test <- readr::read_fwf(file = "input3_test.txt", fwf_widths(rep(1, 66))) %>% as.matrix()
# Create the series 1, 4, 7, 10, etc.
c(1, 1 + 3 * 1:(66/3 - 1))
# Use that to pull out the relevant columns
mat_test[, c(1, 1 + 3 * 1:(66/3 - 1))]
# Pull out diagonal
diag(mat_test[, c(1, 1 + 3 * 1:(66/3 - 1))])
# count the trees
sum(diag(mat_test[, c(1, 1 + 3 * 1:(66/3 - 1))]) == "#")

# The actual problem
# Need to know the width first.
#   Read input as a string per row and find length of 1st row
w <- str_length(scan(file = "input3.txt", what = character())[1])
mat <- readr::read_fwf(file = "input3.txt", fwf_widths(rep(1, w))) %>% as.matrix()
str(mat)
# Need to repeat the provided input to the right enough times
# for the tobaggan to reach the bottom before the right edge.
# If the grid has X rows we need 1 + 3 * X columns for a right 3 down 1 slope
ndups <- ceiling((1 + 3 * nrow(mat)) / ncol(mat))
# need a better way
MAT <- cbind(mat, mat, mat, mat, mat, mat, mat, mat, mat, mat,
             mat, mat, mat, mat, mat, mat, mat, mat, mat, mat,
             mat, mat, mat, mat, mat, mat, mat, mat, mat, mat,
             mat, mat)
# this works
MAT <- mat
for(i in 1:31) {
  MAT <- cbind(MAT, mat)
}
str(MAT)


ncol(MAT)
sum(diag(MAT[, c(1, 1 + 3 * 1:(ncol(MAT)/3 - 1))]) == "#")


# Part 2: Put the above in a function and generalize to cases of down > 1

ntrees <- function(right, down, mat) {
  ndup <- floor(down + right * nrow(mat) / ncol(mat))
  MAT <- mat
  for(i in 1:ndup) {
    MAT <- cbind(MAT, mat)
  }
   if(down == 1) {
  sum(diag(MAT[, c(1, 1 + right * 1:(ncol(MAT)/right - 1))]) == "#")
  } else if(right == 1) {
    sum(diag(MAT[c(1, 1 + down * 1:(nrow(MAT)/down - 1)),]) == "#")
  } else {
    return("ntrees only works with down = 1 or right = 1")
  }
}

ntrees(1, 1, mat_test)
ntrees(3, 1, mat_test)
ntrees(5, 1, mat_test)
ntrees(7, 1, mat_test)
ntrees(1, 2, mat_test)

n1 <- ntrees(1, 1, mat)
n2 <- ntrees(3, 1, mat)
n3 <- ntrees(5, 1, mat)
n4 <- ntrees(7, 1, mat)
n5 <- ntrees(1, 2, mat)

as.numeric(n1) * as.numeric(n2) * as.numeric(n3) * as.numeric(n4) * as.numeric(n5)
# got integer overflow if I did not convert to numeric, i.e. real

86L * 187L * 75L * 89L * 44L
86 * 187 * 75 * 89 * 44


# Jam's solution:

# Part 1:

tree_map <- read_lines("input3.txt") %>%
  str_split_fixed(., "", str_length(.[[1]])) %>%
  (function(mat) {mat == "#"})

map_height <- nrow(tree_map)
map_width  <- ncol(tree_map)

# encode positions to check for trees:
# down 1, over 3
pos_matrix <- cbind(
  1:map_height, # row positions
  (0:(map_height-1) * 3) %% map_width + 1 # col positions, with wrapping (repeating) map
)

# how many trees at those positions?
sum(tree_map[pos_matrix])

# Part 2:

gen_pos_matrix <- function(traj_down, traj_right,
                       n_iter,
                       wrap_width = Inf,
                       start_row = 1, start_col = 1) {
  cbind(
    (0:(n_iter-1) * traj_down) + start_row, # row positions
    (0:(n_iter-1) * traj_right) %% wrap_width + start_col  # col positions using modulo arithmetic
  )
}

all.equal(pos_matrix, gen_pos_matrix(1, 3, map_height, wrap_width = map_width))

trees_hit <- function(tree_map, traj_down, traj_right,
                      start_row = 1, start_col = 1) {

  stopifnot(traj_down > 0)
  n_iter <- (nrow(tree_map) - 1) %/% traj_down + 1

  pos_matrix <- gen_pos_matrix(traj_down, traj_right, n_iter,
                               wrap_width = ncol(tree_map),
                               start_row = start_row, start_col = start_col)

  sum(tree_map[pos_matrix])
}

ntrees <- tribble(
  ~traj_right, ~traj_down,
  1, 1,
  3, 1,
  5, 1,
  7, 1,
  1, 2
) %>%
  rowwise() %>%
  mutate(ntrees = trees_hit(tree_map, traj_down, traj_right))

reduce(as.numeric(ntrees$ntrees), `*`)
