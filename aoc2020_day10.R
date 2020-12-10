# Day 10 jolts
library(tidyverse)

# Part 1
# Distribution of jolt differences

diff_distribution <- function(input) {
  diffs <- c(input, input[length(input)] + 3) - c(0, input)
  c(sum(diffs == 1), sum(diffs == 2), sum(diffs == 3))
}

input <- read_lines("data-naa/input10_test.txt") %>% as.numeric() %>% sort()
diff_distribution(input)
input <- read_lines("data-naa/input10_test2.txt") %>% as.numeric() %>% sort()
diff_distribution(input)

answer <- function(input) {
  diffs <- c(input, input[length(input)] + 3) - c(0, input)
  sum(diffs == 1) * sum(diffs == 3)
}

### Puzzle
input <- read_lines("data-naa/input10.txt") %>% as.numeric() %>% sort()
diff_distribution(input)
answer(input)

# Part 2

# Number of distinct arrangements
# work with diffs
# 1 3 1 1 1 3 1 1 3 1 3 3 sequence of diffs from example

# the triple 1's corresponds to 4, 5, 6, 7
# can remove none, 5, 6, or both, 4 combinations

# the double 1's correspond to 10, 11, 12
# can remove none or 11, 2 combinations

# total combinations is 8

# if there are zero doubles or triples, etc. give each a combination of 1
# for the multiplication

# quad 1's have 3 that can be worked with
# remove 1 of those (3), 2 of those (3), or all (1), or 7

# look for triples, count and remove
# look for doubles in remainder

arrangements <- function(diffs) {

quad <- rep(1, 4)
triple <- rep(1, 3)
double <- rep(1, 2)
  
quads <- 0
start <- 1
repeat {
  if(start > length(diffs) - 3) break()
  if(all(quad == diffs[start: (start + 3)])) {
    quads <- quads + 1
    diffs <- diffs[-start: -(start + 3)]
  } else {
    start <- start + 1
  }
}

triples <- 0
start <- 1
repeat {
  if(start > length(diffs) - 2) break()
  if(all(triple == diffs[start: (start + 2)])) { # triple found
    triples <- triples + 1
    diffs <- diffs[-start: -(start + 2)] # remove the found triple
    # start is good for next iteration
  } else {
    start <- start + 1
  }
}

doubles <- 0
start <- 1
repeat {
  if(start > length(diffs) - 1) break()
  if(all(double == diffs[start: (start + 1)])) { # triple found
    doubles <- doubles + 1
    diffs <- diffs[-start: -(start + 1)] # remove the found triple
    # start is good for next iteration
  } else {
    start <- start + 1
  }
}

# print(c(quads, triples, doubles))

# any number to zero power is 1
return(7^quads*4^triples*2^doubles)

}

input <- read_lines("data-naa/input10_test.txt") %>% as.numeric() %>% sort()
diffs <- c(input, input[length(input)] + 3) - c(0, input)
arrangements(diffs)

input <- read_lines("data-naa/input10_test2.txt") %>% as.numeric() %>% sort()
diffs <- c(input, input[length(input)] + 3) - c(0, input)
arrangements(diffs)

input <- read_lines("data-naa/input10.txt") %>% as.numeric() %>% sort()
diffs <- c(input, input[length(input)] + 3) - c(0, input)
ans <- arrangements(diffs)

start <- Sys.time()
arrangements(diffs)
Sys.time() - start

print(ans, digits = 16)
