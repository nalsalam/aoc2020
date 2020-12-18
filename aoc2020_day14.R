# --- Day 14: Docking Data ---

library(tidyverse)


# https://www.reddit.com/r/adventofcode/comments/kcr1ct/2020_day_14_solutions/
# Common lisp toward the bottom
# https://github.com/AdroMine/AdventOfCode2020/blob/main/Day14/day_14_readme.md

# Interesting but old and I think obsolete.  E.g. << is not recognized in R today
# https://stackoverflow.com/questions/1392059/algorithm-to-generate-bit-mask

# initialize memory as a list
# read each line from input and act upon it
# if the line is mask create a mask on the index of X's
# otherwise *convert value to binary*, *apply mask*, *convert back to decimal*, and
# set value in memory with its address as the name
# sum up the non-missing values in memory

# from numeric to a character vector of 0's and 1's
# and back again
dec_to_binary <- function(dec) {
  assertthat::assert_that(is.numeric(dec))
  # 1) divide d by powers of 2
  # 2) calculate 0,1 remainders
  # 3) a remainder of 0 means the 2 power is larger than the number
  # 4) if it is 1 it goes once so put a 1 in that place
  # 5) if it is a 2 it goes twice, e.g. the 4's place for dec 9 is two 
  #    but don't add because the previous, i.e. 8's place was 1
  # 6) if any odd number add it because previous additions of a 2's power
  #    only represents even multiples

  (dec %/% 2^(35:0) %% 2) %>% as.character()
}
binary_to_decimal <- function(binary) {
  assertthat::assert_that(length(binary) == 36)
  sum(2^(35:0) * as.numeric(binary == "1"))
}
# example from 11 to binary and back
binary <- dec_to_binary(11)
binary_to_decimal(binary)

solve <- function(input) {
memory <- list()
  for(i in 1:length(input)) {
    line <- input[i]
    if(str_detect(line,"^mask = ")) {
      mask <- str_replace(line, "mask = ", "") %>% str_split_fixed("", 36)
      indices <- which(mask == "0" | mask == "1")
    } else {
      addr = str_match(line, "^mem\\[(\\d+)\\]")[, 2]
      val = str_match(line, " = (\\d+)")[, 2] %>% as.numeric()
      val_bin <- dec_to_binary(val)
      val_bin[indices] <- mask[indices]
      val <- binary_to_decimal(val_bin)
      memory[addr] <- val
    }
  }
  sum(memory %>% unlist)
}

## Part 1 modify the value

## Part 1 example

input <- read_lines("data-naa/input14_test.txt")
solve(input)

## Part 1 puzzle

input <- read_lines("data-naa/input14.txt")
solve(input) %>% print(digits = 22)

## Part 2 modify the address

# mask bit "0" addr bit is unchanged
# mask bit "1" addr bit is 1
# mask bit "X" addr bit is floating, i.e. all possible combinations

solve <- function(input) {
  
  # memory <- list()
  memory <- data.frame(addr = integer(), val = integer())
  
  for(i in 1:length(input)) {
    line <- input[i]
    if(str_detect(line,"^mask = ")) {
      mask <- str_replace(line, "mask = ", "") %>% str_split_fixed("", 36)
      indices_X <- which(mask == "X")
      permutations <- expand.grid(rep(list(0:1), length(indices_X)))
    } else {
      val = str_match(line, " = (\\d+)")[, 2] %>% as.numeric()
      addr = str_match(line, "^mem\\[(\\d+)\\]")[, 2] %>% as.numeric()
      addr_bin <- dec_to_binary(addr)
      addr_bin[which(mask == "1")] <- 1
      
      for(j in 1:nrow(permutations)) {
        addr_bin[indices_X] <- 
          permutations[j,] %>% unlist()
        addr <- binary_to_decimal(addr_bin)
        # memory[addr] <- val
        memory <- add_row(memory, addr = addr, val = val, .before = TRUE)
        memory <- distinct(memory, addr, .keep_all = TRUE)
      }
    }
  }
  # sum(memory %>% unlist)
  sum(memory$val)
}

input <- read_lines("data-naa/input14_test2.txt")
solve(input) # 208

# Using tibble solved the unable to allocate vector of size problem
# but slow
# AndroMine has several alternatives which I am sure are faster

input <- read_lines("data-naa/input14.txt")
start <- Sys.time()
answer <- solve(input) 
elapsed <- Sys.time() - start
print(answer, digits = 22)



# What is causing this? memory or permutations? 
# If memory probably because NULL positions take memory.
# To get around that use a data frame with an addr col and a val col

memory <- tibble(addr = integer(), val = integer())
addr <- 10
val <- 13
memory <- add_row(memory, addr = addr, val = val, .before = TRUE)
memory
memory <- distinct(memory, addr, .keep_all = TRUE)
memory
