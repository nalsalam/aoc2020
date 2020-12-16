# --- Day 13: Shuttle Search ---
library(tidyverse)


# ID number == loop time
# all buses leave at timestamp 0, ID number, 2 * id number

# Part 1
# find ID of first bus arriving after your arrival at bus stop
# multiply that ID by the time you had to wait

# Part 1 Example

input <- read_lines("data-naa/input13_test.txt")
arrival_time <- input[[1]] %>% as.numeric()
all_bus_ids <- input[[2]] %>%
  str_split(",") %>% unlist()

bus_ids <- all_bus_ids[all_bus_ids != "x"] %>% as.numeric()
x <- (arrival_time %/% bus_ids) * bus_ids + bus_ids - arrival_time
bus_ids[which.min(x)] * x[which.min(x)]

# Part 1 Puzzle

input <- read_lines("data-naa/input13.txt")
arrival_time <- input[[1]] %>% as.numeric()
all_bus_ids <- input[[2]] %>%
  str_split(",") %>% unlist()

bus_ids <- all_bus_ids[all_bus_ids != "x"] %>% as.numeric()
x <- (arrival_time %/% bus_ids) * bus_ids + bus_ids - arrival_time
bus_ids[which.min(x)] * x[which.min(x)]

# Part 2 Example
# differences in bus departures defined on the original list

input <- read_lines("data-naa/input13_test.txt")
# Bus IDs are coprime
bus_ids <- input[[2]] %>% str_split(",") %>% unlist() %>% as.numeric()
# Indices that are the time offsets for the puzzle
indices <- (0:(length(bus_ids) - 1))[which(!is.na(bus_ids))]
ids <- bus_ids[which(!is.na(bus_ids))]

# https://www.youtube.com/watch?v=zIFehsBHB8o
# Seems hard to do the inverses 
# chinese return same number with 0 or id offset at front
numbers::chinese(ids - indices %% ids, ids) ==
numbers::chinese(c(0, (ids - indices %% ids)[-1]), ids)
t %>% numbers::chinese(ids - indices %% ids, ids)
t %>% print(digits = 22)
# but to verify start with a zero
map2_lgl(c(0, (ids - indices %% ids)[-1]), ids, ~ t %% .y == .x)

t %%  7 == 0-0 
t %% 13 == 13-1
t %% 59 == 59-4
t %% 31 == 31-6
t %% 19 == 19-7

numbers::chinese(c(7, 13 - 1, 59 - 4, 31 - 6, 19 - 7), c(7, 13, 59, 31, 19))
numbers::chinese(c(0, 13 - 1, 59 - 4, 31 - 6, 19 - 7), c(7, 13, 59, 31, 19))
numbers::chinese(ids - indices, ids)
numbers::chinese(ids - indices %% ids, ids)

# Other examples
solve <- function(lst) {
  bus_ids <- lst %>% unlist() %>% as.numeric()
  indices <- (0:(length(bus_ids) - 1))[which(!is.na(bus_ids))]
  ids <- bus_ids[which(!is.na(bus_ids))]
  remainders <- c(0, (ids - indices %% ids)[-1])
  answer <- numbers::chinese(remainders, ids)
  print(ids)
  print(indices)
  print(remainders)
  print(map2_dbl(remainders, ids, ~ answer %% .y))
  answer
}

perturb <- function(lst, add_factor) {
  bus_ids <- lst %>% unlist() %>% as.numeric()
  indices <- (0:(length(bus_ids) - 1))[which(!is.na(bus_ids))]
  ids <- bus_ids[which(!is.na(bus_ids))]
  remainders <- c(0, (ids - indices %% ids)[-1])
  answer <- numbers::chinese(remainders, ids) + add_factor ## nearby
  print(ids)
  print(indices)
  print(remainders)
  print(map2_dbl(remainders, ids, ~ answer %% .y))
  answer
}

solve(list(17,"x",13,19))
solve(list(17,"x",13,19), 1)
solve(list(67,7,59,61))
solve(list(67,"x",7,59,61))
solve(list(67,7,"x",59,61))
solve(list(1789,37,47,1889))

# Part 2 Puzzle

input <- read_lines("data-naa/input13.txt")[[2]] %>% str_split(",")
solve(input) %>% print(digits = 16) 

# chinese() fails
# last two rows should be the same, i.e. remainders equal to indices
# Is another nearby number the answer?
# 
perturb(input, 0) %>% print(digits = 16) 
perturb(input, (804-684)) %>% print(digits = 16)
# 1058443396696792
# looks right, last two lines are the same
