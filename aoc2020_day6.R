library(tidyverse)

input <- read_file(file = "data-naa/input6_test.txt") %>%
  str_replace_all("\\r\\n", "\n")

yes_anyone <- function(input) {
  # groups
  str_split(input,"\\n\\n")[[1]] %>%
  # yes answers 
  str_replace_all("\\n", "") %>% str_split("") %>%
  # de-duplicate
  map(~ 26 - setdiff(letters, .x) %>% length()) %>% unlist()
}

testthat::expect_equal(yes_anyone(input), c(3, 3, 3, 1, 1))
testthat::expect_equal(sum(yes_anyone(input)), 11)

# Part 1

input <- read_file(file = "data-naa/input6.txt")
sum(yes_anyone(input))

# Part 2

#' Number of common yes's by everyone in a group
#' 
#' @param group, a list of list of vectors answers for each person
#' @return a character vector of answer everyone said yes to
#' 
everyone <- function(group) {
  reduce(group, intersect, .init = letters) %>% length()
}
input <- read_file(file = "data-naa/input6_test.txt") %>%
  str_replace_all("\\r\\n", "\n")

# list (groups) of lists (people) of character vector (yes answers)
groups <- str_split(input,"\\n\\n")[[1]] %>% 
  str_split("\\n") %>% map(str_split, "")
testthat::expect_equal(map_dbl(groups, everyone), c(3, 0, 1, 1, 1))
testthat::expect_equal(sum(map_dbl(groups, everyone)), 6)

input <- read_file(file = "data-naa/input6.txt") %>%
  str_replace_all("\\r\\n", "\n") %>%
  str_replace("\\n$", "") # stray \n at the end

groups <- str_split(input,"\\n\\n")[[1]] %>% 
  str_split("\\n") %>% map(str_split, "")

sum(map_dbl(groups, everyone))

# Part 1 using Part 2 approach to input organization and processing

anyone <- function(group) {
  reduce(group, union) %>% length()
}
testthat::expect_equal(sum(yes_anyone(input)), sum(map_dbl(groups, anyone)))

