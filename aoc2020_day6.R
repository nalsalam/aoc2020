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

#' Yes by everyone in a group
#' 
#' @param group, a character vector of answers for each person
#' @return a character vector of answer everyone said yes to
#' 
everyone <- function(group) {
  group %>% str_split("") %>% reduce(group, intersect, .init = letters)
}

groups <- str_split(input,"\\n\\n")[[1]] %>% str_split("\\n") 
intersect(groups[[3]][[1]] %>% str_split("") %>% unlist(), 
          groups[[3]][[2]]  %>% str_split("") %>% unlist())  

