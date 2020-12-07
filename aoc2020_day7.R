library(tidyverse)

# Day 7 -- baggage rules

# rules for colors containing colors and numbers there of
# treat color as an element name
# use recursion to test

# A data structure to represent the rules:
# The rules are a named list of outside colors
# Each list element is a named integer vector
# Why?  names can be used to subset

input <- read_lines("data-naa/input7_test.txt")

create_rules <- function(input) {
  outside_colors <-
    str_extract(input, "^.+( bags contain)") %>%
    str_replace(" bags contain", "")
  
  inner_simplified <-
    str_replace(input, "^.+( bags contain )", "") %>%
    str_replace_all("bags|bag", "") %>%
    str_replace(" \\.", "") %>%
    str_replace("no\\b", "0") %>%
    str_split(" , ")
  
  inner_colors <-
    map(inner_simplified, ~ str_replace(.x, "\\d ", ""))
  
  # aka inner quantities
  rules <-  
    map(1:length(inner_simplified), 
        ~ str_extract(inner_simplified[[.x]], "\\d") %>% set_names(inner_colors[[.x]])
    )
  set_names(rules, outside_colors)
}

rules <- create_rules(input)

