library(tidyverse)

# Day 7 -- baggage rules

# A data structure to represent the rules:
# The rules are a named list of outside colors
# Each list element is a named integer vector
# Why?  names can be used to subset

create_rules <- function(input) {
  outside_colors <-
    str_extract(input, "^.+( bags contain)") %>%
    str_replace(" bags contain", "")
  
  inner_simplified <-
    str_replace(input, "^.+( bags contain )", "") %>%
    str_replace_all("bags|bag", "") %>%
    str_replace(" \\.", "") %>%
    # str_replace("no\\b", "0") %>% but "other" is not a color so ...
    str_replace("no other", "") %>% # result is some outside colors have NA on inside vector
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

input <- read_lines("data-naa/input7_test.txt")
rules <- create_rules(input)

# Count the ways 

shiny_gold_direct <- function(outside_color) {
  "shiny gold" %in% names(rules[[outside_color]])
}
map_lgl(outside_colors, shiny_gold_direct) %>% sum()  # directly

# How about eventually? Need to look for shiny gold one layer deeper
# So go through inside colors of every rule also
# Three layers??

shiny_gold_eventually <- function(outside_color) {
  any(
    "shiny gold" %in% names(rules[[outside_color]]),
  
    {inside_colors <- names(rules[[outside_color]])
    map_lgl(inside_colors, ~ "shiny gold" %in% names(rules[[.x]])) %>% any()}
  )
}

shiny_gold_eventually("dark olive") 

# shiny_gold_eventually is not vectorized
testthat::expect_equal(
  c(shiny_gold_eventually("bright white"), 
    shiny_gold_eventually("muted yellow"), 
    shiny_gold_eventually("dark orange"),
    shiny_gold_eventually("light red"), 
    shiny_gold_eventually("dark olive")), 
  c(TRUE, TRUE, TRUE, TRUE, FALSE)
)
map_lgl(outside_colors, shiny_gold_eventually) %>% sum()

# Part 1

input <- read_lines("data-naa/input7.txt")

outside_colors <-
  str_extract(input, "^.+( bags contain)") %>%
  str_replace(" bags contain", "")

rules <- create_rules(input)

map_lgl(outside_colors, shiny_gold_eventually) %>% sum()

# Not the right answer -- probably need to go deep


