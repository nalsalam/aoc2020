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

input <- read_lines("data-naa/input7_test.txt")
rules <- create_rules(input)
# NEED TO FIX: no other bags needs to be the empty vector

# Count the ways 

shiny_gold_inside <- function(outside_color) {
  "shiny gold" %in% names(rules[[outside_color]])
}
map_lgl(outside_colors, shiny_gold_inside) %>% sum()  # directly

# How about indirectly? Need to look for shiny gold one layer deeper
# So need to go through inside colors of every rule also

# This doesn't work.  Sorry.  Taking a break.
shiny_gold_inside_2 <- function(outside_color, outside_colors = outside_colors) {
  
  if("shiny gold" %in% names(rules[[outside_color]])) TRUE
  else map(outside_colors, shiny_gold_inside_2(.x))
  else FALSE
}

