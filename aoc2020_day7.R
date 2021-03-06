library(tidyverse)

# Day 7 -- baggage rules

# A data structure to represent the rules:
# The rules are a named list of outside colors
# Each list element is a named integer vector
# Why? names can be used to subset
# Jam prefers a data from of parent-child links
# Why? join, unique can be used

create_rules <- function(input) {
  outside_colors <-
    str_extract(input, "^.+( bags contain)") %>%
    str_replace(" bags contain", "")

  inner_simplified <-
    str_replace(input, "^.+( bags contain )", "") %>%
    str_replace_all("bags?", "") %>%
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

# named list of a named vector
rules <- create_rules(input)

make_rules_df <- function(rules) {
  tibble(
    parent = names(rules),
    child_data = map(rules, ~tibble(child = names(.), num = unname(.)))
  ) %>%
    unnest(child_data)
}

# parent-child pairs
rules_df <- make_rules_df(rules) %>%
  filter(!is.na(num))

# subcats_of()
source("R/category-funs.R")

# just in case there is a child color not in parent
all_colors <- union(rules_df$parent, rules_df$child)

num_colors_contain_shiny_gold <- function(all_colors, rules_df) {

  color_options <- setdiff(all_colors, "shiny gold")
  map_lgl(
    color_options,
    # subcats_of find colors reachable 
    # starting with the color in the first argument
    ~"shiny gold" %in% subcats_of(.x, rules_df)
  ) %>%
    sum()
}

num_colors_contain_shiny_gold(all_colors, rules_df)

# My start that worked for example but not the puzzle 
# because for the former going in one level was enough

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

# Jams:
# Part 1
# number of bag colors than can contain a shiny gold bag

input <- read_lines("data-jra/input7.txt")
rules_df <- create_rules(input) %>%
  make_rules_df()
all_colors <- union(rules_df$parent, rules_df$child)
num_colors_contain_shiny_gold(all_colors, rules_df)

# Part 2
# how many bags are required in 1 shiny gold bag

nobag <- tibble(
  parent = c("", NA_character_),
  child = c("", NA_character_),
  num = c(0, 0)
)

fullbag <- tibble(
  parent = unique(c(rules_df$parent, rules_df$child, "fullbag")),
  child = "fullbag",
  num = 1
)

rules_df0 <- rules_df %>%
  mutate(num = as.numeric(num)) %>%
  mutate(child = if_else(child == "", NA_character_, child)) %>%
  mutate(num = if_else(is.na(num), 0, num)) %>%
  bind_rows(nobag) %>% # why?
  bind_rows(fullbag) 
rules_df0 %>% View()

subbags_of <- function(cat) {

  next_cats <- cat
  next_nums <- 1

  while (!all(next_cats %in% c("fullbag", NA_character_))) {

    df <- tibble(
      catcol = next_cats,
      numcol = next_nums
    ) %>%
      left_join(rules_df0, c("catcol" = "parent")) %>%
      group_by(child) %>%
      summarize(num = sum(num * numcol), .groups = "drop")

    next_cats <- df$child
    next_nums <- df$num
    res <- sum(df$num)
  }
 res - length(cat) # originally submitted bag(s) not included
}

subbags_of("shiny gold")


# Pops' input with Jam's solution
# Part 1

input <- read_lines("data-naa/input7.txt")
rules_df <- create_rules(input) %>%
  make_rules_df()
all_colors <- union(rules_df$parent, rules_df$child)
num_colors_contain_shiny_gold(all_colors, rules_df)

# Part 2


nobag <- tibble(
  parent = c("", NA_character_),
  child = c("", NA_character_),
  num = c(0, 0)
)

fullbag <- tibble(
  parent = unique(c(rules_df$parent, rules_df$child, "fullbag")),
  child = "fullbag",
  num = 1
)

rules_df0 <- rules_df %>%
  mutate(num = as.numeric(num)) %>%
  mutate(child = if_else(child == "", NA_character_, child)) %>%
  mutate(num = if_else(is.na(num), 0, num)) %>%
  bind_rows(nobag) %>%
  bind_rows(fullbag)

subbags_of <- function(cat) {
  
  next_cats <- cat
  next_nums <- 1
  
  while (!all(next_cats %in% c("fullbag", NA_character_))) {
    
    df <- tibble(
      catcol = next_cats,
      numcol = next_nums
    ) %>%
      left_join(rules_df0, c("catcol" = "parent")) %>%
      group_by(child) %>%
      summarize(num = sum(num * numcol), .groups = "drop")
    
    next_cats <- df$child
    next_nums <- df$num
    res <- sum(df$num)
  }
  res - length(cat) # originally submitted bag(s) not included
}

subbags_of("shiny gold")
