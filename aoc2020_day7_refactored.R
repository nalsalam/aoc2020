# day 7 
# Clean up
library(tidyverse)

# Use parent-child structure in df
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
  
  tibble(
    parent = outside_colors,
    child_data = map(rules, ~tibble(child = names(.), num = as.numeric(.)))
  ) %>%
  unnest(child_data) %>%
  mutate(child = if_else(child == "", NA_character_, child))
}

colors_inside_of <- function(color, rules) {

  n_colors <- 0
  new_colors <- unique(color)
  
  while (n_colors < length(new_colors)) {
    
    new_colors_df <- tibble(
      color = new_colors
    ) %>%
      left_join(rules, c("color" = "parent"))
    
    n_colors <- length(new_colors)
    new_colors <- unique(c(new_colors_df$color, new_colors_df$child))
  }
  
  sort(new_colors)
}

num_colors_contain_shiny_gold <- function(colors, rules) {
  
  color_options <- setdiff(colors, "shiny gold")
  map_lgl(
    color_options,
    # subcats_of find colors reachable 
    # starting with the color in the first argument
    ~"shiny gold" %in% colors_inside_of(.x, rules)
  ) %>%
    sum()
}

### Part 1

### Example

num_colors_contain_shiny_gold(colors, rules)


input <- read_lines("data-naa/input7.txt")
rules <- create_rules(input)
colors <- rules$parent
num_colors_contain_shiny_gold(colors, rules)

### Part 2

color <- "shiny gold"
rules <- rules_plus
bags_inside_of <- function(color, rules) {
  
  next_color <- color
  next_number <- 1
  
  while (!all(next_color %in% c("fullbag", NA_character_))) {
    
    df <- tibble(
      color_col = next_color,
      number_col = next_number
    ) %>%
      left_join(rules, c("color_col" = "parent")) %>%
      group_by(child) %>%
      summarize(num = sum(num * number_col), .groups = "drop")
    
    next_color <- df$child
    next_number <- df$num
    res <- sum(df$num, na.rm = TRUE)
  }
  res - length(color) # originally submitted bag(s) not included
}

# Example 1
input <- read_lines("data-naa/input7_test.txt")
rules <- create_rules(input)
nobag <- tibble(
  parent = c("", NA_character_),
  child = c("", NA_character_),
  num = c(0, 0)
)
fullbag <- tibble(
  parent = unique(c(rules$parent, rules$child, "fullbag")),
  child = "fullbag",
  num = 1
)
rules <- rules %>%
  bind_rows(nobag) %>%
  bind_rows(fullbag)
bags_inside_of("shiny gold", rules)

# Example 2
input <- read_lines("data-naa/input7_test2.txt")
rules <- create_rules(input)
nobag <- tibble(
  parent = c("", NA_character_),
  child = c("", NA_character_),
  num = c(0, 0)
)
fullbag <- tibble(
  parent = unique(c(rules$parent, rules$child, "fullbag")),
  child = "fullbag",
  num = 1
)
rules <- rules %>%
  bind_rows(nobag) %>%
  bind_rows(fullbag)
bags_inside_of("shiny gold", rules)

# puzzle input

input <- read_lines("data-naa/input7.txt")
rules <- create_rules(input)
nobag <- tibble(
  parent = c("", NA_character_),
  child = c("", NA_character_),
  num = c(0, 0)
)
fullbag <- tibble(
  parent = unique(c(rules$parent, rules$child, "fullbag")),
  child = "fullbag",
  num = 1
)
rules_plus <- rules %>%
  filter(!is.na(num)) %>%
  bind_rows(nobag) %>%
  bind_rows(fullbag) %>%
  mutate(parent = if_else(is.na(parent), "", parent))
  
rules_plus %>% count(is.na(num), is.na(parent))
bags_inside_of("shiny gold", rules_plus)




