# passwords that don't meet policy
library(tidyverse)

input <- read_lines("data-jra/input2.txt")

passwds <- tibble(
  input = input
) %>%
  extract(
    col = c(input),
    regex = "(\\d+)-(\\d+) ([[:lower:]]): ([[:lower:]]+)",
    into = c("rep_min", "rep_max", "char", "passwd"),
    remove = FALSE
  ) %>%
  mutate(across(.cols = c(rep_min, rep_max), .fns = as.numeric))

# How many passwords are valid according to their policies?

passwds %>%
  mutate(
    count = str_count(passwd, pattern = char),
    valid = (count >= rep_min) & (count <= rep_max)
    ) %>%
count(valid)

# Part 2
# New intpretation of the policy

map_dfr(1:nrow(passwds),
# have to do a row at a time because the pattern parameter of str_detect cannot be a vector
~ passwds[.x,] %>%
  mutate(
    valid =
      # exclusive or
      xor(
      str_detect(
      passwd,
      # building the regular expression pattern from rep_min, rep_max, and char
      # using multiple "." to anchor the position to look for the character
      # paste0("^", paste0(rep(".", rep_min - 1), collapse = ""), char, collapse = "")),
      paste0("^.{", rep_min - 1, "}", char, collapse = "")),
    str_detect(
      passwd,
      # paste0("^", paste0(rep(".", rep_max - 1), collapse = ""), char, collapse = ""))
      paste0("^.{", rep_max - 1, "}", char, collapse = ""))
    )
  )
) %>%
count(valid)

