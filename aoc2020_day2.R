# passwords that don't meet policy
library(tidyverse)

input <- 
  read_delim("input2.txt", 
             delim = " ", 
             col_names = c("rep", "char", "passwd"))

passwds <-
  input %>% 
  mutate(char = substr(char, 1, 1)) %>% # strip the ":"
  separate(rep, into = c("rep_min", "rep_max"), sep = "-", remove = FALSE) %>%
  mutate(across(.cols = c(rep_min, rep_max), .fns = as.numeric)) # failed to do this the 1st time

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
      paste0("^", paste0(rep(".", rep_min - 1), collapse = ""), char, collapse = "")),
    str_detect(
      passwd, 
      paste0("^", paste0(rep(".", rep_max - 1), collapse = ""), char, collapse = ""))
    )
  )
) %>%
count(valid)

