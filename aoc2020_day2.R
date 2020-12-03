# passwords that don't meet policy
library(tidyverse)

input <- read_lines("data-jra/input2.txt")

passwds <- tibble(
  input = input
) %>%
  extract(
    col = c(input),
    regex = "(\\d+)-(\\d+) ([[:lower:]]): ([[:lower:]]+)",
    into = c("num1", "num2", "char", "passwd"),
    remove = FALSE
  ) %>%
  mutate(across(.cols = c(num1, num2), .fns = as.numeric))

# How many passwords are valid according to their policies?

valid_passwd_old <- function(passwd, char, rep_min, rep_max) {
  count <- str_count(passwd, pattern = char)
  valid <- (count >= rep_min) & (count <= rep_max)
  valid
}

passwds %>%
  mutate(valid = valid_passwd_old(passwd, char, num1, num2)) %>%
  count(valid)


# Part 2
# New intpretation of the policy

# Jam's solution:
valid_passwd_new <- function(passwd, char, pos1, pos2) {
  match1 <- str_sub(passwd, start = pos1, end = pos1) == char
  match2 <- str_sub(passwd, start = pos2, end = pos2) == char
  xor(match1, match2)
}

passwds %>%
  mutate(valid = valid_passwd_new(passwd, char, num1, num2)) %>%
  count(valid)

# Dad's solution:
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
      paste0("^.{", num1 - 1, "}", char, collapse = "")),
    str_detect(
      passwd,
      # paste0("^", paste0(rep(".", rep_max - 1), collapse = ""), char, collapse = ""))
      paste0("^.{", num2 - 1, "}", char, collapse = ""))
    )
  )
) %>%
count(valid)

