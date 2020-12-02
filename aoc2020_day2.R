# passwords that don't meet policy
library(tidyverse)

### Test

input_test <-
  tibble(
    rep = c("1-3", "1-3", "2-9"),
    char = c("a:", "b:", "c:"), 
    passwd = c("abcde", "cdefg", "ccccccccc")
  )

passwds_test <-
  input_test %>% 
  mutate(char = substr(char, 1, 1)) %>% 
  separate(rep, into = c("rep_min", "rep_max"), sep = "-")

passwds_test %>%
  mutate(
    count = str_count(passwd, pattern = char), 
    valid = count >= rep_min & count <= rep_max
  ) %>%
  count(valid)

### 

input <- 
  read_delim("input2.txt", 
             delim = " ", 
             col_names = c("rep", "char", "passwd"))

passwds <-
  input %>% 
  mutate(char = substr(char, 1, 1)) %>% # strip the ":"
  separate(rep, into = c("rep_min", "rep_max"), sep = "-") # split the policy
         
# How many passwords are valid according to their policies?

passwds %>%
  mutate(
    count = str_count(passwd, pattern = char), 
    valid = count >= rep_min & count <= rep_max
    ) %>%
count(valid)
