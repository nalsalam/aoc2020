# Day 4
library(tidyverse)

# Passports
# valid passports have 8 fields
# but treat those with missing cid as valid

# *** The main challenge of this puzzle is reading the data ***
# blank lines and EOF terminate records
# key:value pairs
# key:values pairs can be in any order of key
# some keys will not exist, implicit missing

# I think we should
# 1. organize long with two colums key and value  
# 2. add an id column that changes when a blank key value field is found
# 3. use the id to pivot_wider() which will make implicit missings explicit

dat <- read_lines(file = "data-naa/input4_test.txt")

tibble(keyvalue =
  # organize vector long
  str_split(dat, pattern = " ") %>% flatten_chr()
)
