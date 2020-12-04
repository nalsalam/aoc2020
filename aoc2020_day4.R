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

dat <- read_lines(file = "data-naa/input4_test.txt")

# Maybe add an id at each blank line, e.g.

dat2 <- if_else(dat == "", "id:x", dat)

# but need x to increment or at least be unique

rec_num <- 1: (sum(dat2 == "id:x") + 1) # add 1 because EOF terminates a record also

dat3 <- if_else(dat2 == "id:x", paste("id", rec_num, sep = ":"), dat2) # doesn't work


  