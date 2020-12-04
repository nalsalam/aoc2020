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

expected_fields <- tribble(
  ~field_code, ~field_desc,
  "byr", "Birth Year",
  "iyr", "Issue Year",
  "eyr", "Expiration Year",
  "hgt", "Height",
  "hcl", "Hair Color",
  "ecl", "Eye Color",
  "pid", "Passport ID",
  "cid", "Country ID"
)

raw_input <- read_file(file = "data-jra/input4.txt")

sep_by_passport <- str_split(raw_input, pattern = "[\\n\\r]\\s*[\\n\\r]")[[1]]
# a blank line separating two passports is identified by the presence of a newline codepoint, optional whitespace, and then another newline. I am unsure if \n is the only relevant newline, or additional newlines are possible

sep_by_keyvalue <- str_split(sep_by_passport, pattern = "[\\n\\r\\s]") %>%
  tibble(
    id = 1:length(.),
    keyvalue_text = .
  ) %>%
  unnest(keyvalue_text) %>%
  filter(!is.na(keyvalue_text) & keyvalue_text != "") %>% # better way to trim last newline?
  separate(col = c(keyvalue_text), sep = ":", into = c("key", "value"))

passports <- pivot_wider(sep_by_keyvalue,
                         names_from = "key", values_from = "value")

required_fields <- setdiff(expected_fields$field_code, "cid")

valid_passport <- function(data, required_fields) {
  data %>%
    mutate(across(all_of(required_fields), ~!is.na(.x))) %>%
    rowwise() %>%
    mutate(valid = all(c_across(all_of(required_fields)))) %>%
    pull(valid)
}

validated_passports <- passports %>%
  mutate(., valid = valid_passport(., required_fields))

sum(validated_passports$valid)



