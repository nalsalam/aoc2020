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




####### 

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

# 1 long string
  raw_input <- read_file(file = "data-jra/input4.txt")
  # copied and pasted this which resulted in \r\n\r\n for blank lines
  raw_input <- read_file(file = "data-naa/input4_test.txt")
  # save directly which results in \n\n for blank lines
  raw_input <- read_file(file = "data-naa/input4.txt")

# string split on blank lines
sep_by_passport <- str_split(raw_input, pattern = "[\\n\\r]\\s*[\\n\\r]")[[1]]
# a blank line separating two passports is identified by the presence of a newline codepoint, optional whitespace, and then another newline. I am unsure if \n is the only relevant newline, or additional newlines are possible

# df organized lonig with id for passport
sep_by_keyvalue <- str_split(sep_by_passport, pattern = "[\\n\\r\\s]") %>%
  tibble(
    id = 1:length(.),
    keyvalue_text = .
  ) %>%
  unnest(keyvalue_text) %>%
  filter(!is.na(keyvalue_text) & keyvalue_text != "") %>% # better way to trim last newline?
  separate(col = c(keyvalue_text), sep = ":", into = c("key", "value"))

# pivot wide
passports <- pivot_wider(sep_by_keyvalue,
                         names_from = "key", values_from = "value")

# required fields do not include "cid"
required_fields <- setdiff(expected_fields$field_code, "cid")

# function to determine if passport is valid
valid_passport <- function(data, required_fields) {
  data %>%
    mutate(across(all_of(required_fields), ~!is.na(.x))) %>%
    rowwise() %>%
    mutate(valid = all(c_across(all_of(required_fields)))) %>%
    pull(valid)
}

# determine if each is valid
validated_passports <- passports %>%
  mutate(., valid = valid_passport(., required_fields))

# add them the number of valid
sum(validated_passports$valid)

# part 2:

# stricter validation:

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
valid_byr <- function(byr) {
  str_detect(byr, "\\d{4}") & as.numeric(byr) >= 1920 & as.numeric(byr) <= 2002 }

testthat::expect_equal(valid_byr(c("2002", "2003")), c(TRUE, FALSE))

# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
valid_iyr <- function(iyr) {
  str_detect(iyr, "\\d{4}") & as.numeric(iyr) >= 2010 & as.numeric(iyr) <= 2020 }

# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
valid_eyr <- function(eyr) {
  str_detect(eyr, "\\d{4}") & as.numeric(eyr) >= 2020 & as.numeric(eyr) <= 2030 }

# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
valid_hgt <- function(hgt_text) {
  hgt_regex <- str_match(hgt_text, "(\\d+)((?:cm)|(?:in))")
  hgt <- as.numeric(hgt_regex[,2])
  unit <- hgt_regex[,3]
  case_when(
    is.na(hgt) | is.na(unit) ~ FALSE,
    unit == "cm" & hgt >= 150 & hgt <= 193 ~ TRUE,
    unit == "in" & hgt >= 59 & hgt <= 76 ~ TRUE,
    TRUE ~ FALSE)}

testthat::expect_equal(valid_hgt(c("60in", "190cm", "190in", "190")),
                       c(TRUE, TRUE, FALSE, FALSE))

# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
valid_hcl <- function(hcl) {
  str_detect(hcl, "\\#[[:xdigit:]]{6}") %in% TRUE}

testthat::expect_equal(valid_hcl(c("#123abc", "#123abz", "123abc")),
                       c(TRUE, FALSE, FALSE))

# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
valid_ecl <- function(ecl) {
  ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

testthat::expect_equal(valid_ecl(c("brn", "wat")),
                       c(TRUE, FALSE))

# pid (Passport ID) - a nine-digit number, including leading zeroes.
valid_pid <- function(pid) {
  str_detect(pid, "^\\d{9}$") }

testthat::expect_equal(valid_pid(c("000000001", "0123456789")),
                       c(TRUE, FALSE))

# cid (Country ID) - ignored, missing or not.

strict_valid_passport <- function(data, required_fields) {

  required_fields_present <- data %>%
    mutate(across(all_of(required_fields), ~!is.na(.x))) %>%
    rowwise() %>%
    mutate(valid = all(c_across(all_of(required_fields)))) %>%
    pull(valid)

  # all valid and required fields present:
  required_fields_present &
    valid_byr(data$byr) &
    valid_iyr(data$iyr) &
    valid_eyr(data$eyr) &
    valid_hgt(data$hgt) &
    valid_hcl(data$hcl) &
    valid_ecl(data$ecl) &
    valid_pid(data$pid)
}

strict_validated_passports <- passports %>%
  mutate(., valid = strict_valid_passport(., required_fields))

sum(strict_validated_passports$valid)
