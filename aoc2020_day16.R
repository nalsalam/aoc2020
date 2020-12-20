library(tidyverse)

# --- Day 16: Ticket Translation ---

# Part 1

# input <- read_lines("data-naa/input16_test.txt")
input <- read_lines("data-naa/input16.txt")

nearby_tickets_idx <- str_detect(input, "^nearby") %>% which() + 1
rules_idx <- str_detect(input, "^your ticket") %>% which() - 2

# Rules as df with two min/max pairs
rules <-
tibble(rule = input[1:rules_idx]) %>%
  tidyr::extract(rule, 
    c("field", "min1", "max1", "min2", "max2"),
    "^([[[:lower:]] ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)") %>%
  mutate(across(.cols = -c(field), .fns = as.integer))
field_names <- rules$field

# Nearby ticket fields (for Part 1)
nearby_tickets_fields <- 
  str_split(input[nearby_tickets_idx : length(input)], ",") %>% 
  unlist() %>% # this loses the row which is the ticket
  as.integer()

# Nearby tickets (for Part 2)
nearby_tickets <- 
  tibble(nearby_ticket = input[nearby_tickets_idx : length(input)]) %>%
  separate(nearby_ticket, 
          into = as.character(1:20),
          sep = ",") %>% 
  mutate(across(.fns = as.integer))
length(nearby_tickets)  
nrow(nearby_tickets)

# not vectorized
is_valid_field <- function(field, rules) {
  any(field >= rules$min1 & field <= rules$max1,
      field >= rules$min2 & field <= rules$max2)
}
is_valid_field(408, rules)
is_valid_field(1, rules)

valid_fields <- map_lgl(nearby_ticket_fields, function(field) is_valid_field(field, rules))
length(nearby_ticket_fields[!valid_fields]) # 49
sum(nearby_ticket_fields[!valid_tickets]) # 25972

# Part 2

# discard nearby tickets (rows) that include an invalid field
# so need to redo solution to part 1 to get rid of unlist()

# order of fields is consistent across nearby tickets and your ticket
# the names I gave the fields are not necessarily 

valid_nearby_tickets <-
  nearby_tickets %>% 
    rowwise() %>%  # apply mutates row-by-row because is_valid_field() is not vectorized
    mutate(across(.cols = everything(), 
                  .fns = ~ is_valid_field(.x, rules),
                  .names = "v_{col}")) %>% # indicator for each field
    ungroup() %>% mutate(valid = reduce(., `&`)) %>% 
    select(-starts_with("v_")) %>%
    filter(valid) 

# your ticket  

your_ticket <- 
  tibble(field_value = input[rules_idx + 3] %>% str_split(",") %>% unlist() %>% as.integer(),
         field_num = as.character(1:20)) %>%
  pivot_wider(names_from = field_num, values_from = field_value)

# field order is consistent among all valid tickets
# figure out mapping from field_num to field
# for example determine for field 1 determine which field rule is valid across all tickets

all_valid_tickets <-
  bind_rows(your_ticket, valid_nearby_tickets)

# for each field there is a rule number that returns all TRUE
all_valid_tickets %>%
  rowwise() %>%
  mutate(across(.cols = c(`1`), 
                .fns = list(r1 = ~ is_valid_field(`1`, rules[1,]),
                            r2 = ~ is_valid_field(`1`, rules[2,])),
                .names = "{col}_{fn}"
                )) %>%
  View()

# OK this works but how to expand to 20 fields and 20 rules and then summarize into
# short-cut:  only need to find the fields consistent with rules 1 to 6 the departure rules??


