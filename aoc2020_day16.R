
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
    c("min1", "max1", "min2", "max2"),
    "(\\d+)-(\\d+) or (\\d+)-(\\d+)") %>%
  mutate(across(.fns = as.integer))

# Nearby tickets
nearby_tickets <- 
  str_split(input[nearby_tickets_idx : length(input)], ",") %>% 
  unlist() %>%
  as.integer()
length(nearby_tickets)  

is_valid <- function(ticket, rules) {
  any(ticket >= rules$min1 & ticket < rules$max1, 
      ticket >= rules$min2 & ticket < rules$max2)
}

# Valid tickets
valid_tickets <- map_lgl(nearby_tickets, function(ticket) is_valid(ticket, rules))
sum(nearby_tickets[!valid_tickets])

