# Day 11: Seating System
library(tidyverse)

#### functions

# use math to count surrounding seats
# returns a numeric vector
# not vectorized, so slow
# create list once

cr_surr_ids <- function(seat_num, row = 92, col = 91) {
  seat_num <- seat_num - 1
  same_row_seats <- (seat_num %% col) + 0:2
  same_row_seats <- same_row_seats[same_row_seats %in% 1:col]
  x <- c(
    (col * (seat_num %/% row - 1)) + same_row_seats,
    (col * (seat_num %/% row + 0)) + same_row_seats,
    (col * (seat_num %/% row + 1)) + same_row_seats
  )
  setdiff(x[x %in% 1:(row * col)], seat_num + 1)
}

cr_seat_state_df <- function(seats_input, n_seats) {
  tibble(
    seat_num = 1:n_seats,
    seat_state = seats_input,
    empty_seats = map_dbl(1:n_seats, ~ sum(seat_state[surr_ids[[.x]]] == "L")),
    occupied_seats = map_dbl(1:n_seats, ~ sum(seat_state[surr_ids[[.x]]] == "#"))
  )
}

update_state <- function(seat_state_df) {
  df <- seat_state_df %>% 
  mutate(
    seat_state = case_when(
      seat_state == "L" & occupied_seats == 0 ~ "#",
      seat_state == "#" & occupied_seats >= 4 ~ "L",
      TRUE ~ seat_state
    )) %>% 
  mutate(
    empty_seats = map_dbl(1:n_seats, ~ sum(seat_state[surr_ids[[.x]]] == "L")),
    occupied_seats = map_dbl(1:n_seats, ~ sum(seat_state[surr_ids[[.x]]] == "#"))
  )    
}

occupied_seat_in_ss <- function(seat_state_df) {
  iter <- 0
  df_next <- df <- seat_state_df
  df_next[df == "L"] <- "#"
  while(any(df_next!=df)) {
    df <- df_next
    df_next <- update_state(df)
    iter <- iter + 1
    message(iter)
  }
  sum(df_next == "#")
}

## debugging functions

picture <- function(seat_state_df, row, col) {
  seat_state_df %>% 
    select(seat_state) %>%
    pull(seat_state) %>% matrix(nrow = row, ncol = col, byrow = TRUE)
}

seat_summary <- function(seat_state_df) {
  seat_state_df %>%
    summarize(
      n_empty_seats = sum(seat_state == "L"),
      n_occupied_seats = sum(seat_state == "#")
    )
}

# Example

rows <- length(read_lines("data-naa/input11_test.txt"))
cols <- str_length(read_lines("data-naa/input11_test.txt")[[rows]])
n_seats <- rows * cols

seats_input <- 
  read_lines("data-naa/input11_test.txt") %>%
  str_split_fixed(., "", str_length(.[[1]])) %>% t() %>% as.vector()
length(seats_input) == n_seats

surr_ids <- map(1:n_seats, cr_surr_ids, row = 10, col = 10)
seat_state_df <- cr_seat_state_df(seats_input, n_seats)
occupied_seat_in_ss(seat_state_df)

# Puzzle

rows <- length(read_lines("data-naa/input11.txt"))
cols <- str_length(read_lines("data-naa/input11.txt")[[rows]])
n_seats <- rows * cols

seats_input <- 
  read_lines("data-naa/input11.txt") %>%
  str_split_fixed(., "", str_length(.[[1]])) %>% t() %>% as.vector()
length(seats_input) == n_seats

surr_ids <- map(1:n_seats, cr_surr_ids, row = 92, col = 91)

seat_state_df <- cr_seat_state_df(seats_input, n_seats)

occupied_seat_in_ss(seat_state_df) # not converging

# Debug
# surr_ids -- looks good
surr_ids[[1]] 
surr_ids[[91]]
surr_ids[[92]]
surr_ids[[91*3]]
surr_ids[[91*91]]
surr_ids[[93]]

# seat_state_df -- looks good
picture(seat_state_df, 92, 91)[, 20]
seat_state_df %>% select(seat_state) %>% slice(1:10)
seats_input[1:10]

# update_state
update_n_times <- function(seat_state_df, n) {
  if(n == 0) return(seat_state_df)
  else update_n_times(update_state(seat_state_df), n-1)
  }
# bouncing back and forth
seat_state_100 <- update_n_times(seat_state_df, 100)
seat_state_100 %>% seat_summary()
seat_state_101 <- update_n_times(seat_state_df, 101)
seat_state_101 %>% seat_summary()
seat_state_102 <- update_n_times(seat_state_df, 102)
seat_state_102 %>% seat_summary()
seat_state_103 <- update_n_times(seat_state_df, 103)
seat_state_103 %>% seat_summary()
seat_state_104 <- update_n_times(seat_state_df, 104)
seat_state_104 %>% seat_summary()
anti_join(seat_state_100, seat_state_101)
anti_join(seat_state_101, seat_state_103)
anti_join(seat_state_100, seat_state_102)
seat_state_100 %>% summarize(occupied_seats = sum(seat_state == "#"))
seat_state_101 %>% summarize(occupied_seats = sum(seat_state == "#"))

# Part 2 --
# Only the seats along (unblocked) rays are seen 
# 5+ occupied to go to empty
