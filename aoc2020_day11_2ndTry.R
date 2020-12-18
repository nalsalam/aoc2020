# Day 11 -- try again with matrix 

library(tidyverse)

parse_input <- function(input) {
  seats <- 
    input %>% 
    str_replace_all("L", "0") %>%
    str_replace_all("#", "1") %>%
    str_split("") %>% unlist() %>% as.numeric() %>%
    matrix(nrow = length(input), ncol = str_length(input[1]), byrow = TRUE) 
  seats <- rbind(NA_integer_, seats, NA_integer_)
  seats <- cbind(NA_integer_, seats, NA_integer_)
  return(seats)
}

# Update seats

update_seats <- function(seats) {
  seats_new <- seats
  for(r in 2:(R-1)) {
    for(c in 2:(C-1)) {
      seat <- seats[r, c]
      if(is.na(seat)) next()
      occ_adj <- sum(seats[(r-1):(r+1), (c-1):(c+1)], na.rm = TRUE) - seat
      if(seat == 0 && occ_adj == 0) seats_new[r, c] <- 1 
      else if(seat ==1 && occ_adj >= 4) seats_new[r, c] <- 0 
      else next()
      }
    }
  seats_new
}

solve <- function(seats) {
  seats_new <- seats_old <- seats
  seats_new[seats_old == 0] <- 1
  while(any(seats_new != seats_old, na.rm = TRUE)) {
    seats_old <- seats_new
    seats_new <- update_seats(seats_old)
  }
  sum(seats_new == 1, na.rm = TRUE)
}

# Example

input <- readLines("data-naa/input11_test.txt")
seats <- parse_input(input)
R <- nrow(seats)
C <- ncol(seats) 
solve(seats)

# Part 1

input <- readLines("data-naa/input11.txt") 
seats <- parse_input(input)
R <- nrow(seats)
C <- ncol(seats) 
solve(seats)

# Part 2

# diagonal directions

update_seats <- function(seats) {
  seats_new <- seats
  for(r in 2:(R-1)) {
    for(c in 2:(C-1)) {
      seat <- seats[r, c]
      if(is.na(seat)) next()
      
      occ_adj <- 0
      # N
      d <- 1
      while(is.na(seats[r-d, c]) && (r-d) != 1) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r-d, c], na.rm = TRUE)
      # NE
      d <- 1
      while(is.na(seats[r-d, c+d]) && (r-d) != 1 && c+d != C) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r-d, c+d], na.rm = TRUE)
      # E
      d <- 1
      while(is.na(seats[r, c+d]) && (c+d) != C) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r, c+d], na.rm = TRUE)
      # SE
      d <- 1
      while(is.na(seats[r+d, c+d]) && (r+d) != R && c+d != C) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r+d, c+d], na.rm = TRUE)
      # S
      d <- 1
      while(is.na(seats[r+d, c]) && (r+d) != R) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r+d, c], na.rm = TRUE)
      # SW
      d <- 1
      while(is.na(seats[r+d, c-d]) && (r+d) != R && c-d != 1) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r+d, c-d], na.rm = TRUE)
      # W
      d <- 1
      while(is.na(seats[r, c-d]) && (c-d) != 1) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r, c-d], na.rm = TRUE)
      # NW
      d <- 1
      while(is.na(seats[r-d, c-d]) && (r-d) != 1 && c-d != 1) {d <- d + 1}
      occ_adj <- sum(occ_adj, seats[r-d, c-d], na.rm = TRUE)
      
      if(seat == 0 && occ_adj == 0) seats_new[r, c] <- 1 
      else if(seat ==1 && occ_adj >= 5) seats_new[r, c] <- 0 
      else next()
    }
  }
  seats_new
}

input <- readLines("data-naa/input11_test.txt")
seats <- parse_input(input)
R <- nrow(seats)
C <- ncol(seats) 

solve(seats)

input <- readLines("data-naa/input11.txt")
seats <- parse_input(input)
R <- nrow(seats)
C <- ncol(seats) 

solve(seats)
