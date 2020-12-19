# --- Day 15: Rambunctious Recitation ---

library(tidyverse)

# memory game -- saying numbers

# speak a number and update history
# treat history as a global and use <<- to update it inside the function
# keep history from current to old

play_game <- function(input, stop = 2020) {
  history <- rev(input)
  previous <- history[1L]

while(length(history) < stop) {  
  if(!(history[1] %in% history[-1L])) {
    history <- c(0L, history)
  }
  else {
    age <- first(which(history[-1L] == history[1L]))
    history <- c(age, history)
  }
}
history[1L]  
}

play_game2 <- function(input, stop) {
  s <- length(input)
  history <- rep(0L, stop)
  history[1:s] <- input %>% as.integer()
  
# guessing that %in% and which() slow this down  
  i <- s
  while(i < stop) {  
    if(!(history[i] %in% history[1: (i-1)])) {
      history[i+1] <- 0L
    }
    else {
      age <- i - last(which(history[1: (i-1)] == history[i]))
      history[i+1] <- age
    }
  i <- i + 1L
  }
history[stop]  
}

# From
# https://github.com/AdroMine/AdventOfCode2020/blob/main/Day15/solution.R

play_game3 <- function(input, stop) {

  # vector indexed by number from 0+ with value of the turn when last seen
  # the number cannot be larger than the number of turns
  
  # zero means never
  turn_seen <- rep(0L, stop) # safely long
  start <- length(input)

  # last number spoken and turns previous ones were spoken
  num <- input[start]
  turn_seen[input[-start] + 1L] <- 1L:(start - 1L)

  for(turn in (start + 1L):stop) {  
    
    # need at bottom for updating turn_seen
    tmp <- num
    
    if(turn_seen[num + 1L] != 0L) {
    # previous turn - turn spoken before then
      num <- turn - 1L - turn_seen[num + 1L] 
    }
    else {
      num <- 0L
    }
    turn_seen[tmp + 1L] <- turn - 1L
  }
  # number spoken 
  num
}

# Part 1 examples

play_game3(c(0, 3, 6), 2020) == 436
play_game3(c(1, 3, 2), 2020) == 1
play_game3(c(1, 2, 3), 2020) == 27
play_game3(c(2, 3, 1), 2020) == 78
play_game3(c(3, 2, 1), 2020) == 438
play_game3(c(3, 1, 2), 2020) == 1836

# Part 1 puzzle

system.time(play_game(c(15,12,0,14,3,1))) # 249, .07

# Part 2 -- need to speed up!
# Ideas
# preallocate history as 3e7 + 5

bench::mark(
  play_game(c(15,12,0,14,3,1), 1000),  
  play_game2(c(15,12,0,14,3,1), 1000), # 1.05 times faster
  play_game3(c(15,12,0,14,3,1), 1000), # 100 times faster!!! Much less memory
iterations = 10
)

# https://github.com/AdroMine/AdventOfCode2020/blob/main/Day15/day_15_readme.md
# use memoise for the if else part

play_game(c(0, 3, 6), 3e7) == 175594
play_game(c(1, 3, 2), 3e7) == 2578
play_game(c(2, 1, 3), 3e7) == 3544142
play_game(c(1, 2, 3), 3e7) == 261214
play_game(c(2, 3, 1), 3e7) == 6895259
play_game(c(3, 2, 1), 3e7) == 18
play_game(c(3, 1, 2), 3e7) == 362

system.time(play_game3(c(15,12,0,14,3,1), 3e7) %>% print(digits = 22)) 
# 41687 in 8.03 seconds
