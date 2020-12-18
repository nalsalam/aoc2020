# --- Day 15: Rambunctious Recitation ---

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

# https://github.com/AdroMine/AdventOfCode2020/blob/main/Day15/solution.R
find_n_num <- function(starting_nums, n){
  starting_nums <- as.integer(starting_nums)
  s <- length(starting_nums)
  
  last_seen <- rep(0L, n)
  
  # 0 saved at 1
  last_seen[starting_nums[-s] + 1L] <- 1L:(s-1L)
  
  num <- starting_nums[s] 
  for(turn in (s+1L):n){
    tmp <- num
    if(last_seen[num+1L]!=0L){ 
      num <- turn - 1L - last_seen[num + 1L]
    } else{                
      num <- 0L
    }
    last_seen[tmp + 1L] <- turn - 1L
  }
  num
}

# Part 1 examples

play_game(c(0, 3, 6)) == 436
play_game(c(1, 3, 2)) == 1
play_game(c(1, 2, 3)) == 27
play_game(c(2, 3, 1)) == 78
play_game(c(3, 2, 1)) == 438
play_game(c(3, 1, 2)) == 1836

# Part 1 puzzle

system.time(play_game(c(15,12,0,14,3,1))) # 249, .07

# Part 2 -- need to speed up!
# Ideas
# preallocate history as 3e7 + 5

bench::mark(
  play_game(c(15,12,0,14,3,1), 1000),  
  play_game2(c(15,12,0,14,3,1), 1000), # 1.05 times faster
  find_n_num(c(15,12,0,14,3,1), 1000), # 100 times faster!!! Much less memory
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

system.time(play_game(c(15,12,0,14,3,1), 1e6) %>% print(digits = 22))

system.time(find_n_num(c(15,12,0,14,3,1), 3e7) %>% print(digits = 22)) 
# 41687 in 8.03 seconds
