# Day 12 Rain Risk

# The state of the ferry
initial_state <- 
  c(long = 0, lat = 0, facing = 0) 

# take the current state and and instruction and
# return the next state
step <- function(state, instruction) {
  long <- state[[1]]
  lat <- state[[2]]
  facing <- state[[3]]
  
  action = str_sub(instruction, 1, 1)
  amt = str_sub(instruction, 2) %>% as.numeric()
  
  if(action == "E") {
   long <- long + amt
  } else if(action == "S") {
   lat <- lat + amt
  } else if(action == "W") {
   long <- long - amt
  } else if(action == "N") {
   lat <- lat - amt
  } else if(action == "L") {
   facing <- (facing - amt / 90) %% 4
  } else if(action == "R") {
   facing <- (facing + amt / 90) %% 4
  } else if(action == "F" && facing == 0) {
    long <- long + amt
  } else if(action == "F" && facing == 1) {
    lat <- lat + amt
  } else if(action == "F" && facing == 2) {
    long <- long - amt
  } else if(action == "F" && facing == 3) {
    lat <- lat - amt
  } else {
    stop("Unrecognized action")
}
 c(long = long, lat = lat, facing = facing)
}  

solve <- function(initial_state, instructions) {
  state_current <- initial_state
  # print(c(" ", state_current))
  i <- 1
  while(i <= length(instructions)) {
    state_next <- step(state_current, instructions[i])
    state_current <- state_next
    # print(c(instructions[i], state_current))
    i <- i + 1
  }
  abs(state_current[[1]]) + abs(state_current[[2]])
}  

### Example

instructions <- read_lines("data-naa/input12_test.txt")
solve(initial_state, instructions)

### Part 1
instructions <- read_lines("data-naa/input12.txt")
solve(initial_state, instructions)

### Part 2
### F move the ferry in the direction of its heading
### The heading is defined in terms of run & rise (vs. angle)
### N,S,E,W change the ship's heading using run & rise
### L,R change the ships heading using angles

### The state of the ship is its position (long,lat) & heading (run,rise)
### initial state is 0,0 and  1,10

initial_state <- c(long = 0, lat = 0, run = 10, rise = -1)

step <- function(state, instruction) {
  long <- state[[1]]
  lat <- state[[2]]
  run <- state[[3]]
  rise <- state[[4]]
  
  action = str_sub(instruction, 1, 1)
  amt = str_sub(instruction, 2) %>% as.numeric()
  
  # change heading
  # change rise or run only
  if(action == "E") {
    run <- run + amt
  } else if(action == "S") {
    rise <- rise + amt
  } else if(action == "W") {
    run <- run - amt
  } else if(action == "N") {
    rise <- rise - amt
  
  # change both rise and run
  # L90 === R180
  } else if((action == "L" && amt / 90 == 1) || 
            (action == "R" && amt / 90 == 3)) {
      run_p <- run
      run  <- rise
      rise <- run_p * -1.0 
  # R90 === L180    
  } else if((action == "R" && amt / 90 == 1) ||
            (action == "L" && amt / 90 == 3)) {
      run_p <- run
      run <- rise * -1.0
      rise <- run_p
   # R180 === L180
  } else if((amt / 90 == 2) && (action %in% c("R", "L"))) {
      run <- run * -1.0
      rise <- rise * -1.0
  }
  
  # move ferry
    else if(action == "F") {
      long <- long + amt * run
      lat <- lat + amt * rise
    } else {
      
  # Error
      stop("Unrecognized action")      
    }
  c(long = long, lat = lat, run = run, rise = rise)
}

## Step by Step

state <- step(initial_state, "F10")
state
state <- step(state, "N3")
state
state <- step(state, "F7")
state
state <- step(state, "R90")
state
state <- step(state, "F11")
state

### Part 2 Example
instructions <- read_lines("data-naa/input12_test.txt")
solve(initial_state, instructions)

### Part 2 Puzzle
instructions <- read_lines("data-naa/input12.txt")
solve(initial_state, instructions)

