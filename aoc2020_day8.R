# Day 8
library(tidyverse)

# Boot code
# Find state of accumulator when execution of the code finishes loop 1 of an infinite loop 

pgm <-
  read_delim(file = "data-naa/input8_test.txt", delim = " ", col_names = c("op", "arg")) %>%
  # add col for visit number
  mutate(
    it = 0 # iteration number
  )

#' Run program
#' 
#' @param pgm, the program
#' @acc, starting value of accumulator
#' @line, starting line number
#' 
run_pgm <- function(pgm, acc, line) {
  repeat {
    # indicate visited  
    pgm$it[line] <- pgm$it[line] + 1
    if(pgm$it[line] == 2) {
      term <- "loop"
      break() # oh oh, loop
    }
    # execute operation 
    if(pgm$op[line] == "nop") {
      line <- line + 1
    } else if(pgm$op[line] == "jmp") {
        line <- line + pgm$arg[line]
    } else {
      acc <- acc + pgm$arg[line] 
      line <- line + 1
    }
    
    if(line > nrow(pgm)) {
      term <- "normal"
      break() # normal termination
    }
  }
  return(c(acc, term))
}  

run_pgm(pgm, 0, 1)

# Part 1

pgm <-
  read_delim(file = "data-naa/input8.txt", delim = " ", col_names = c("op", "arg")) %>%
  mutate(
    it = 0
  )

run_pgm(pgm, 0, 1)

# Part 2
# jmp to nop or nop to jmp

# edit program, run, until "normal" termination

edit_pgm <- function(pgm, line) {
  if (pgm$op[line] == "jmp") {
      pgm$op[line] <- "nop"
  } else if (pgm$op[line] == "nop") {
    pgm$op[line] <- "jmp"
  } else {
  }
  return(pgm)
}

fix_pgm <- function(pgm) {
  for(line in seq(1:nrow(pgm))) {
    pgm_edited <- edit_pgm(pgm, line)
    termination <- run_pgm(pgm_edited, 0, 1)
    if(termination[2] == "normal") break()
  }
  return(termination)
}

fix_pgm(pgm)





