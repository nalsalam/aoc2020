# Day 8 - boot computer with R6

library(tidyverse)
library(R6)
# https://r6.r-lib.org/articles/Introduction.html

# a program is a series of instructions
# A computer runs a program
Computer <-
  R6Class("Computer",
          
    public = list(
      # binding to be initialized
      pgm = NULL,
      line = NULL,
      acc_g = NULL,
      it = NULL,

      # called when an instance is created with $new()
      initialize = function(pgm = NA) {
        self$pgm <- pgm
        self$line <- 1
        self$acc_g <- 0
        # decided to make this a state of the computer, not the program.  
        # Not sure.
        self$it <- rep(0, times = nrow(self$pgm))
      },
      
      # called with $run() on the instance
      run = function() {
        
        repeat {
          line <- self$line
          # update state
          self$it[line] <- self$it[line] + 1
          # oh oh, loop
          if(self$it[line] == 2) {
            term <- "loop"
            break() 
          }
        
          # read the program op and execute it
          # note the use of private$ vs. self$
          if(self$pgm$op[line] == "nop") {
            private$nop()
          } else if(self$pgm$op[line] == "jmp") {
            private$jmp(self$pgm$arg[line])
          } else {
            private$acc(self$pgm$arg[line])
          }  
          
          # check if done
          if(self$line > nrow(self$pgm)) {
            term <- "normal"
            break() # normal termination
          }
        }
      return(c(self$acc_g, term))
      },
      
      print = function() {
        print(self$pgm)
      }
    ),
    
    # ops are accessed by run method
    private = list(
      
      nop = function() {
        self$line <- self$line + 1
      },
      
      jmp = function(arg) {
        self$line <- self$line + arg
      },
      
      acc = function(arg) {
        self$acc_g <- self$acc_g + arg
        self$line <- self$line + 1
      }
    )
    
  )
# Test
boot_pgm <-
  read_delim(file = "data-naa/input8_test.txt", delim = " ", col_names = c("op", "arg"))
boot_cmp <- Computer$new(pgm = boot_pgm) # create a computer with boot_pgm
boot_cmp$run() 

# Part 1
boot_pgm <-
  read_delim(file = "data-naa/input8.txt", delim = " ", col_names = c("op", "arg"))
boot_cmp <- Computer$new(pgm = boot_pgm) # create a computer with boot_pgm
boot_cmp$run() 

# Part 2
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
    
    # Only difference with R6
    boot_cmp <- Computer$new(pgm = pgm_edited) # create a computer with boot_pgm
    termination <- boot_cmp$run() 
    
    if(termination[2] == "normal") break()
  }
  return(termination)
}

fix_pgm(boot_pgm)
