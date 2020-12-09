# Day 9 - encryption
library(tidyverse)

### Part 1

### Example

input <- read_lines(file = "data-naa/input9_test.txt") %>% as.numeric()

valid <-
map_lgl(6:length(input), 
~ input[[.x]] %in% combn(input[(.x - 5):(.x - 1)], 2, FUN = sum))

input[6:20][!valid]

### Puzzle

input <- read_lines(file = "data-naa/input9.txt") %>% as.numeric()
preamble_len <- 25

invalid_number <- function(input, preamble_len) {
  # logical vector of valid numbers
  valid <-
    map_lgl(
      (preamble_len + 1):length(input), 
  # combn() makes the puzzle easy
      ~ input[[.x]] %in% combn(input[(.x - preamble_len):(.x - 1)], 2, FUN = sum)
    )
  # first of all the invalid numbers
  first(input[(preamble_len + 1):length(input)][which(valid == FALSE)])
}

part1_answer <- invalid_number(input, 25)  

### Part 2
# continguous set of arbitrary length that add to invalid number from Part 1

# search 2, 3, ... at a time

input <- read_lines(file = "data-naa/input9_test.txt") %>% as.numeric()
part1_answer <- invalid_number(input, 5)  

search_ans <- function(input, part1_answer) {
  .x <- 1
  set_len <- 2
  repeat{
    # answer found
    if(part1_answer == sum(input[.x : (.x + set_len - 1)])) {
      return(
        min(input[.x : (.x + set_len - 1)]) + 
        max(input[.x : (.x + set_len - 1)])
        )
    # set length doesn work, increment it and start over  
    } else if(.x == (length(input) - set_len) + 1) {
      set_len <- set_len + 1
      .x <- 1
    # increment start position of set  
    } else {
      .x <- .x + 1
    }
    # should guard against no anser
  }
}

search_ans(input, 127)

### Puzzle

input <- read_lines(file = "data-naa/input9.txt") %>% as.numeric()
preamble_len <- 25
part1_answer <- invalid_number(input, preamble_len)  
search_ans(input, part1_answer)

#### Another approach using map_dbl but needed more data to provide the answer

set_len <- 2
repeat {
if(part1_answer %in% 
  map_dbl(1:(len - set_len), 
      ~ sum(input[.x : (.x + set_len - 1)])
  )) break()

set_len <- set_len + 1
}
print(set_len) # need other information
