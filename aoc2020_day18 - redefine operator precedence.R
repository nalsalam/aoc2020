# --- Day 18: Operation Order ---

# rules of operator precedence have changed -- purely left to right
# parenthesis override these

# Evaluate the expression on each line of the homework; what is the sum of the resulting values?

# Part 1 - left to right precedence

# Create infix operators that have left to right precedence
`%+%` <- function(a, b) { a + b}
`%*%` <- function(a, b) { a * b}

# Examples
input <- read_lines("data-naa/input18_test.txt") %>%
  str_replace_all("\\+", "%+%") %>% str_replace_all("\\*", "%*%")
for(i in 1:length(input)) {
  print(eval(parse(text = input[[i]])))
}

# Puzzle
input <- read_lines("data-naa/input18.txt") %>%
  str_replace_all("\\+", "%+%") %>% str_replace_all("\\*", "%*%")
homework <- vector(mode = "integer", length = length(input))
for(i in 1:length(input)) {
  homework[[i]] <- eval(parse(text = input[[i]]))
}
sum(homework) %>% print(digits = 22)

# Part 2 - addition has higher precedence that multiplication

# Examples -- 
input <- read_lines("data-naa/input18_test.txt") %>%
  input[[5]] %>%
  str_replace_all(" (\\d \\+ \\d) ", " \\(\\1\\) ") %>%
  str_replace_all(" (\\d \\+ )(\\(\\d [+*] \\d\\) )", " \\(\\1\\2\\) ") %>% 
  str_replace_all(" (\\(\\d [+*] \\d\\))( \\+ \\d)", " \\(\\1\\2\\)")


for(i in 1:length(input)) {
  print(eval(parse(text = input[[i]])))
}


