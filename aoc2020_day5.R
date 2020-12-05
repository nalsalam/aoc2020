# Day 5
# Boarding pass

#' Take upper or lower half of the supplied vector
#'
#' If half is TRUE the upper half of rows is taken, o.w. the lower half
#'
#' @param rows, the supplied vector
#' @param half, logical
#'
take_half <- function(rows, half) {
  rows[half * length(rows)/2 + 1:(length(rows)/2)]
}

seat_id <- function(bp) {
  row <- (str_sub(bp, end = 7) %>% str_split(., ""))[[1]] %>% (function(b) {b == "B"})
  seat <- (str_sub(bp, start = 8) %>% str_split(., ""))[[1]] %>% (function(b) {b == "R"})

  #' subtract 1 at the end because AOC puzzle numbers rows starting with 0
  row_num <- reduce(row, take_half, .init = 1:128) - 1
  seat_num <- reduce(seat, take_half, .init = 1:8) - 1

  row_num * 8 + seat_num
}

testthat::expect_equal(
  map(c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"), seat_id),
  list(357, 567, 119, 820)
)


# Part 1

bps <- read_lines("data-naa/input5.txt")
bps_ids <- map_dbl(bps, seat_id)
max(bps_ids)

# Part 2

# Jam, what is another way of generating all seat ids?

all_seat_ids <- outer(c(0:7), c(0:127) * 8, FUN = "+") %>% as.vector()
front_row_seat_ids <- outer(c(0:7), c(0) * 8, FUN = "+") %>% as.vector()
back_row_seat_ids <- outer(c(0:7), c(127) * 8, FUN = "+") %>% as.vector()

empty_seats <- setdiff(all_seat_ids, bps_ids) %>%
  setdiff(front_row_seat_ids) %>%
  setdiff(back_row_seat_ids)

# empty seats between occupied seats
empty_seats[(empty_seats %in% (bps_ids + 1)) &
            (empty_seats %in% (bps_ids - 1))]


# Jam's variation:

seat_id_vectorized <- function(bp) {

  bp_txt_binary <- bp %>%
    str_replace_all("B|R", "1") %>%
    str_replace_all("F|L", "0")

  digits <- str_length(bp[[1]])

  total = 0
  for (i in 1:digits) {
    total = total + as.numeric(str_sub(bp_txt_binary, -i, -i)) * 2^(i-1)
  }

  total
}

testthat::expect_equal(
  seat_id_vectorized(c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")),
  c(357, 567, 119, 820)
)

# Part 1:

bps_jra <- read_lines("data-jra/input5.txt")
bps_ids_jra <- seat_id_vectorized(bps_jra)
max(bps_ids_jra)

# Part 2:

# find seat ids not in the input, but where +1 and -1 are in the input

candidate_seats_left <- function(all_ids, filled_ids) {
  setdiff(all_ids, filled_ids) %>%
    keep(., (. + 1) %in% filled_ids) %>%
    keep(., (. - 1) %in% filled_ids)
}

candidate_seats_left(
  all_ids = 0:seat_id_vectorized("BBBBBBBRRR"),
  filled_ids = bps_ids_jra
)

