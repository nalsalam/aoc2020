
library(tidyverse)
 
df <- tibble(
  x = c(1, 2),
  y = c(10, 20)
)

mult <- c(11, -2)

mulfn <- function(x, y) x * y

fnslist1 <- list(
  f1 = ~ mulfn(.x, mult[1]),
  f2 = ~ mulfn(.x, mult[2])
)

df2 <- df %>%
  mutate(across(c(x, y), fnslist1))

fnslist2 <- map(mult, ~partial(mulfn, y = .x)) %>% set_names(c("f1", "f2"))

df3 <- df %>%
  mutate(across(c(x, y), fnslist2))

all.equal(df2, df3)
