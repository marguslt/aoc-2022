library(dplyr)
library(readr)

source("helpers/aoc.R")
df <- read_table(get_aoc("2022", "01"), col_names = F, skip_empty_rows = F)

# test data ---------------------------------------------------------------
test <- list( t = 
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000", a1 = 24000, a2 = 45000)



max_cal_count <- function (d = df, max_n = 1){
  rle_ <- rle(is.na(d$X1))
  d$grp <- rep(seq_along(rle_$lengths),rle_$lengths)
  
  d %>% filter(!is.na(X1)) %>% 
    group_by(grp) %>% 
    summarise(sum = sum(X1)) %>% 
    slice_max(sum, n = max_n) %>% 
    pull(sum) %>% 
    sum()
}

testthat::test_that("AOC Day 1", {
  df_ <- read_table(test$t, col_names = F, skip_empty_rows = F, col_types = "i")
  testthat::expect_equal(max_cal_count(df_),test$a1)
  testthat::expect_equal(max_cal_count(df_,3),test$a2)
})

# part 1 ------------------------------------------------------------------
# Most calories calories carried by single Elf  
max_cal_count(df,1)
# part 2 ------------------------------------------------------------------
# sum of calories  carried by top 3 Elves  
max_cal_count(df,3)
