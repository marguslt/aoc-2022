library(dplyr)
library(readr)

source("helpers/aoc.R")
df <- read_table(get_aoc(), col_names = F)

# test data ---------------------------------------------------------------
test <- list( t =
"", 
p1 = c(),
p2 = c(),
a1 = 0, 
a2 = 0)


# part 1 ------------------------------------------------------------------
part_1 <- function(d = df){
  
}

# part 2 ------------------------------------------------------------------
part_2 <- function(d = df){

}

# test --------------------------------------------------------------------
testthat::test_that("AOC", {
  df_ <- read_table(test$t, col_names = F)
  # testthat::expect_equal(f1_(df_$X1, df_$X2),test$p1)
  # testthat::expect_equal(f2_(df_$X1, df_$X2),test$p2)
  
  testthat::expect_equal(part_1(df_),test$a1)
  testthat::expect_equal(part_2(df_),test$a2)
})

# -------------------------------------------------------------------------
part_1()
part_2()



