# -------------------------------------------------------------------------
library(dplyr, warn.conflicts = F)
library(readr)
library(testthat)
library(tidyverse)
source("helpers/aoc.R")

test <- list(t = get_sample())
df <- read_table(get_aoc(), col_names = F)

# test data ---------------------------------------------------------------
test <- list( t =
"", 
p1 = c(),
p2 = c(),
a1 = 0, 
a2 = 0)

df_ <- read_table(test$t, col_names = F)

# part 1 ------------------------------------------------------------------
part_1 <- function(d = df){
  
}

# part 2 ------------------------------------------------------------------
part_2 <- function(d = df){

}

# test --------------------------------------------------------------------
test_that("AOC", {
  df_ <- read_table(test$t, col_names = F)
  # expect_equal(f1_(df_$X1, df_$X2),test$p1)
  # expect_equal(f2_(df_$X1, df_$X2),test$p2)
  
  expect_equal(
    part_1(!!df_),
    !!test$a1 )
  expect_equal(
    part_2(!!df_),
    !!test$a2 )
})

# -------------------------------------------------------------------------
part_1()
part_2()
