# -------------------------------------------------------------------------
library(dplyr, warn.conflicts = F)
library(readr)
library(testthat)
library(tidyverse)
source("helpers/aoc.R")

test <- list(t = get_sample())
df <- read_file(get_aoc())

# test data ---------------------------------------------------------------
test <- list( t =
"mjqjpqmgbljsphdztnvjfqwrcgsmlb", 
p1 = c(),
p2 = c(),
a1 = 7, 
a2 = 19)

df_ <- test$t

first_msg <- function(data, pkg_len){
  s <- str_split(data,"", simplify = T) 
  for(i in pkg_len:length(s)){
    if (length(unique(s[(i-pkg_len+1):i]))>=pkg_len){
      break
    }
  }
  i
}

# part 1 ------------------------------------------------------------------
part_1 <- function(d = df){
  first_msg(d,4)
}

# part 2 ------------------------------------------------------------------
part_2 <- function(d = df){
  first_msg(d,14)
}

# test --------------------------------------------------------------------
test_that("AOC", {
  df_ <- test$t
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
