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
a1 = 7, 
a2 = 19)

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
