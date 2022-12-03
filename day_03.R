library(dplyr, warn.conflicts = F)
library(readr)
library(purrr)

source("helpers/aoc.R")
df <- read_table(get_aoc(), col_names = F)

# test data ---------------------------------------------------------------
# https://adventofcode.com/2022/day/3
test <- list( t =
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw", 
p1 = c("p", "L", "P", "v", "t", "s"),
p2 = c(16, 38, 42, 22, 20, 19),
p3 = c(18,52),
a1 = 157, 
a2 = 70)

items_in_both_comps <- function (rucksacs_content){
  map(rucksacs_content, ~ matrix(unlist(strsplit(.x, "")),ncol = 2)) %>% 
    map_chr(~ intersect(.x[,1], .x[,2])) %>%
    match(c(letters, LETTERS))
    
    # to keep item names :
    # set_names() %>% 
    # map_int(~ match(.x,c(letters, LETTERS)))
}

items_in_all <- function (rucksacs_content_df){
  rucksacs_content_df %>% 
    # groups of 3
    group_by(grp = ceiling(row_number() / 3)) %>% 
    summarise(
      item = strsplit(X1, "") %>% 
        reduce(intersect) %>% 
        match(c(letters, LETTERS))
    ) %>% pull(item) 
}

# part 1 ------------------------------------------------------------------
part_1 <- function(d = df){
  sum(items_in_both_comps(d[[1]]))
}

# part 2 ------------------------------------------------------------------
part_2 <- function(d = df){
  sum(items_in_all(d))
}

# test --------------------------------------------------------------------
testthat::test_that("AOC", {
  df_ <- read_table(test$t, col_names = F)
  # testthat::expect_equal(
  #   names(items_in_both_comps(df_$X1)),test$p1)
  testthat::expect_equal(items_in_both_comps(df_$X1),test$p2)
  testthat::expect_equal(items_in_all(df_),test$p3)
  
  testthat::expect_equal(part_1(df_),test$a1)
  testthat::expect_equal(part_2(df_),test$a2)
})

# -------------------------------------------------------------------------
part_1()
part_2()



