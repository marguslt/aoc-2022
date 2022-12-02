library(dplyr)
library(readr)

source("helpers/aoc.R")
df <- read_table(get_aoc("2022", "02"), col_names = F)

# test data ---------------------------------------------------------------
test <- list( t =
"A Y
B X
C Z", p1 = c(8, 1, 6),
      p2 = c("X", "X", "X"),
      a1 = 15, 
      a2 = 12)


# part 1 ------------------------------------------------------------------
# A for Rock, B for Paper, and C for Scissors
# X for Rock, Y for Paper, and Z for Scissors
# Your total score is the sum of your scores for each round. The score 
# for a single round is the score for the shape you selected 
# (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome 
# of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

score <- function(oponent_moves, my_moves){
  m_score <- matrix( c(4, 8, 3, 
                       1, 5, 9, 
                       7, 2, 6), 
                     nrow = 3, 
                     byrow = T, 
                     dimnames = list(oponent = c("A", "B", "C"), me = c("X","Y","Z")))
  m_score[cbind(oponent_moves, my_moves)]
}

part_1 <- function(d = df){
  sum(score(d[[1]], d[[2]]))
}




# part 2 ------------------------------------------------------------------
move <- function(oponent_moves, my_round_results){
  # Anyway, the second column says how the round needs to end: 
  # X means you need to lose, 
  # Y means you need to end the round in a draw, and 
  # Z means you need to win.
  
  #                     Lose Draw Win
  #                     X(R) Y(P) Z(S)
  m_move  <- matrix( c("Z", "X", "Y", # A - rock
                       "X", "Y", "Z", # B - paper
                       "Y", "Z", "X"),# C - scissors
                     nrow = 3, 
                     byrow = T, 
                     dimnames = list(oponent = c("A", "B", "C"), me = c("X","Y","Z")))
  m_move[cbind(oponent_moves, my_round_results)]
}

part_2 <- function(d = df){
  d$move <- move(d[[1]], d[[2]])
  sum(score(d[[1]],d$move))
}


# test --------------------------------------------------------------------
testthat::test_that("AOC Day 2", {
  df_ <- read_table(test$t, col_names = F)
  testthat::expect_equal(score(df_$X1, df_$X2),test$p1)
  testthat::expect_equal(move(df_$X1, df_$X2),test$p2)
  
  testthat::expect_equal(part_1(df_),test$a1)
  testthat::expect_equal(part_2(df_),test$a2)
})

# -------------------------------------------------------------------------
part_1()
part_2()



