library(dplyr, warn.conflicts = F)
library(readr)
library(tibble)
library(stringr)
library(tidyr)

source("helpers/aoc.R")
df <- read_lines(get_aoc("2022", "05"))

# test data ---------------------------------------------------------------
test <- list( t =
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2", 
a1 = "CMZ", 
a2 = "MCD")

df_ <- read_lines(test$t)

# -------------------------------------------------------------------------
get_cols <- function(lines_){
  s <- which.max(lines_ == "") - 1
  strsplit(trimws(lines_[s]), "\\s+") |> unlist() |> as.integer() |> max()
}
get_cols(df_)


get_state <- function(lines_){
  s <- which.max(lines_ == "") - 2
  read_fwf(paste(head(lines_,s),collapse = "\n"), fwf_widths(rep(4,get_cols(lines_)))) 
}
get_state(df_)
#> # A tibble: 3 × 3
#>   X1    X2    X3   
#>   <chr> <chr> <chr>
#> 1 <NA>  [D]   <NA> 
#> 2 [N]   [C]   <NA> 
#> 3 [Z]   [M]   [P]


get_instr <- function(lines_){
  s <- length(lines_) - which.max(lines_ == "")
  tail(lines_,s) %>% 
    # remove empty line
    .[. != ""] %>% 
    str_replace_all("(from|to)", ";") %>% 
    str_remove_all("[^0-9;]") %>% 
    enframe() %>% 
    separate(value, into = c("n", "from", "to"), convert = T) %>% 
    select(n,from,to)
}
get_instr(df_)
#> # A tibble: 4 × 3
#>       n  from    to
#>   <int> <int> <int>
#> 1     1     2     1
#> 2     3     1     3
#> 3     2     2     1
#> 4     1     1     2


# part 1 ------------------------------------------------------------------
part_1 <- function(d = df){
  state <- get_state(d) %>% as.matrix()

  # flip the matrx and add some convas to pile those crates
  # max matrix size 
  state <- rbind(
    state[nrow(state):1,],
    matrix(rep(NA, nrow(state) * (ncol(state) - 1)  * ncol(state)), ncol = ncol(state))
  )
  
  
  instr <- get_instr(d) %>% setNames(c("n", "from", "to"))
  instr <- instr[rep(1:nrow(instr),instr$n),c("from", "to")] %>% as.matrix()
  
  for (i in 1:nrow(instr)){
    from <- c(NA,instr[i,"from"])
    from[1] <- which.max(is.na(state[,from[2]])) - 1 
    
    to <- c(NA,instr[i,"to"])
    to[1] <- which.max(is.na(state[,to[2]]))
    
    state[to[1],to[2]] <- state[from[1],from[2]]
    state[from[1],from[2]] <- NA
  }
  paste0(apply(state,2, \(x) rev(x[!is.na(x)])[1]), collapse = "") %>% str_remove_all("\\W")  
}

# part 2 ------------------------------------------------------------------
part_2 <- function(d = df){
  state <- get_state(d) %>% as.matrix()
  state <- rbind(
    state[nrow(state):1,],
    matrix(rep(NA, nrow(state) * (ncol(state) - 1)  * ncol(state)), ncol = ncol(state))
  )
  
  instr <- get_instr(d) %>% setNames(c("n", "from", "to")) %>% as.matrix()
  
  for (i in 1:nrow(instr)){
    from_col <- instr[i,"from"]
    from_rows <- which.max(is.na(state[,from_col])) - 1
    from_rows <- (from_rows - instr[i,"n"] + 1):from_rows # [1] 1 2 3
    
    to_col <- instr[i,"to"]
    to_rows <- which.max(is.na(state[,to_col]))
    to_rows <- to_rows:(to_rows + instr[i,"n"] - 1)
    
    state[to_rows,to_col] <- state[from_rows,from_col]
    state[from_rows,from_col] <- NA
    # print(state[1:20,])
  }
  paste0(apply(state,2, \(x) rev(x[!is.na(x)])[1]), collapse = "") %>% str_remove_all("\\W")
}

# test --------------------------------------------------------------------
testthat::test_that("AOC", {
  df_ <- read_lines(test$t)
  # testthat::expect_equal(f1_(df_$X1, df_$X2),test$p1)
  # testthat::expect_equal(f2_(df_$X1, df_$X2),test$p2)
  
  testthat::expect_equal(part_1(df_),test$a1)
  testthat::expect_equal(part_2(!!df_),!!(test$a2))
})

# -------------------------------------------------------------------------
part_1()
part_2()



