library(dplyr, warn.conflicts = F)
library(readr)
library(tidyr)
source("helpers/aoc.R")

df <- read_csv(get_aoc("2022", "04"), col_names = F)
head(df)
# test data ---------------------------------------------------------------
test <- list( t =
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

df_ <- read_csv(test$t, col_names = F)

# -------------------------------------------------------------------------

df %>% 
  separate(X1, c("a","b")) %>% 
  separate(X2, c("c","d")) %>% 
  mutate(across(as.integer())) %>% 
  rowwise() %>% 
  mutate(contains = all(seq(a,b) %in% seq(c,d)) | all(seq(c,d) %in% seq(a,b)),
         overlaps = length(intersect(seq(a,b),seq(c,d))) > 0) %>% 
  ungroup() %>% 
  summarise(part_1 = sum(contains), 
            part_2 = sum(overlaps))
