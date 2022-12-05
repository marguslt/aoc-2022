library(httr)
library(rvest)
# .aoc_session contains session cookie

get_aoc <- function(year_=NULL, day_=NULL){
  AOC_COOKIES <- c(session = readr::read_file(".aoc_session"))
  if (is.null(year_) || is.null(day_)){
    date_ <-strsplit(format(Sys.Date(), "%Y,%d"), ",") |> unlist()
    year_ <- date_[1]    
    day_ <- date_[2]    
  }
  filename <- paste0("in/input_", day_)
  url <- paste0("https://adventofcode.com/",year_,"/day/",as.numeric(day_),"/input")
  message(paste(url, ">>", filename))
  if (!file.exists(filename)){
    resp <- GET(url, set_cookies(AOC_COOKIES))
    write(content(resp, as = "text"), file = filename)
  }
  message("\n# top 25 lines ------------------------------------------------------------\n")
  read_lines(filename, n_max = 25) %>% paste(collapse = "\n") %>% cat()
  message("\n# -------------------------------------------------------------------------\n")

  return(filename)
}

get_sample <- function(year_=NULL, day_=NULL){
  if (is.null(year_) || is.null(day_)){
    date_ <-strsplit(format(Sys.Date(), "%Y,%d"), ",") |> unlist()
    year_ <- date_[1]    
    day_ <- date_[2]    
  }
  filename <- paste0("in/sample_1_", day_)
  url <- paste0("https://adventofcode.com/",year_,"/day/",as.numeric(day_) )
  message(cli::style_hyperlink(url, url))
  if (!file.exists(filename)){
    aoc <- read_html(url)
    aoc %>% html_element("article.day-desc > pre:first-of-type > code") %>% html_text() %>% write_file(filename)
  }
  s <-read_file(filename) 
  message("\n# Part 1 sample -----------------------------------------------------------\n")
  s %>% cat()
  message("\n# -------------------------------------------------------------------------\n")
  return(s)
}
