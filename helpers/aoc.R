library(httr)
# .aoc_session contains session cookie

get_aoc <- function(year=NULL, day=NULL){
  AOC_COOKIES <- c(session = readr::read_file(".aoc_session"))
  if (is.null(year) || is.null(day)){
    date <-strsplit(format(Sys.Date(), "%Y,%d"), ",") |> unlist()
    year <- date[1]    
    day <- date[2]    
  }
  filename <- paste0("in/input_", day)
  url <- paste0("https://adventofcode.com/",year,"/day/",as.numeric(day),"/input")
  message(paste(url, ">>", filename))
  if (!file.exists(filename)){
    resp <- GET(url, set_cookies(AOC_COOKIES))
    write(content(resp, as = "text"), file = filename)
  }
  return(filename)
}
