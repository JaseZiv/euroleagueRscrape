#' Scrape shot and shot location data
#'
#' This functions scrapes shooting location data for 
#' a game in a season and returns a data frame of 
#' shooting data since the 2015-2016 season
#' 
#' @param gamecode the match number
#' @param seasoncode the year the season started with an "E" appended to the front 
#'
#' @export 
get_each_shots <- function(gamecode, seasoncode) {
  
  resp <- httr::GET(paste0("https://live.euroleague.net/api/Points?gamecode=", gamecode, "&seasoncode=", seasoncode, "&disp=")) |> httr::content()
  dat_out <- data.frame(code=gamecode, season_code=seasoncode)
  dat_out <- dat_out |> dplyr::bind_cols(
    resp[1] |> dplyr::bind_rows()
  )
  
  dat_out <- janitor::clean_names(dat_out)
  
  return(dat_out)
  
}