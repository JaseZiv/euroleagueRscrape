#' Scrape season round information
#'
#' This functions scrapes season rounds data 
#' and returns a data frame of round information
#' including round name and number, phase type, 
#' and round starting and ending dates
#' 
#' @param seasoncode the year the season started with an "E" appended to the front
#' 
#' @importFrom rlang .data
#' 
#' @export
get_season_rounds <- function(seasoncode) {
  
  print(paste0("scraping season: ", seasoncode))
  Sys.sleep(1)
  
  res_rounds <- httr::GET(url = paste0("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/", seasoncode, "/rounds")) |> 
    httr::content()
  
  rounds <- res_rounds$data |> dplyr::bind_rows()
  rounds <- rounds |> dplyr::arrange(.data$minGameStartDate)
  rounds <- janitor::clean_names(rounds)
  
  return(rounds)
  
}