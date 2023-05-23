#' Scrape PBP data
#'
#' This functions scrapes pbp data for a game in a 
#' season and returns a data frame of pbp data
#' 
#' @param gamecode the match number
#' @param seasoncode the year the season started with an "E" appended to the front
#' 
#' @export 
get_each_pbp <- function(gamecode, seasoncode) {
  resp <- httr::GET(paste0("https://live.euroleague.net/api/PlaybyPlay?gamecode=", gamecode, "&seasoncode=", seasoncode)) |> httr::content()
  
  if(length(resp) == 0) {
    pbp <- data.frame()
  } else {
    pbp_events <- dplyr::bind_rows(
      resp$FirstQuarter |> dplyr::bind_rows(),
      resp$SecondQuarter |> dplyr::bind_rows(),
      resp$ThirdQuarter |> dplyr::bind_rows(),
      resp$ForthQuarter |> dplyr::bind_rows(),
      resp$ExtraTime |> dplyr::bind_rows()
    )
    
    pbp <- dplyr::bind_cols(Code=gamecode, SeasonCode=seasoncode, TeamA=resp$TeamA, TeamB=resp$TeamB, CodeTeamA=resp$CodeTeamA, 
                            CodeTeamB=resp$CodeTeamB, NumQuarters=resp$ActualQuarter, pbp_events)
  }
  
  pbp <- janitor::clean_names(pbp)
  
  return(pbp)
}