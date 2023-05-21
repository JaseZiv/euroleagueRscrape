#' Scrape PBP data
#'
#' This functions scrapes pbp data for a game in a 
#' season and returns a data frame of pbp data
#' 
#' @param seasoncode the year the season started with an "E" appended to the front
#' @param round_phase either 'RS' for regular season or 'PO' for playoffs
#' @param round_number integer of the round number
#' 
#' @importFrom rlang .data
#'
#' @export
match_results <- function(seasoncode, round_phase, round_number) {
  
  params = list(
    `teamCode` = "",
    `phaseTypeCode` = round_phase,
    `roundNumber` = round_number
  )
  
  res_games <- httr::GET(url = paste0("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/", seasoncode, "/games"), query = params) |>  
    httr::content()
  
  games <- res_games$data |> jsonlite::toJSON() |> jsonlite::fromJSON() |> data.frame()
  
  
  match_meta <- games |>  
    dplyr::select(tidyselect::any_of(c("id", "code", "date", "season", "competition", "group", "phaseType", "round", "minute", "venue", "confirmedDate", "confirmedTime", "audience", "audienceConfirmed"))) |>  
    tidyr::unnest(c(.data$season, .data$competition, .data$group, .data$phaseType, .data$round, .data$venue), names_sep = "_")
  
  home_df <- parse_h_a_results(games, "home")
  away_df <- parse_h_a_results(games, "away")
  
  match_results <- dplyr::bind_cols(
    match_meta,
    home_df,
    away_df
  )
  
  match_results <- janitor::clean_names(match_results)
  
  return(match_results)
}