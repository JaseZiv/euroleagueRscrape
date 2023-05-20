#' Parse Home and Away box scores
#'
#' This functions parses scraped box stats for each match
#' and separates them for each team in the match
#'
#' @param box_list datafram of a scraped match
#' @param home_away whether `"home"` or `"away"`
#' @param team_player whether you want box scores for "team" or "player"
#' 
#' @importFrom rlang .data
#'
#' @export
.parse_box_h_a <- function(box_list, home_away, team_player) {
  
  if(home_away == "home") {
    h_a <- 1
  } else {
    h_a <- 2
  }
  
  team <- box_list$Stats[[h_a]]$Team
  coach <- box_list$Stats[[h_a]]$Coach
  
  
  box_stats <- box_list$Stats
  team_abbrv <- box_stats[[h_a]]$tmr$Team
  
  if(team_player == "team") {
    qbq <- box_list$ByQuarter |> dplyr::bind_rows()
    qbq <- qbq[h_a, ] |> dplyr::select(-.data$Team)
    box_stats_df <- box_stats[[h_a]]$totr |> dplyr::bind_rows()
    dat_out <- dplyr::bind_cols(team=team, team_abbrv = team_abbrv, coach=coach, qbq, box_stats_df)
    
  } else if (team_player == "player") {
    box_stats_df <- box_stats[[h_a]]$PlayersStats |> dplyr::bind_rows()
    dat_out <- dplyr::bind_cols(player_team=team, player_team_abbrv = team_abbrv, player_coach=coach, box_stats_df)
    
  } else {
    stop("select either 'team' or 'player'")
  }
  
  return(dat_out)
  
}



#' Scrape Box Score Lists
#'
#' This function scrapes box score lists for a given
#' match in season and the outputted list is used in 
#' `get_box_stats`. 
#' 
#' @param gamecode the match number
#' @param seasoncode the year the season started with an "E" appended to the front 
#'
#' @export
get_box_list <- function(gamecode, seasoncode) {
  box_list <- httr::GET(paste0("https://live.euroleague.net/api/Boxscore?gamecode=", gamecode, "&seasoncode=", seasoncode)) |> httr::content()
  return(box_list)
}




#' Parse box score lists
#'
#' This functions parses scraped box score lists
#' and returns a data frame of box score statistics for
#' either the team level, or for players
#' 
#' @param box_list the list of a match's box score data
#' @param team_or_player whether you want box scores for "team" or "player"
#'
#' @export 
get_box_stats <- function(box_list, team_or_player) {
  
  if(team_or_player == "team") {
    # first get a data frame of home and away team box scores
    home_team_box <- .parse_box_h_a(box_list = box_list, home_away = "home", team_player = team_or_player)
    away_team_box <- .parse_box_h_a(box_list = box_list, home_away = "away", team_player = team_or_player)
    box_out_team <- dplyr::bind_rows(home_team_box, away_team_box)
    # box_out_team <- bind_cols(season = seasoncode, code=gamecode, box_out_team)
    box_out_team <- janitor::clean_names(box_out_team)
    return(box_out_team)
  } else {
    # then get a data frame of home and away player box scores  
    home_player_box <- .parse_box_h_a(box_list = box_list, home_away = "home", team_player = team_or_player)
    away_player_box <- .parse_box_h_a(box_list = box_list, home_away = "away", team_player = team_or_player)
    box_out_player <- dplyr::bind_rows(home_player_box, away_player_box)
    # box_out_player <- bind_cols(season = seasoncode, code=gamecode, box_out_player)
    box_out_player <- janitor::clean_names(box_out_player)
    return(box_out_player)
  }
  
}
