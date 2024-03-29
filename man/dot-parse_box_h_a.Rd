% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_scores.R
\name{.parse_box_h_a}
\alias{.parse_box_h_a}
\title{Parse Home and Away box scores}
\usage{
.parse_box_h_a(box_list, home_away, team_player)
}
\arguments{
\item{box_list}{datafram of a scraped match}

\item{home_away}{whether `"home"` or `"away"`}

\item{team_player}{whether you want box scores for "team" or "player"}
}
\description{
This functions parses scraped box stats for each match
and separates them for each team in the match
}
