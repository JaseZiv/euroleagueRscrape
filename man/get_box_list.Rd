% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_scores.R
\name{get_box_list}
\alias{get_box_list}
\title{Scrape Box Score Lists}
\usage{
get_box_list(gamecode, seasoncode)
}
\arguments{
\item{gamecode}{the match number}

\item{seasoncode}{the year the season started with an "E" appended to the front}
}
\description{
This function scrapes box score lists for a given
match in season and the outputted list is used in 
`get_box_stats`.
}
