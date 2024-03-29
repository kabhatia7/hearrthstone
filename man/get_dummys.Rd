% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_dummys}
\alias{get_dummys}
\title{create dummy dataframe for decks to be used for classification}
\usage{
get_dummys(deckcode, deckarchetype)
}
\arguments{
\item{websitecode}{a string of characters that identifies a deck in the game of hearthstone}
}
\value{
returns a dummy data frame with X columns based on unique copies of cards in the deck plus the mana cost and deck name
}
\description{
create dummy dataframe for decks to be used for classification
}
