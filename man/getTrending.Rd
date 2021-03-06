% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/steemrdata.R
\name{getTrending}
\alias{getTrending}
\title{Current Trending Posts}
\usage{
getTrending(tag = "", limit = 100, node)
}
\arguments{
\item{tag}{Tag to Filter}

\item{limit}{Number of Posts to Return}

\item{node}{Optional Argument (Steem Node to Query)

Get details of the Trending Posts}
}
\value{
List with Details of Posts
}
\description{
Current Trending Posts
}
\examples{
\dontrun{
getTrending()  # Get first 100 Trending Posts
getTrending(20)  # Get Top 10 Trending Posts
getTrending(20, node = "https://api.steemit.com")
}
}
