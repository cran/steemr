% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.r
\name{bblog}
\alias{bblog}
\title{Build a hugo blog site for a steem author}
\usage{
bblog(author = NA, post_df, dest_path = "blog", initial = FALSE, 
    template = "xmin", post_df_source = c("appbase_api", "steemsql.com"), 
    orginal_link = c("steemit.com", "cnsteem.com", "busy.org", "steemdb.com", 
        "steemd.com"), my_github = "your_name/your_repo")
}
\arguments{
\item{author}{author name without @}

\item{post_df}{A dataframe with the posts contents retrieved from SteemSQL}

\item{dest_path}{A character string}

\item{initial}{if initialize a site}

\item{template}{the hugo template}

\item{post_df_source}{A character string of the data frame source.}

\item{orginal_link}{A character string}

\item{my_github}{A character string of a github repo}
}
\value{
a blogdown-hugo web site
}
\description{
Build a hugo blog site for a steem author
}
\examples{
\dontrun{
bblog()
}

}
