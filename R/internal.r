#' Clear the format of the 'follow' information from steemdb.com.
#'
#' @param x A character string of the follower information from steemdb.com.
#'
#' @return A character string of the formatted follower information
#'
clearferfing <- function(x){
  # remove the spaces
  x1 <- gsub(' ', '', x)
  # remove the line breaker
  x2 <- gsub('\n', '', x1)
  # remove "
  y <- gsub('\\"','', x2)
  y <- substr(y, 2, nchar(y) - 1)
  strsplit(y, ',')[[1]]
}

#' Concert unit from GV, MV, kV to numeric values.
#'
#' @param x the unit GV, MV, kV to convert
#'
#' @return the numeric multiplier.
#'
unitconvert <- function(x){
  switch(x,
         'GV' = 10 ^ 9,
         'MV' = 10 ^ 6,
         'kV' = 10 ^ 3)
}

#' Convert an id from a character to html hyperlink
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A character string of the hyperlink to the Steem id.
idlink <- function(id = NA) {
  paste0('<a href="https://steemit.com/@', id, '">@', id, '</a>')
}


#' Split the characters in a data frame
#'
#' @param dataframe A column of a dataframe with characters to split
#'
#' @return A character vector
charsplit <- function(dataframe = NA){
  if(is.na(dataframe)) {
    char_single <- NA
  } else {
    char_all <- paste(dataframe, collapse = ' ')
    char_single <- strsplit(char_all, split = ' ')[[1]]
  }
  return(char_single)
}


#' Clear the characters in the titles
#'
#' @param title The post title
#'
#' @return A clear title
clear_title <- function(title){
  title_new <- gsub('"', '\\\\"', title)
  title_new <- gsub('\\|', '-', title_new)
  title_new <- gsub('\\?\\?', '', title_new)
  return(title_new)
}



#' Calculate the level of and ID
#'
#' @param x the Raw SP value of an ID
#'
#' @return the lavel of the ID
whale <- function(x){
  if (is.na(x)) return(NA)
  y <- c(0, 1e6, 1e7, 1e8, 1e9)  * 485.656 / 1000000
  if (x > y[5]) return(5)
  if (x > y[4]) return(4)
  if (x > y[3]) return(3)
  if (x > y[2]) return(2)
  if (x >= y[1]) return(1)
}

#' Calculate the skewness
#'
#' @param x The data for calculation.
#'
#' @return The skewness
skewness <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  skewness <- n / (n-1) / (n-2) * sum((x - mean(x)) ^ 3) / sd(x) ^3
  se_skewness <- sqrt(6/length(x))
  return(skewness/se_skewness)
}

#' Retrieve tags from json_str of a post
#'
#' @param post_json_str json string with tag information
#'
#' @return tags
tag_of_post <- function(post_json_str) {
  post_json_str <- stringi::stri_trans_general(post_json_str, "latin-ascii")
  if (sum(nchar(post_json_str) == 0)) return(NA)
  y <- rjson::fromJSON(json_str = post_json_str, unexpected.escape = 'keep')
  if ("tags" %in% names(y)) return(unname(y$tags))
  NA
}
