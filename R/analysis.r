#' Find which followers have not voted a post yet. avotenot means 'analysis who has not voted yet'.
#'
#' @param postlink A character string of the link to a target post.
#'
#' @return A character vector of the name list of the followers who have not voted the target post yet.
#' @export
#'
#' @examples
#' \dontrun{
#' avotenot('cn/@dapeng/steemit-markdown')
#' }
avotenot <- function(postlink = NA){
  if(is.na(postlink)) {
    return(message('Please give a valid id.'))
  }

  vr_votes <- gvotep(postlink)$Voter
  postlink <- paste0('https://steemdb.com/', postlink)
  vr_id <- substr(strsplit(postlink, '/')[[1]][5],
                  2,
                  nchar(strsplit(postlink, '/')[[1]][5]))
  vr_follower <- unique(gfollower(id = vr_id)$followers)
  vr_follower <- vr_follower[!(vr_follower %in% vr_votes)]
  if (is.null(vr_follower)) {
    return(NULL)
  } else {
    return(vr_follower[order(vr_follower)])
  }
}


#' Summary of the voters of a series of posts. avotep means 'analysis of the votes on the given posts retrieved with the appbase_api method.'
#'
#' @param mypost A data frame of a series of posts retrieved with the appbase_api method.
#' @param if_plot A logic value of whether plot the pie diagram.
#' @param top A numeric value of the Top voters plotted in the pie diagram.
#'
#' @return A data frame of the voters
#' @export
#'
#' @examples
#' \dontrun{
#' avotep()
#' }
avotep <- function(mypost = NA,
                   if_plot = TRUE,
                   top = 10){
  if(is.na(mypost)) {
    voter_sum <- NA
  } else {
    # pre calculation
    voter <- data.frame(
      voter = charsplit(dataframe = mypost$voter),
      rshares = as.numeric(charsplit(dataframe = mypost$voter.rshares)),
      percent = as.numeric(charsplit(dataframe = mypost$voter.percent)) / 100,
      row.names = NULL
    )

    # summry of voters
    voter_sum <- beginr::tapplydf(data = voter,
                                  select = 'rshares',
                                  myfactor = 'voter',
                                  sum)
    voter_percent <- beginr::tapplydf(data = voter,
                                      select = 'percent',
                                      myfactor = 'voter',
                                      mean)
    voter_sum <- merge(voter_sum,
                       voter_percent,
                       by = 'voter')
    voter_sum$percentage <- voter_sum$rshares / sum(voter_sum$rshares)
    voter_sum <- voter_sum[rev(order(voter_sum$rshares)), ]
    row.names(voter_sum) <- 1:nrow(voter_sum)

    # plot the pie diagram
    if (if_plot) {
      pie_df <- voter_sum[voter_sum$percentage >= 0,
                          c('voter', 'percentage')]
      if (nrow(pie_df) > top) {
        pie_df <- pie_df[1:top, ]
      }
      pie = pie(pie_df$percentage, labels = pie_df$voter)
    }
  }
  return(voter_sum)
}


#' Calculate the the daily frequency.
#' adailyf means 'analysis of the daily frequency'.
#'
#' @param mydata A data frame with a date column.
#' @param datecol A chracter string of the date column name. The date column could either be Date or Character (in '\%Y-\%m-\%d' format).
#' @param col A color for plotting
#' @param ylab A character string of the y label
#' @param if_plot A logic value of whether plot the time series
#'
#' @return A dataframe of the daily frequency
#' @export
adailyf <- function(mydata,
                    datecol,
                    if_plot = FALSE,
                    col = 'steelblue3',
                    ylab = 'Daily posts') {
  daily <- data.frame(table(mydata[, datecol]))
  names(daily)[1] <- 'date'
  daily$date <- as.Date(daily$date)
  if(if_plot) {
    pdate(x = daily$date, y = daily$Freq, myxlim = range(daily$date),
          myylim = c(0, max(daily$Freq, na.rm = TRUE) + 1),
          myylab = ylab,
          mycol = col)
  }
  return(daily)
}

#' Analysis of the CN sub categories.
#'
#' @param from A Date object or character in '2017-10-24' format of the beginning of the period
#' @param to A Date object or character in '2017-10-24' format of the end of the period
#' @param if_plot A logic value of whether plot the time series
#' @param sql_con A SQL connection
#' @param top A numeric value of the Top tags for plotting
#'
#' @return A figure showing the active cn sub tag
#' @export
#'
#'
acnsub <- function(from = Sys.Date() - 7,
                   to = Sys.Date(),
                   sql_con,
                   if_plot = FALSE,
                   top = 10){
  # query the data from the server
  print('Querying. Please wait...')
  sql_query <- paste0("SELECT created, json_metadata
                      FROM Comments
                      WHERE CONTAINS(json_metadata, 'cn-')
                      AND depth = 0
                      AND created BETWEEN '",
                      format(from, '%Y/%m/%d'),
                      "' and '",
                      format(to, '%Y/%m/%d'),
                      "' ORDER by created")
  posts <- RODBC::sqlQuery(channel = sql_con,
                           query = sql_query,
                           stringsAsFactors = FALSE)

  # extract the tags from the json metadata
  tags <- sapply(X = posts$json_metadata,
                 FUN = tag_of_post,
                 simplify = TRUE)
  tags <- unlist(unname(tags))
  tags <- tags[grep('cn-', tags)]
  tags2 <- table(tags)
  tags2 <- as.data.frame(tags2)
  tags3 <- tags2[order(-tags2$Freq), ]

  # plot the bar diagram
  if (if_plot) {
    oldpar <- par(mar = c(4, 10,1,1))
    barplot(tags3$Freq[top:1],
            horiz = TRUE,
            border = NA,
            space = 0,
            names.arg = tags3$tags[top:1],
            las = 1,
            col = rainbow(top),
            xlab = 'Posts')
    box()
    par(oldpar)
  }
  return(tags3)
}
