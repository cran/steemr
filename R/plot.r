#' Adapt the x axis to the time range
#'
#' @param x the time range
#'
#' @return the optimized label
xatf <- function(x) {
  if (x > 365 * 4) return(c('year', '%Y'))
  if (x > 365) return(c('quarter', '%Y-%m'))
  if (x > 100) return(c('month', '%Y-%m'))
  if (x > 30) return(c('week','%m-%d'))
  c('day','%m-%d')
}


#' Plot a time series with x as Date.
#'
#' @param x A Date vector.
#' @param y A numeric vector.
#' @param myylab A character string of the y label.
#' @param mylegend A character string of the legend.
#' @param mycol A color.
#' @param myxlim A Date vector of the date range of x.
#' @param myylim A Date vector of the date range of y.
#'
#' @return A time series diagram.
#' @export
#'
pdate <- function(x,
                  y,
                  myylab = '',
                  mylegend = '',
                  mycol = 'darkgreen',
                  myxlim = NULL,
                  myylim = NULL){
  # x range for plotting.
  xrange <- as.numeric(diff.Date(myxlim))

  # x at for x labels.
  xat <- xatf(xrange)

  oldpar <- par(mar = c(3, 4, 0.5, 1),
                las = 1)
  plot(x,
       y,
       xlab = '',
       ylab = myylab,
       axes = FALSE,
       type = 'l',
       col = mycol,
       xlim = myxlim,
       ylim = myylim)
  box()
  axis.Date(side = 1,
            at = seq.Date(myxlim[1], myxlim[2], by = xat[1]),
            x = seq.Date(myxlim[1], myxlim[2], by = xat[1]),
            format = xat[2])
  axis(2)
  rug(x, col = "darkgrey")
  legend('topleft', legend = mylegend, bty = 'n')
  par(oldpar)
}
