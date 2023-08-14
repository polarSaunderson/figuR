calc_plot_limits <- function(x, y) {
  #' Calculate axis limits automatically
  #'
  #' @description This function is used in `plot_lines` and `plot_points`,
  #'   if the x and y limits are are not provided.
  #'
  #' @param x The x values that will be plotted.
  #' @param y The y values that will be plotted.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Create xlim and ylim - these are the only things that pre_plot must have
  xxInt <- calc_intervals(x)
  xxLim <- range(xxInt)
  xxOff <- (xxInt[2] - xxInt[1]) / 2
  xxLim <- c(xxLim[1] - xxOff,
             xxLim[2] + xxOff)
  if (min(x) > 0 & xxLim[1] < 0) {
    xxLim[1] <- 0
    xxLim[2] <- max(x) + (min(x))
  } else if (max(x) < 0 & xxLim[2] > 0) {
    xxLim[2] <- 0
    xxLim[1] <- min(x) + (max(x))
  }

  yyInt <- calc_intervals(y)
  yyLim <- range(yyInt)
  yyOff <- (yyInt[2] - yyInt[1]) / 2
  yyLim <- c(yyLim[1] - yyOff,
             yyLim[2] + yyOff)
  if (min(y) > 0 & yyLim[1] < 0) {
    yyLim[1] <- 0
    yyLim[2] <- max(y) + (min(y))
  } else if (max(y) < 0 & yyLim[2] > 0) {
    yyLim[2] <- 0
    yyLim[1] <- min(y) + (max(y))
  }
  return(list("xxLim" = xxLim,
              "yyLim" = yyLim))
}
