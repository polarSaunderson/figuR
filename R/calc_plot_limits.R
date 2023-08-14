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
  xxInt <- calc_intervals(x) |> suppressWarnings()
  if (isTRUE(xxInt$vLabels)) {
    x <- seq_along(x)
  }

  xxLim <- xxInt$vRange
  # cat("\nxxLim:", xxLim, "\n")
  if (isFALSE(xxInt$vPretty)) {
    xxOff <- xxInt$vSeq
    xxLim <- c(xxLim[1] - xxOff,
               xxLim[2] + xxOff)
    if (min(x) > 0 & xxLim[1] < 0) {
      xxLim[1] <- 0
      xxLim[2] <- round(max(x) + (min(x)))
    } else if (max(x) < 0 & xxLim[2] > 0) {
      xxLim[2] <- 0
      xxLim[1] <- round(min(x) + (max(x)))
    }
  }

  yyInt <- calc_intervals(y) |> suppressWarnings()
  if (isTRUE(yyInt$vLabels)) {
    # print("yLabels")
    y <- seq_along(y)
  }
  yyLim <- yyInt$vRange
  # cat("\nyyLim:", yyLim, "\n")
  if (isFALSE(yyInt$vPretty)) {
    yyOff <- yyInt$vSeq
    # cat("\nyyOff:", yyOff, "\n")
    yyLim <- c(yyLim[1] - yyOff,
               yyLim[2] + yyOff)
    if (min(y) > 0 & yyLim[1] < 0) {
      yyLim[1] <- 0
      yyLim[2] <- round(max(y) + (min(y)))
    } else if (max(y) < 0 & yyLim[2] > 0) {
      yyLim[2] <- 0
      yyLim[1] <- round(min(y) + (max(y)))
    }
  }

  return(list("xxLim" = xxLim,
              "yyLim" = yyLim,
              "xLabels" = xxInt$vLabels,
              "yLabels" = yyInt$vLabels))
}
