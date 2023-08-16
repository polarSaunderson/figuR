calc_plot_limits <- function(x, y, expandLimits = TRUE) {
  #' Calculate axis limits automatically
  #'
  #' @description This function is used in `plot_lines` and `plot_points`, if
  #'   the x and y limits are are not provided.
  #'
  #' @param x The x values that will be plotted.
  #' @param y The y values that will be plotted.
  #' @param expandLimits Should the axes fit exactly to the data (FALSE), or
  #'   should they be expanded slightly (TRUE)? The former will produce a point
  #'   on each axis, the latter will use `pretty()` to widen the axes.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xVector <- calc_intervals(x) |> suppressWarnings()
  xxLim   <- xVector$vRange
  if (isTRUE(expandLimits)) {
    xxLim <- range(pretty(xxLim))
    # xxPrec <- domR::count_decimal_places(xxLim) |> max()
    # if (xxPrec != 0) {
    #   xxPrec <- 10^(xxPrec - 1)
    #   xxLim <- c((floor(min(xVector$vRange) * xxPrec )) / xxPrec,
    #             (ceiling(max(xVector$vRange) * xxPrec)) / xxPrec)
    # } else {
    #   xxLim <- c(floor(min(xVector$vRange)),
    #            ceiling(max(xVector$vRange)))
    # }
  }

  yVector <- calc_intervals(y) |> suppressWarnings()
  yyLim   <- yVector$vRange
  if (isTRUE(expandLimits)) {
    yyLim <- range(pretty(yyLim))
    # yyPrec <- domR::count_decimal_places(yyLim) |> max()
    # if (yyPrec != 0) {
    #   yyPrec <- 10^(yyPrec - 1)
    #   yyLim <- c((floor(min(yVector$vRange) * yyPrec )) / yyPrec,
    #             (ceiling(max(yVector$vRange) * yyPrec)) / yyPrec)
    # } else {
    #   yyLim <- c(floor(min(yVector$vRange)),
    #            ceiling(max(yVector$vRange)))
    # }
  }

  return(list("xxLim" = xxLim,
              "yyLim" = yyLim,
              "xLabels" = xVector$vLabels,
              "yLabels" = yVector$vLabels))
}
