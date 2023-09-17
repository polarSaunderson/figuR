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
  xxLim   <- xVector$range
  if (isTRUE(expandLimits)) {
    xxLim <- range(pretty(xxLim))
  }

  yVector <- calc_intervals(y) |> suppressWarnings()
  yyLim   <- yVector$range
  if (isTRUE(expandLimits)) {
    yyLim <- range(pretty(yyLim))
  }

  return(list("xxLim" = xxLim,
              "yyLim" = yyLim,
              "xLabels" = xVector$labels,
              "yLabels" = yVector$labels))
}
