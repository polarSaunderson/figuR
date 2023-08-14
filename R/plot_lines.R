plot_lines <- function(x, y,
                       addPoints = TRUE,
                       lineKula = "#004488FF",
                       lwd = 4,
                       lty = 1,
                       borderKula = NULL,
                       pointKula  = "white",
                       cex = 1.5, pch = 21,
                       xpd = TRUE, ...) {
  #' Line plot of x and y values
  #'
  #' @description This is the figuR equivalent of `plot(x, y, type = "l")` to
  #'   create a simple line chart.
  #'
  #' @param x The x data to plot.
  #' @param y The y data to plot.
  #' @param addPoints LOGICAL: Should points be added on top of the lines (TRUE,
  #'   default), or not (FALSE)?
  #' @param lineKula What colour should the line be?
  #' @param lwd How thick should the line be?
  #' @param lty What type (solid, dotted, dashed, etc.) should the line be?
  #' @param borderKula What colour should the boundary of the marker be?
  #' @param pointKula What colour should the centre of the marker be?
  #' @param cex What size should the marker be?
  #' @param pch What style should the marker be?
  #' @param xpd Can the markers be plotted off the chart?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Create xxLim & yyLim for xLimits and yLimits (both mandatory for pre_plot)
  xyLimits <- calc_plot_limits(x, y)
  xxLimits <- xyLimits$xxLim
  yyLimits <- xyLimits$yyLim

  # Use created xxLim and yyLim if necessary
  defArgs <- list(yLimits = yyLimits, xLimits = xxLimits)
  dotArgs <- list(...)
  defArgs[names(dotArgs)] <- dotArgs
  do.call(figuR::pre_plot, defArgs)   # run the function

  # Add points
  lines(x, y, col = lineKula,
         lwd = lwd, xpd = xpd)
  if (isTRUE(addPoints)) {
    borderKula <- domR::set_if_null(borderKula, lineKula)
    points(x, y,
           col = borderKula, bg = pointKula,
           cex = cex, pch = pch,
           xpd = xpd)
  }
}
