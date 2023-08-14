plot_lines <- function(x, y,
                       addPoints = TRUE,
                       lineKula = "#004488FF",
                       lwd = 4,
                       lty = 1,
                       borderKula = NULL,
                       pointKula  = "white",
                       cex = 1.25, pch = 21,
                       xpd = TRUE, ...) {
  #' Line plot of x and y values
  #'
  #' @description This is the figuR equivalent of `plot(x, y, type = "l")` to
  #'   create a simple line chart.
  #'
  #' @param x The x data to plot.
  #' @param y The y data to plot. y must either be the same length as x, or a
  #'   multiple of its length; if the latter, the x data is repeated, and the y
  #'   treated as separate data series; 'lineKula' is ignored in such a
  #'   situation and each y series is coloured differently (colours will repeat
  #'   after 7 y "series").
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
  # Create xxLimits & yyLimits for xLimits and yLimits (both mandatory for pre_plot)
  xyLimits <- calc_plot_limits(x, y)
  xxLimits <- xyLimits$xxLim
  yyLimits <- xyLimits$yyLim

  # If provided, yLabels and xLabels need special handling BEFORE pre_plot
  dotArgs <- list(...)
  if ("yLabels" %in% names(dotArgs)) {
    yyLimits <- range(seq_along(dotArgs$yLabels))
  }
  if ("xLabels" %in% names(dotArgs)) {
    xxLimits <- range(seq_along(dotArgs$xLabels))
  }

  # Use created xxLimits and yyLimits if necessary
  defArgs <- list(yLimits = yyLimits, xLimits = xxLimits) # defaults
  dotArgs[names(defArgs)] <- defArgs

  # Plot the data, but account for multiple y vectors against a single x
  if (length(y) != length(x)) {
    # print("no match!")
    if (length(y) %% length(x) == 0) {
      # print("goes into it!!")
      do.call(figuR::pre_plot, dotArgs)
      xRep <- length(y) / length(x)
      kk <- 1
      for (ii in 1:xRep) {
        iiData <- y[(ii * length(x) - (length(x) - 1)):(ii * length(x))]
        lines(x, iiData, col = kulaQ(kk), lwd = 4)
        if (isTRUE(addPoints)) {
          borderKula <- domR::set_if_null(borderKula, kulaQ(kk))
          points(x, iiData,
                 col = borderKula, bg = pointKula,
                 cex = cex, pch = pch,
                 xpd = xpd)
        }
        # There are only so many colours in the world!
        kk <- kk + 1
        if (kk > 7) {kk <- 1}
      }
    } else {
      stop("x and y are different lengths!")
    }
  } else {
    # Plot normally if x and y lengths match
    do.call(figuR::pre_plot, dotArgs)

    # Add lines (and points?)
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
}


# xx <- xData
# yy <- c(racmoData$senf[, 6],
#     racmoData$latf[, 6],
#     racmoData$senf[, 6] + racmoData$latf[, 6])
#
# alternative with easier data
#
# xx <- 1:10
# yy <- c(1:10, 11:20, 21:30)  # or yy <- c(1:30)
# plot_lines(xx, yy) # this fails
#
# Split the data if it matches length wise
# if (length(yy) %% length(xx) == 0) {
#   xRep <- length(yy) / length(xx)
# }
#
# plot_points(rep(xx, xRep), yy, cex = 0) # just for the plot background really
# for (ii in 1:xRep) {
#   iiData <- yy[(ii * length(xx) - (length(xx) - 1)):(ii * length(xx))]
#   lines(xx, iiData, col = kulaQ3(ii), lwd = 4)
# }
