plot_points <- function(x, y,
                        borderKula = "#004488FF",
                        pointKula  = "white",
                        cex = 1.25, pch = NULL,
                        lwd = 3, xpd = TRUE, ...) {
  #' Scatterplot of x and y values
  #'
  #' @description This is the figuR equivalent of `plot(x, y)` to create a
  #'   simple scatterplot.
  #'
  #' @param x The x data to plot.
  #' @param y The y data to plot. y must either be the same length as x, or a
  #'   multiple of its length; if the latter, the x data is repeated, and the y
  #'   treated as separate data series; 'borderKula' is ignored in such a
  #'   situation and each y series is coloured differently (colours will repeat
  #'   after 7 y "series").
  #' @param borderKula What colour should the boundary of the marker be?
  #' @param pointKula What colour should the centre of the marker be?
  #' @param cex What size should the marker be?
  #' @param pch What style should the marker be?
  #' @param lwd How thick should the marker boundary be?
  #' @param xpd Can the markers be plotted off the chart?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Create xxLim & yyLim for xLimits and yLimits (both mandatory for pre_plot)
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

  # Account for multiple y vectors against a single x
  if (length(y) != length(x)) {
    # print("no match!")
    if (length(y) %% length(x) == 0) {
      # print("goes into it!!")
      xRep <- length(y) / length(x)
    } else {
      stop("x and y are different lengths!")
    }
  }

  # run the function
  do.call(pre_plot, dotArgs)

  # Add points
  if (exists("xRep", inherits = FALSE)) {  # only search inside the function
    pch <- domR::set_if_null(pch, 16)
    kk <- 1
    for (ii in 1:xRep) {
      iiData <- y[(ii * length(x) - (length(x) - 1)):(ii * length(x))]
      graphics::points(x, iiData,
             col = kulaR::kulaQ(kk), bg = pointKula,
             cex = cex, pch = pch,
             lwd = lwd, xpd = xpd)
      kk <- kk + 1
      if (kk > 7) {kk <- 1}
    }
  } else {
    pch <- domR::set_if_null(pch, 21)
    graphics::points(x, y,
           col = borderKula, bg = pointKula,
           cex = cex, pch = pch,
           lwd = lwd, xpd = xpd)
  }
}




# xx <- sample(1:100, 25)
# yy <- sample(-100:99, 25)
# #
# plot_points <- function(x, y,
#                         borderKula = "red",
#                         pointKula  = "white",
#                         cex = 1, pch = 21,
#                         lwd = 1, ...) {
#   xInfo <- kulaR::get_kulaInfo(xx)
#   xlim <- c(xInfo$zRange[1] - xInfo$zIncrements,
#             xInfo$zRange[2] + xInfo$zIncrements)
#
#   yInfo <- kulaR::get_kulaInfo(yy)
#   ylim <- c(yInfo$zRange[1] - yInfo$zIncrements,
#             yInfo$zRange[2] + yInfo$zIncrements)
#
#   pre_plot(xlim, ylim,
#            xTickSeq = xInfo$zIncrements,
#            xTickFirst = 2, xTickEvery = 2,
#            xGridFirst = 2, xGridEvery = 2,
#            xLabelFirst = 2, xLabelEvery = 2,
#            yTickSeq = yInfo$zIncrements,
#            yTickFirst = 2, yTickEvery = 2,
#            yGridFirst = 2,  yGridEvery = 2,
#            yLabelFirst = 2, yLabelEvery = 2, ...)
#
#   points(xx, yy,
#          col = borderKula, bg = pointKula,
#          cex = cex, pch = pch,
#          lwd = lwd, xpd = TRUE)
# }
#
# plot_points(xx, yy, addOrigin = FALSE)
# plot(xx, yy)
