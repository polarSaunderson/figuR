plot_lines <- function(x, y = NULL,
                       addPoints = TRUE,
                       lineKula = "#004488FF",
                       lwd = 4,
                       lty = 1,
                       expandLimits = TRUE,
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
  #' @param y The y data to plot. y must either be empty, the same length as x,
  #'   or a multiple of its length. If empty, 'x' is used as 'y', and 'x'
  #'   becomes `1:length(y)`. If length is a multiple of 'x' length , the 'x'
  #'   data are repeated, and the 'y' data are treated as separate data series;
  #'   'lineKula' is currently ignored in such a situation and each y series is
  #'   coloured differently (colours will repeat after 7 y "series").
  #' @param addPoints LOGICAL: Should points be added on top of the lines (TRUE,
  #'   default), or not (FALSE)?
  #' @param lineKula What colour should the line be?
  #' @param lwd How thick should the line be?
  #' @param lty What type (solid, dotted, dashed, etc.) should the line be?
  #' @param expandLimits LOGICAL: Should `pretty()` limits be used (TRUE), or
  #'   exact min & max values (FALSE)? Fed directly into `calc_plot_limits()`.
  #' @param borderKula What colour should the boundary of the marker be?
  #' @param pointKula What colour should the centre of the marker be?
  #' @param cex What size should the marker be?
  #' @param pch What style should the marker be?
  #' @param xpd Can the markers be plotted off the chart?
  #' @param ... Any arguments that can be fed into `pre_plot()`.
  #'
  #' @examples
  #' \dontrun{
  #'   # Data
  #'   x  <- c(1:6)
  #'   y1 <- c(1:6)
  #'   y2 <- c(7:12)
  #'   y3 <- c(8:3)
  #'
  #'   # Plots
  #'   par(mar = c(5, 5, 5, 5))
  #'   plot_lines(x = x, y = y1)
  #'   plot_lines(x = x, y = c(y1, y2, y3))
  #'   plot_lines(x = x, y = y2, xName = "newXname")
  #'
  #'   # We can also just provide labels if they're unique and correctly ordered
  #'   plot_lines(x = month.abb, y = c(y1, y3), xLabelEvery = 1)
  #'
  #'   # But beware here - the following won't plot what you'd hope for
  #'   plot_lines(x = c("Jan", "Feb", "Mar", "Jan", "Feb", "Mar"),
  #'              y = 1:6, xLabelEvery = 1)
  #' }
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle if only one dataset is provided
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }

  # Create xxLimits & yyLimits for xLimits and yLimits (both mandatory for pre_plot)
  xyLimits <- calc_plot_limits(x, y, expandLimits)
  xxLimits <- xyLimits$xxLim
  yyLimits <- xyLimits$yyLim

  # Get any additional arguments fed in for pre_plot
  dotArgs <- list(...)

  # If axis labels are provided in place of x or y, enumerate them
  if (isTRUE(xyLimits$xLabels)) {
    dotArgs$xLabels <- x         # add as labels for pre_plot
    x <- seq_along(x)            # but must plot against numbers
  }
  if (isTRUE(xyLimits$yLabels)) {
    dotArgs$yLabels <- y         # add as labels for pre_plot
    y <- seq_along(y)            # but must plot against numbers
  }

  # If yLabels and xLabels exist, they need special handling BEFORE pre_plot
  # because labels take precedence when determining the axis limits
  if ("yLabels" %in% names(dotArgs)) {
    yyLimits <- range(seq_along(dotArgs$yLabels))
  }
  if ("xLabels" %in% names(dotArgs)) {
    xxLimits <- range(seq_along(dotArgs$xLabels))
  }

  # Only use created xxLimits and yyLimits if necessary
  # They are considered the defaults, but will be overwritten if xLimits or
  # yLimits are provided in as ... arguments
  defArgs <- list(yLimits = yyLimits, xLimits = xxLimits) # defaults
  defArgs[names(dotArgs)] <- dotArgs                      # overwrite defaults

  # Plot the data, but account for multiple y vectors against a single x
  if (length(y) != length(x)) {
    # print("x and y length don't match!")
    if (length(y) %% length(x) == 0) {
      # print("but y is an exact multiple of x!")
      do.call(pre_plot, defArgs)

      # We'll loop through the multiples and plot each as a separate y series
      xRep <- length(y) / length(x)
      kk <- 1
      for (ii in 1:xRep) {
        iiData <- y[(ii * length(x) - (length(x) - 1)):(ii * length(x))]
        graphics::lines(x, iiData, col = kulaR::kulaQ(kk), lwd = 4)
        if (isTRUE(addPoints)) {
          borderKula <- set_if_null(borderKula, kulaR::kulaQ(kk))
          graphics::points(x, iiData,
                           col = borderKula, bg = pointKula,
                           cex = cex, pch = pch,
                           xpd = xpd)
        }
        # There are only so many colours in the world!
        kk <- kk + 1
        if (kk > 7) {kk <- 1}
      }
    } else {
      stop("x and y are different lengths! And y is not an exact multiple of x!")
    }
  } else {
    # Plot normally if x and y lengths match
    do.call(pre_plot, defArgs)

    # Add lines (and points if necessary)
    graphics::lines(x, y, col = lineKula,
          lwd = lwd, xpd = xpd)
    if (isTRUE(addPoints)) {
      borderKula <- set_if_null(borderKula, lineKula)
      graphics::points(x, y,
                       col = borderKula, bg = pointKula,
                       cex = cex, pch = pch,
                       xpd = xpd)
    }
  }
}
