plot_matrix <- function(x, kulas = NULL, range = NULL, breaks = NULL,
                        ...,
                        xMarks = NULL, yMarks = NULL,
                        markPch = 3, markCex = 0.5,
                        xMarkOffset = NULL, yMarkOffset = NULL,
                        xyGuides = NULL, xyKulas = "black", xyLwd = 1) {
  #' Plot a matrix as an image
  #'
  #' @description Plots matrices (using [graphics::image()], but on top of a
  #'   customisable plotting frame from [pre_plot()].
  #'
  #' @param x matrix: The data to plot.
  #' @param kulas The colours for the matrix image.
  #' @param range c(vector) The min and max values to plot.
  #' @param xMarks,yMarks If both of these are not NULL (the default), guide
  #'   "marks" are added over the matrix image. The values should be vectors
  #'   that respectively refer to the x and y coordinates of the guide marks, in
  #'   relation to the number of underlying scaffolding meshlines. A mark is
  #'   added to each x coordinate for every y coordinate entered.
  #' @param markPch What symbol should the marks be?
  #' @param markCex How large should the mark symbol be?
  #' @param xyGuides If 'xMarks' and 'yMarks' are not NULL, this argument can be
  #'   used to add guidelines across the plot. Set as "xy" for both, "x" just
  #'   for vertical lines rising from the x-axis, and "y" for horizontal lines
  #'   stemming from the y-axis.
  #' @param xyKulas What colour should the 'xyGuides' be? If a single value is
  #'   provided, it is used for both the x and y; if a longer vector, the first
  #'   colour is used for the xGuide, and the second for the yGuide.
  #' @param xyLwd How thick should the 'xyGuides' be? If a single value is
  #'   provided, it is used for both the x and y; if a longer vector, the first
  #'   value is used for the xGuide, and the second for the yGuide.
  #' @param xMarkOffset How much should xMarks and xGuides be offset? Unit is
  #'   fractions of the meshlines separation. Positive is right.
  #' @param yMarkOffset How much should yMarks and yGuides be offset? Unit is
  #'   fractions of the meshlines separation. Positive is up.
  #' @param ... Any arguments (except 'xLimits' and 'yLimits') that can be added
  #'   to [pre_plot()].
  #' @param breaks Fed directly into [graphics::image()] if not NULL.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Create the plot frame
  tickLocations <- pre_plot_matrix(x, ...)

  # If no kula is set, default to batlow from `khroma`
  kulas <- set_if_null(kulas, c("#001959FF", "#0D355EFF", "#1C5360FF",
                                "#3E6C54FF", "#687A3DFF", "#99872DFF",
                                "#D49347FF", "#F8A380FF", "#FDB6BCFF",
                                "#F9CCF9FF"))
  # Add the matrix image
  if (is.null(breaks)) {
    range <- set_if_null(range, range(x))
    graphics::image(x,
                    add = TRUE,
                    col = kulas,
                    zlim = range,
                    axes = FALSE)
  } else {
    graphics::image(x,
                    add = TRUE,
                    col = kulas,
                    breaks = breaks,
                    axes = FALSE)
  }

  # Decorate with marks and guidelines?
  if (!is.null(xMarks) & !is.null(yMarks)) {
    xx <- tickLocations$xTicks[[1]][xMarks] |> rep(each = length(yMarks))
    yy <- tickLocations$yTicks[[1]][yMarks] |> rep(times = length(xMarks))

    # offsetting?
    if (!is.null(xMarkOffset)) {
      xInt <- tickLocations$xTicks[[1]][2] - tickLocations$xTicks[[1]][1]
      xInt <- xMarkOffset * xInt
      xx <- xx + xInt
    }

    if (!is.null(yMarkOffset)) {
      yInt <- tickLocations$yTicks[[1]][2] - tickLocations$yTicks[[1]][1]
      yInt <- yMarkOffset * yInt
      yy <- yy + yInt
    }


    if (!is.null(xyGuides)) {
      if (length(xyKulas) == 1) xyKulas <- rep(xyKulas, 2)
      if (length(xyLwd) == 1) xyLwd <- rep(xyLwd, 2)
      xyGuides <- strsplit(xyGuides, "") |> unlist()
      if ("x" %in% xyGuides) graphics::abline(v = xx, col = xyKulas[1], lwd = xyLwd[1])
      if ("y" %in% xyGuides) graphics::abline(h = yy, col = xyKulas[2], lwd = xyLwd[2])
    }
    graphics::points(xx, yy, pch = markPch, cex = markCex, xpd = TRUE)
  }

  # Return
  return(invisible(tickLocations))
}
