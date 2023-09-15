#' @inheritParams add_axis


pre_plot_2 <- function(xLimits, yLimits,

                       main = "",
                       mainOffset = 0.25,
                       mainCex = 1.1,

                       xLabels = NULL,
                       yLabels = NULL,

                       xInterval = NULL,
                       yInterval = NULL,

                       addOrigin  = TRUE,
                       originLwd  = 1,
                       originType = 1,
                       originKula = "#E6E6E6AA",

                       mar = c(2.5, 3.5, 2.5, 1.5)) {

  # Code -----------------------------------------------------------------------
  # Basic Plot -----------------------------------------------------------------
  # Set margins
  graphics::par(mar = mar)

  # Blank plot area
  graphics::plot(xLimits, yLimits,                   # defines the bounds
                 xlim = xLimits, ylim = yLimits,
                 type = "n",
                 xaxs = "i", yaxs = "i",             # CRITICAL for add_axis
                 axes = FALSE,
                 main = NA, xlab = "", ylab = "")

  # Add title
  graphics::mtext(text = main,
                  side = 3,
                  line = mainOffset,
                  cex  = mainCex)

  # x-axis ---------------------------------------------------------------------
  # Labels
  # Calculate defaults if nothing is entered
  if (is.null(xLabels)) {
    if (is.null(xInterval)) {
      xAutomated <- calc_intervals(xLimits[1], xLimits[2], intMax = 256,
                                   preferError = TRUE) |> suppressWarnings()
      xLabels   <- xAutomated$vector
      xInterval <- xAutomated$interval
    }
  }

  # Where do the labels go?
  xLabelEvery <- domR::set_if_null(xLabelEvery, ceiling(length(xLabels) / 8))
  xLabelFirst <- domR::set_if_null(xLabelFirst, xLabelEvery)

  # The rest is just done via add_axis
  xInfo <- add_axis()

  # y-axis ---------------------------------------------------------------------
  # Labels
  # Calculate defaults if nothing is entered
  if (is.null(yLabels)) {
    if (is.null(yInterval)) {
      yAutomated <- calc_intervals(yLimits[1], yLimits[2], intMax = 256,
                                   preferError = TRUE) |> suppressWarnings()
      yLabels   <- yAutomated$vector
      yInterval <- yAutomated$interval
    }
  }

  # Where do the labels go?
  yLabelEvery <- domR::set_if_null(yLabelEvery, ceiling(length(yLabels) / 8))
  yLabelFirst <- domR::set_if_null(yLabelFirst, yLabelEvery)

  # The rest is just done via add_axis
  yInfo <- add_axis()

  # Addition Decoration --------------------------------------------------------
  # Origin?
  # Can be FALSE (no origin), TRUE (automated to 0,0) or set (e.g. c(x, y))
  if (!isFALSE(addOrigin)) {
    if (isTRUE(addOrigin)) {
      addOrigin <- c(0, 0)
    }
    graphics::abline(h = addOrigin[1], v = addOrigin[2],
                     col = originKula, lwd = originLwd, lty = originType)
  }

  # Gradient line?
  if (isTRUE(add121)) { # i.e. x:y = 1:1 line
    graphics::abline(c(0, 1), lty = 2, col = "#EE6677FF")
  }

  # Subplot Number
  graphics::text(x = annotationLocation[1],
                 y = annotationLocation[2],
                 annotationText,
                 cex = annotationCex)

  # Frame
  add_plot_frame()

  return(invisible(list("xTicks" = xInfo,
                        "yTicks" = yInfo)))
}
