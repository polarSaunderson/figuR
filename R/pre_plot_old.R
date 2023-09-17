pre_plot_old <- function(xLimits, yLimits,

                     main = "",
                     mainOffset = 0.25,
                     mainCex = 1.1,

                     xTickSeq        = NULL,     # xSeq = NULL,
                     xTickCount      = NULL,
                     xTickFirst      = NULL,
                     xTickEvery      = NULL,
                     xAlignMidPoints = FALSE,
                     xTickLength     = 0.2,
                     xTickKula       = "#1A1A1AFF",

                     xAxisSide       = 1,
                     xLabels         = NULL,
                     xLabelEvery     = NULL,
                     xLabelFirst     = NULL,
                     xLabelOffset    = NULL,
                     xLabelCex       = 0.92,
                     xLabelSrt       = 0,
                     xLabelKula      = "#4D4D4DFF",

                     xAxisLwd        = 1,
                     xAxisType       = 1,
                     xAxisKula       = "#1A1A1AFF",

                     xName           = NULL,
                     xNameAxis       = NULL,
                     xNameKula       = "#1A1A1AFF",
                     xNameCex        = 1,
                     xNameOffset     = NULL,
                     xNameSrt        = NULL,

                     xGridEvery      = NULL,
                     xGridFirst      = NULL,
                     xGridLwd        = 1,
                     xGridType       = 1,
                     xGridKula       = "#E6E6E6AA",


                     yTickSeq        = NULL,     # YSeq = NULL,
                     yTickCount      = NULL,
                     yTickFirst      = NULL,
                     yTickEvery      = NULL,
                     yAlignMidPoints = FALSE,
                     yTickLength     = 0.2,
                     yTickKula       = "#1A1A1AFF",

                     yAxisSide       = 2,
                     yLabels         = NULL,
                     yLabelEvery     = NULL,
                     yLabelFirst     = NULL,
                     yLabelOffset    = NULL,
                     yLabelCex       = 0.92,
                     yLabelSrt       = NULL,
                     yLabelKula      = "#4D4D4DFF",

                     yAxisLwd        = 1,
                     yAxisType       = 1,
                     yAxisKula       = "#1A1A1AFF",

                     yName           = NULL,
                     yNameAxis       = NULL,
                     yNameKula       = "#1A1A1AFF",
                     yNameCex        = 1,
                     yNameOffset     = NULL,
                     yNameSrt        = NULL,

                     yGridEvery      = NULL,
                     yGridFirst      = NULL,
                     yGridLwd        = 1,
                     yGridType       = 1,
                     yGridKula       = "#E6E6E6AA",


                     addOrigin       = TRUE,
                     originLwd       = 1,
                     originType      = 1,
                     originKula      = "#B3B3B388",

                     add121          = FALSE,

                     annotationLocation = c(0, 0),
                     annotationText     = "",
                     annotationCex      = 0.9,

                     mar = c(2.5, 3.5, 2.5, 1.5)) {

  #' Fine-combed control for plots
  #'
  #' @description Easily customisable plot areas
  #' @inheritParams add_axis_old
  #' @export

  # Code -----------------------------------------------------------------------
  # Set margins
  graphics::par(mar = mar)

  # Blank plot area
  graphics::plot(xLimits, yLimits,
                 type = "n",
                 xlim = xLimits, ylim = yLimits,
                 xaxs = "i", yaxs = "i",
                 axes = FALSE,
                 main = NA,
                 ylab = "", xlab = "")

  graphics::mtext(main, side = 3, line = mainOffset, cex = mainCex)

  # x-axis ---------------------------------------------------------------------
  # Labels
  if (is.null(xLabels)) {
    if (is.null(xTickSeq)) {
      xIntervals <- calc_intervals(xLimits[1], xLimits[2], intMax = 256,
                                   preferError = TRUE) |> suppressWarnings()
      xLabels  <- xIntervals$vVector
      xTickSeq <- xIntervals$vSeq

      # xIntervals <- 1
      # xLabels    <- seq(xLimits[1], xLimits[2], 1)
      # xTickSeq   <- 1
    }
  }

  xLabelEvery <- domR::set_if_null(xLabelEvery, ceiling(length(xLabels) / 8))
  xLabelFirst <- domR::set_if_null(xLabelFirst, xLabelEvery)

  # Add to the plot
  xTickLocations <- add_axis_old(axis           = xAxisSide,
                             tickSeq        = xTickSeq,
                             tickCount      = xTickCount,
                             tickEvery      = xTickEvery,
                             tickFirst      = xTickFirst,
                             alignMidPoints = xAlignMidPoints,
                             tickLength     = xTickLength,
                             tickKula       = xTickKula,
                             labels         = xLabels,
                             labelEvery     = xLabelEvery,
                             labelFirst     = xLabelFirst,
                             labelOffset    = xLabelOffset,
                             labelCex       = xLabelCex,
                             labelSrt       = xLabelSrt,
                             labelKula      = xLabelKula,
                             axisLwd        = xAxisLwd,
                             axisType       = xAxisType,
                             axisKula       = xAxisKula,
                             name           = xName,
                             nameAxis       = xNameAxis,
                             nameKula       = xNameKula,
                             nameCex        = xNameCex,
                             nameOffset     = xNameOffset,
                             nameSrt        = xNameSrt,
                             gridEvery      = xGridEvery,
                             gridFirst      = xGridFirst,
                             gridLwd        = xGridLwd,
                             gridType       = xGridType,
                             gridKula       = xGridKula)

  # y-axis ---------------------------------------------------------------------
  # Labels
  # cat("\n pre_plot line_157")
  # print(yLabels)
  # print_line(".")
  if (is.null(yLabels)) {
    if (is.null(yTickSeq)) {
      yIntervals <- calc_intervals(yLimits[1], yLimits[2], intMax = 256,
                                   preferError = TRUE) |> suppressWarnings()
      yLabels  <- yIntervals$vVector
      yTickSeq <- yIntervals$vSeq
    }
  }
  # cat("\n pre_plot line_168")
  # print(yLabels)
  # print_line(":")

  yLabelEvery <- domR::set_if_null(yLabelEvery, ceiling(length(yLabels) / 8))
  yLabelFirst <- domR::set_if_null(yLabelFirst, yLabelEvery)

  # Add to the plot
  yTickLocations <- add_axis_old(axis           = yAxisSide,
                             tickSeq        = yTickSeq,
                             tickCount      = yTickCount,
                             tickEvery      = yTickEvery,
                             tickFirst      = yTickFirst,
                             alignMidPoints = yAlignMidPoints,
                             tickLength     = yTickLength,
                             tickKula       = yTickKula,
                             labels         = yLabels,
                             labelEvery     = yLabelEvery,
                             labelFirst     = yLabelFirst,
                             labelOffset    = yLabelOffset,
                             labelCex       = yLabelCex,
                             labelSrt       = yLabelSrt,
                             labelKula      = yLabelKula,
                             axisLwd        = yAxisLwd,
                             axisType       = yAxisType,
                             axisKula       = yAxisKula,
                             name           = yName,
                             nameAxis       = yNameAxis,
                             nameKula       = yNameKula,
                             nameCex        = yNameCex,
                             nameOffset     = yNameOffset,
                             nameSrt        = yNameSrt,
                             gridEvery      = yGridEvery,
                             gridFirst      = yGridFirst,
                             gridLwd        = yGridLwd,
                             gridType       = yGridType,
                             gridKula       = yGridKula)

  # Additional Decoration ------------------------------------------------------
  # Add origin
  if (!isFALSE(addOrigin)) {
    if (isTRUE(addOrigin)) {
      addOrigin <- c(0, 0)
    }
    graphics::abline(h = addOrigin[1], v = addOrigin[2],
                     col = originKula, lwd = originLwd, lty = originType)
  }

  # Add line with a gradient of 1 (i.e. x:y = 1:1 (1 to 1) (1 2 1))
  if (isTRUE(add121)) {
    graphics::abline(c(0, 1), lty = 2, col = "#EE6677FF")
  }

  # Add a subplot number
  graphics::text(x = annotationLocation[1],
                 y = annotationLocation[2],
                 annotationText,
                 cex = annotationCex)

  # Add plot frame
  add_plot_frame()

  return(invisible(list("xTicks" = xTickLocations,
                        "yTicks" = yTickLocations)))
}
