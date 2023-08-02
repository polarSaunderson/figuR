pre_plot <- function(xlim, ylim,

                     main = "",
                     mainOffset = 0.25,
                     mainCex = 1.25,

                     xTickSeq        = NULL,     # xSeq = NULL,
                     xTickCount      = NULL,
                     xTickFirst      = 1,
                     xTickEvery      = 1,
                     xAlignMidPoints = FALSE,
                     xTickLength     = 0.2,
                     xTickKula       = "#1A1A1AFF",

                     xAxisSide       = 1,
                     xLabels         = NULL,
                     xLabelEvery     = 1,
                     xLabelFirst     = 1,
                     xLabelOffset    = NULL,
                     xLabelCex       = 0.92,
                     xLabelSrt       = NULL,
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

                     xGridEvery      = 1,
                     xGridFirst      = 1,
                     xGridLwd        = 1,
                     xGridType       = 1,
                     xGridKula       = "#E6E6E6AA",


                     yTickSeq        = NULL,     # YSeq = NULL,
                     yTickCount      = NULL,
                     yTickFirst      = 1,
                     yTickEvery      = 1,
                     yAlignMidPoints = FALSE,
                     yTickLength     = 0.2,
                     yTickKula       = "#1A1A1AFF",

                     yAxisSide       = 2,
                     yLabels         = NULL,
                     yLabelEvery     = 1,
                     yLabelFirst     = 1,
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

                     yGridEvery      = 1,
                     yGridFirst      = 1,
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

                     mar = c(2.75, 2.75, 1.5, 1.25)) {
  # Set margins
  graphics::par(mar = mar)

  # Blank plot area
  graphics::plot(xlim, ylim,
                 type = "n",
                 xlim = xlim, ylim = ylim,
                 xaxs = "i", yaxs = "i",
                 axes = FALSE,
                 main = NA,
                 ylab = "", xlab = "")

  graphics::mtext(main, side = 3, line = mainOffset, cex = mainCex)

  # x-axis ---------------------------------------------------------------------
  # Labels
  if (is.null(xLabels)) {
    if (is.null(xTickSeq)) xTickSeq <- 1                       # default
    if (xlim[1] - xlim[2] < 0) xTickSeq <-  sqrt(xTickSeq^2)   # ascending
    if (xlim[1] - xlim[2] > 0) xTickSeq <- -sqrt(xTickSeq^2)   # descending
    xLabels <- seq(xlim[1], xlim[2], xTickSeq)             # new labels
    if (sum(xlim %notIn% xLabels) != 0) {
      stop("Choose a nicer interval! ",
          xTickSeq, " doesn't go into ", xlim[1], ":", xlim[2])
    }
  }

  # Add to the plot
  xTickLocations <- add_axis(axis           = xAxisSide,
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
  if (is.null(yLabels)) {
    if (is.null(yTickSeq)) yTickSeq <- 1                   # default
    if (ylim[1] - ylim[2] < 0) yTickSeq <-  sqrt(yTickSeq^2)   # ascending
    if (ylim[1] - ylim[2] > 0) yTickSeq <- -sqrt(yTickSeq^2)   # descending
    yLabels <- seq(ylim[1], ylim[2], yTickSeq)             # new labels
    if (sum(ylim %notIn% yLabels) != 0) {
      stop("Choose a nicer interval! ",
          yTickSeq, " doesn't go into ", ylim[1], ":", ylim[2])
    }
  }

  # Add to the plot
  yTickLocations <- add_axis(axis           = yAxisSide,
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
