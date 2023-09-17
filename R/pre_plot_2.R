pre_plot_2 <- function(xLimits, yLimits,

                       main       = "",
                       mainOffset = 0.25,
                       mainCex    = 1.1,

                       xLabels = NULL,
                       yLabels = NULL,

                       xInterval = NULL,
                       yInterval = NULL,

                       xTickCount  = NULL,
                       yTickCount  = NULL,

                       alignMidPoints  = FALSE,
                       xAlignMidPoints = NULL,
                       yAlignMidPoints = NULL,

               gridEvery  = NULL,
               xGridEvery = NULL,
               yGridEvery = NULL,

               gridFirst  = NULL,
               xGridFirst = NULL,
               yGridFirst = NULL,

               gridLwd    = 1,
               xGridLwd   = NULL,
               yGridLwd   = NULL,

               gridKula    = "#E6E6E6AA",
               xGridKula  = NULL,
               yGridKula  = NULL,

               gridType   = 1,
               xGridType  = NULL,
               yGridType  = NULL,

                       tickFirst   = NULL,
                       xTickFirst  = NULL,
                       yTickFirst  = NULL,

                       tickEvery   = NULL,
                       xTickEvery  = NULL,
                       yTickEvery  = NULL,

                       tickLength  = 0.2,
                       xTickLength = NULL,
                       yTickLength = NULL,

                       tickKula    = "#1A1A1AFF",
                       xTickKula   = NULL,
                       yTickKula   = NULL,

             xAxisSide = 1,
             yAxisSide = 2,

             axisLwd   = 1,
             xAxisLwd  = NULL,
             yAxisLwd  = NULL,

             axisType  = 1,
             xAxisType = NULL,
             yAxisType = NULL,

             axisKula  = "#1A1A1AFF",
             xAxisKula = NULL,
             yAxisKula = NULL,

                       labelEvery   = NULL,
                       xLabelEvery  = NULL,
                       yLabelEvery  = NULL,

                       labelFirst   = NULL,
                       xLabelFirst  = NULL,
                       yLabelFirst  = NULL,

                       labelOffset  = NULL,
                       xLabelOffset = NULL,
                       yLabelOffset = NULL,

                       labelCex     = 0.92,
                       xLabelCex    = NULL,
                       yLabelCex    = NULL,

                       labelKula    = "#4D4D4DFF",
                       xLabelKula   = NULL,
                       yLabelKula   = NULL,

                       labelSrt  = NULL,
                       xLabelSrt = NULL,
                       yLabelSrt = NULL,

               xName = NULL,
               yName = NULL,

               xNameSide   = NULL,
               yNameSide   = NULL,

               nameCex     = 1,
               xNameCex    = NULL,
               yNameCex    = NULL,

               nameKula   = "#1A1A1AFF",
               xNameKula  = NULL,
               yNameKula  = NULL,

               nameOffset  = NULL,
               xNameOffset = NULL,
               yNameOffset = NULL,

               nameSrt     = NULL,
               xNameSrt    = NULL,
               yNameSrt    = NULL,

                       addOrigin  = TRUE,
                       originLwd  = 1,
                       originType = 1,
                       originKula = "#B3B3B388",

                       add121          = FALSE,

                       annotationLocation = c(0, 0),
                       annotationText     = NULL,
                       annotationCex      = 0.9,

                       mar = c(2.5, 3.5, 2.5, 1.5)) {
  # Code -----------------------------------------------------------------------
  # Handle defaults & matching x / y axes setting
  xAlignMidPoints <- domR::set_if_null(xAlignMidPoints, alignMidPoints)
  yAlignMidPoints <- domR::set_if_null(yAlignMidPoints, alignMidPoints)

  ## Gridlines ----
  xGridEvery <- domR::set_if_null(xGridEvery, gridEvery)
  yGridEvery <- domR::set_if_null(yGridEvery, gridEvery)

  xGridFirst <- domR::set_if_null(xGridFirst, gridFirst)
  yGridFirst <- domR::set_if_null(yGridFirst, gridFirst)

  xGridLwd <- domR::set_if_null(xGridLwd, gridLwd)
  yGridLwd <- domR::set_if_null(yGridLwd, gridLwd)

  xGridType <- domR::set_if_null(xGridType, gridType)
  yGridType <- domR::set_if_null(yGridType, gridType)

  xGridKula <- domR::set_if_null(xGridKula, gridKula)
  yGridKula <- domR::set_if_null(yGridKula, gridKula)

  ## Tickmarks ----
  xTickFirst <- domR::set_if_null(xTickFirst, tickFirst)
  yTickFirst <- domR::set_if_null(yTickFirst, tickFirst)

  xTickEvery <- domR::set_if_null(xTickEvery, tickEvery)
  yTickEvery <- domR::set_if_null(yTickEvery, tickEvery)

  xTickLength <- domR::set_if_null(xTickLength, tickLength)
  yTickLength <- domR::set_if_null(yTickLength, tickLength)

  xTickKula <- domR::set_if_null(xTickKula, tickKula)
  yTickKula <- domR::set_if_null(yTickKula, tickKula)

  ## Axes ----
  xAxisLwd <- domR::set_if_null(xAxisLwd, axisLwd)
  yAxisLwd <- domR::set_if_null(yAxisLwd, axisLwd)

  xAxisType <- domR::set_if_null(xAxisType, axisType)
  yAxisType <- domR::set_if_null(yAxisType, axisType)

  xAxisKula <- domR::set_if_null(xAxisKula, axisKula)
  yAxisKula <- domR::set_if_null(yAxisKula, axisKula)

  ## Labels ----
  xLabelEvery <- domR::set_if_null(xLabelEvery, labelEvery)
  yLabelEvery <- domR::set_if_null(yLabelEvery, labelEvery)

  xLabelFirst <- domR::set_if_null(xLabelFirst, labelFirst)
  yLabelFirst <- domR::set_if_null(yLabelFirst, labelFirst)

  xLabelCex <- domR::set_if_null(xLabelCex, labelCex)
  yLabelCex <- domR::set_if_null(yLabelCex, labelCex)

  xLabelKula <- domR::set_if_null(xLabelKula, labelKula)
  yLabelKula <- domR::set_if_null(yLabelKula, labelKula)

  xLabelSrt <- domR::set_if_null(xLabelSrt, labelSrt)
  yLabelSrt <- domR::set_if_null(yLabelSrt, labelSrt)

  xLabelOffset <- domR::set_if_null(xLabelOffset, labelOffset)
  yLabelOffset <- domR::set_if_null(yLabelOffset, labelOffset)

  ## Names ----
  xNameCex <- domR::set_if_null(xNameCex, nameCex)
  yNameCex <- domR::set_if_null(xNameCex, nameCex)

  xNameKula <- domR::set_if_null(xNameKula, nameKula)
  yNameKula <- domR::set_if_null(xNameKula, nameKula)

  xNameOffset <- domR::set_if_null(xNameOffset, nameOffset)
  yNameOffset <- domR::set_if_null(xNameOffset, nameOffset)

  xNameSrt <- domR::set_if_null(xNameSrt, nameSrt)
  yNameSrt <- domR::set_if_null(xNameSrt, nameSrt)


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
  xInfo <- add_axis(axis = xAxisSide,

                      labels    = xLabels,
                      interval  = xInterval,
                      tickCount = xTickCount,

                      alignMidPoints = xAlignMidPoints,

                      gridEvery = xGridEvery,
                      gridFirst = xGridFirst,
                      gridKula  = xGridKula,
                      gridLwd   = xGridLwd,
                      gridType  = xGridType,

                      tickEvery  = xTickEvery,
                      tickFirst  = xTickFirst,
                      tickKula   = xTickKula,
                      tickLength = xTickLength,

                      axisLwd  = xAxisLwd,
                      axisType = xAxisType,
                      axisKula = xAxisKula,

                      labelEvery  = xLabelEvery,
                      labelFirst  = xLabelFirst,
                      labelCex    = xLabelCex,
                      labelKula   = xLabelKula,
                      labelOffset = xLabelOffset,
                      labelSrt    = xLabelSrt,

                      name       = xName,
                      nameSide   = xNameSide,
                      nameCex    = xNameCex,
                      nameKula   = xNameKula,
                      nameOffset = xNameOffset,
                      nameSrt    = xNameSrt)

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
  yInfo <- add_axis(axis = yAxisSide,

                      labels    = yLabels,
                      interval  = yInterval,
                      tickCount = yTickCount,

                      alignMidPoints = yAlignMidPoints,

                      gridEvery = yGridEvery,
                      gridFirst = yGridFirst,
                      gridKula  = yGridKula,
                      gridLwd   = yGridLwd,
                      gridType  = yGridType,

                      tickEvery  = yTickEvery,
                      tickFirst  = yTickFirst,
                      tickKula   = yTickKula,
                      tickLength = yTickLength,

                      axisLwd  = yAxisLwd,
                      axisType = yAxisType,
                      axisKula = yAxisKula,

                      labelEvery  = yLabelEvery,
                      labelFirst  = yLabelFirst,
                      labelCex    = yLabelCex,
                      labelKula   = yLabelKula,
                      labelOffset = yLabelOffset,
                      labelSrt    = yLabelSrt,

                      name       = yName,
                      nameSide   = yNameSide,
                      nameCex    = yNameCex,
                      nameKula   = yNameKula,
                      nameOffset = yNameOffset,
                      nameSrt    = yNameSrt)

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
  if (!is.null(annotationText)) {
    graphics::text(x = annotationLocation[1],
                   y = annotationLocation[2],
                   annotationText,
                   cex = annotationCex)
  }

  # Frame
  add_plot_frame()

  return(invisible(list("xTicks" = xInfo,
                        "yTicks" = yInfo)))
}
