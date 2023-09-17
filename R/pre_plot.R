pre_plot <- function(xLimits, yLimits,

                       main       = "",
                       mainOffset = 1,
                       mainCex    = 1.1,

                       xLabels = NULL,
                       yLabels = NULL,

                       xInterval = NULL,
                       yInterval = NULL,

                       xMeshlines  = NULL,
                       yMeshlines  = NULL,

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

             tickBoth = FALSE,
             xTickBoth = NULL,
             yTickBoth = NULL,

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

                       tagLocation = NULL,
                       tagText     = NULL,
                       tagCex      = 1,

                       mar = c(3, 3.5, 2.5, 1.5)) {
  #' Easily customisable plot areas
  #'
  #' @description An alternative to the default [plot()] that allows more
  #'   clarity in the customisation.
  #'
  #'   Most values can be set for the x and y axes separately (e.g. 'xGridLwd'
  #'   and 'yGridLwd') or with a single argument that applies to both (e.g.
  #'   'gridLwd'). However, a single argument doesn't make sense for some
  #'   arguments (e.g. 'xLabels' and 'yLabels' are almost always different).
  #'
  #'   Many of these arguments are fed directly into [add_axis()] so see there
  #'   for an overview of how the axes work.
  #'
  #' @inheritParams add_axis
  #' @param xLimits,yLimits vector The min and max values for the x and y-axes;
  #'   each vector is separately fed into [calc_intervals()] so see there for
  #'   accepted inputs. These are the only two mandatory arguments for this
  #'   function.
  #' @param main What is the title of the plot? Added above the plot, using
  #'   [mtext()].
  #' @param mainOffset How far from the axis should the title be? Set using the
  #'   'line' argument of [mtext()].
  #' @param mainCex What font size should the title text be?
  #'
  #' @param tickBoth Should tick marks be added to the opposing axis? For
  #'   example, if the y-axis is on the left (2), should tickmarks also be added
  #'   to the right axis (4)?
  #'
  #' @param addOrigin Should origin lines be added? These are thicker than the
  #'   gridlines to help orient the reader.
  #'
  #'   If TRUE, lines are drawn at x = 0 and y = 0; provide a `c(x, y)` vector
  #'   for alternative values. IF FALSE, no origin is added to the plot.
  #' @param originLwd numeric: How thick should the origin lines be?
  #' @param originKula What colour should the origin lines be?
  #' @param originType What type of line should the origin lines be? See
  #'   'axisType'.
  #'
  #' @param add121 logical. Should a red line with a gradient of x:y = 1:1 be
  #'   added to the plot?
  #'
  #' @param tagText "string": If not NULL, this text will be added to the plot.
  #'   Useful for "tagging" subplots with letters/numbers (e.g. `"a)"`). Be
  #'   aware that subsequent plotting of data could cover this.
  #' @param tagLocation numeric: A `c(x, y)` vector indicating where the
  #'   'tagText' should go.
  #' @param tagCex What font size should the 'tagText' be?
  #' @param mar Set the margins around the plot. See [par()] for details.
  #'
  #' @export

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

  xTickBoth <- domR::set_if_null(xTickBoth, tickBoth)
  yTickBoth <- domR::set_if_null(yTickBoth, tickBoth)

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
  yNameCex <- domR::set_if_null(yNameCex, nameCex)

  xNameKula <- domR::set_if_null(xNameKula, nameKula)
  yNameKula <- domR::set_if_null(yNameKula, nameKula)

  xNameOffset <- domR::set_if_null(xNameOffset, nameOffset)
  yNameOffset <- domR::set_if_null(yNameOffset, nameOffset)

  xNameSrt <- domR::set_if_null(xNameSrt, nameSrt)
  yNameSrt <- domR::set_if_null(yNameSrt, nameSrt)

  ## Handle limits ----
  xAuto <- calc_intervals(xLimits, intMax = 256,
                          preferError = TRUE) |> suppressWarnings()
  yAuto <- calc_intervals(yLimits, intMax = 256,
                          preferError = TRUE) |> suppressWarnings()

  xLimits <- xAuto$range
  yLimits <- yAuto$range

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
  # Define defaults if nothing is entered
  if (is.null(xLabels) & is.null(xMeshlines) & is.null(xInterval)) {
    xLabels   <- xAuto$vector
    xInterval <- xAuto$interval
  }

  # The rest is just done via add_axis
  xInfo <- add_axis(axis = xAxisSide,

                    labels    = xLabels,
                    interval  = xInterval,
                    meshlines = xMeshlines,

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
  if (isTRUE(xTickBoth)) {
    add_axis(axis       = switch(xAxisSide, 3, NA, 1, NA),   # opposing side
             alignMidPoints = xAlignMidPoints,
             labels     = unlist(xInfo),
             labelEvery = xLabelEvery,
             labelFirst = xLabelFirst,
             labelKula  = "white", labelCex = 0.001,         # hide labels
             tickEvery  = xTickEvery,
             tickFirst  = xTickFirst,
             tickKula   = xTickKula,
             tickLength = xTickLength,
             gridLwd = 0)
  }

  # y-axis ---------------------------------------------------------------------
  # Define defaults if nothing is entered
  if (is.null(yLabels) & is.null(yMeshlines) & is.null(yInterval)) {
    yLabels   <- yAuto$vector
    yInterval <- yAuto$interval
  }

  # The rest is just done via add_axis
  yInfo <- add_axis(axis = yAxisSide,

                    labels    = yLabels,
                    interval  = yInterval,
                    meshlines = yMeshlines,

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

  if (isTRUE(yTickBoth)) {
    add_axis(axis       = switch(yAxisSide, NA, 4, NA, 2),   # opposing side
             alignMidPoints = yAlignMidPoints,
             labels     = unlist(yInfo),
             labelEvery = yLabelEvery,
             labelFirst = yLabelFirst,
             labelKula  = "white", labelCex = 0.001,         # hide the labels
             tickEvery  = yTickEvery,
             tickFirst  = yTickFirst,
             tickKula   = yTickKula,
             tickLength = yTickLength,
             gridLwd = 0)
  }

  # Addition Decoration --------------------------------------------------------
  # Origin? FALSE = no origin; TRUE is automated to 0,0; or set (i.e. c(x, y))
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
  if (!is.null(tagText)) {
    if (is.null(tagLocation)) {
      tagLocation <- c(xLimits[1] + ((xLimits[2] - xLimits[1]) * 0.035),
                       yLimits[1] + ((yLimits[2] - yLimits[1]) * 0.9))
    }

    graphics::text(x = tagLocation[1],
                   y = tagLocation[2],
                   tagText,
                   cex = tagCex)
  }

  # Frame
  add_plot_frame()

  return(invisible(list("xTicks" = xInfo,
                        "yTicks" = yInfo)))
}
