add_axis <- function(axis,
                     tickSeq        = NULL,
                     tickCount      = NULL,
                     alignMidPoints = FALSE,
                     tickEvery      = NULL,
                     tickFirst      = NULL,
                     tickLength     = 0.2,
                     tickKula       = "#1A1A1AFF",
                     labels         = NULL,
                     labelEvery     = 2,
                     labelFirst     = 1,
                     labelOffset    = NULL,
                     labelCex       = 0.92,
                     labelSrt       = NULL,
                     labelKula      = "#4D4D4DFF",
                     axisLwd        = 1,
                     axisType       = 1,
                     axisKula       = "#1A1A1AFF",
                     name           = NULL,
                     nameAxis       = NULL,
                     nameKula       = "#1A1A1AFF",
                     nameCex        = 1,
                     nameOffset     = NULL,
                     nameSrt        = NULL,
                     gridEvery      = NULL,
                     gridFirst      = NULL,
                     gridLwd        = 1,
                     gridType       = 1,
                     gridKula       = "#E6E6E6AA") {
  #' Add highly customisable axes to a plot
  #'
  #' @description The syntax and logic for axes in base R are not intuitive.
  #'   This function allows the different parts of the axis to be customised
  #'   more clearly (at least for me). This function only works properly if
  #'   called after a `plot()` call where `xaxs = "i"` and `yaxs = "i"`. See
  #'   examples. Everything is based on the concept of "ticks", the little marks
  #'   that come over the edge of the axis to show where the values fall
  #'   exactly; labels and grid lines can be added to align with these ticks (or
  #'   a subset of them). It is also possible to only draw tickmarks on certain
  #'   ticks.
  #'
  #' @param axis numeric: Which side to add the axis to? 1 bottom, 2 left, 3
  #'   top, 4 right.
  #' @param tickSeq numeric: What interval is between the tick marks? See also
  #'   the tickCount and labels arguments. Only one of the three can be used,
  #'   with the priority being 1. labels, 2. tickSeq, 3. tickCount.
  #' @param tickCount numeric: How many ticks should be created? Include the
  #'   ticks that would fall at the minimum and maximum values. See also the
  #'   tickSeq and labels arguments. Only one of the three can be used, with the
  #'   priority being 1. labels, 2. tickSeq, 3. tickCount.
  #' @param alignMidPoints BINARY: Should the ticks be aligned midway between
  #'   values? The default is FALSE, which is necessary for a scatter plot where
  #'   the ticks line up with a single point value. However, on a matrix or a
  #'   barchart, the ticks cany sit in the centre of the column, so this would
  #'   be TRUE. See the examples for a visual explanation. If still unsure, call
  #'   `plot()` using `axes = TRUE` and add dummy data to see what aligns and
  #'   what makes sense.
  #' @param tickEvery numeric: Draw an actual tick mark every how many ticks? If
  #'   null, matches 'labelEvery'; see there for details.
  #' @param tickFirst numeric: WHich should be the first tick mark shown? Works
  #'   from the bottom on the y-axes and the left on the x-axes. Provide the
  #'   index, not the tick value.
  #' @param tickLength numeric: How long should the tick mark be? Positive
  #'   values go outwards from the axis. Works using `tcl`, so see par for more
  #'   information.
  #' @param tickKula What colour should the ticks be?
  #' @param labels Which labels (i.e. axis values) should be displayed beside
  #'   the tick marks? By default, this is NULL, and the true value are added.
  #'   However, customisable labels can also be added. This argument can be a
  #'   bit of a misnomer depending on how it is used - ticks are calculated
  #'   based on the number of labels, so if you want empty ticks it is necessary
  #'   to provide extra labels, and then exclude them with the "labelEvery"
  #'   argument. See also the tickSeq and tickCount arguments. Only one of the
  #'   three can be used, with the priority being 1. labels, 2. tickSeq, 3.
  #'   tickCount.
  #' @param labelEvery numeric: Add a label to every how many tick marks? If a
  #'   single value is provided, it is sequenced into the labels in increments
  #'   of that value (starting at the "labelFirst" argument). If a vector, it is
  #'   indexed directly into the labels (i.e. provide the indices, not the
  #'   values).
  #' @param labelFirst numeric: Which should be the first label shown? Works
  #'   from the bottom on the y-axes and the left on the x-axes. Provide the
  #'   index, not the tick value.
  #' @param labelOffset numeric: How far away from the axis should the labels be
  #'   written? Works on the "offset" argument of `text()`.
  #' @param labelCex numeric: What font size should the label text be?
  #' @param labelSrt numeric: What angle should the label text be? Positive
  #'   values rotate the label clockwise (i.e. the top moves right).
  #' @param labelKula What colour should the label text be?
  #' @param axisLwd numeric: How thick should the axis line be?
  #' @param axisType numeric: What line type should the axis line be? Use
  #'   numeric values for (1) solid; (2) dashed; (3) dotted lines; (4)
  #'   dot-dashed ; (5) long dashed ; and (6) dash-dotted.
  #' @param axisKula What colour should the axis line be?
  #' @param name "string" What is the axis name?
  #' @param nameAxis numeric: Which side of the axis should the name be added?
  #' @param nameKula What colour should the name text be?
  #' @param nameCex What font size should the name text be?
  #' @param nameSrt What angle should the name text be? Positive values rotate
  #'   the label clockwise (i.e. the top moves right).
  #' @param nameOffset numeric: How far from the axis should the name text be?
  #'   Works as the second value in the vector for the 'adj' argument of
  #'   `text()`; the first is 0.5, to centre the text.
  #' @param gridEvery numeric: Add a gridline across the full plot every how
  #'   many ticks? If null, matches 'labelEvery'; see there for details.
  #' @param gridFirst numeric: Which should be the first gridline shown? Works
  #'   from the bottom on the y-axes and the left on the x-axes. Provide the
  #'   index, not the tick value.
  #' @param gridLwd numeric: How thick should the gridlines be?
  #' @param gridType numeric: What line type should the gridlines be? See the
  #'   axisType argument.
  #' @param gridKula What colour should the gridlines be?
  #'
  #' @examples
  #'   # Most simple implementation
  #'   plot(1:10, xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, 1)
  #'
  #'   # Use different tick mark spacing
  #'   plot(1:10, xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(2, tickSeq = 0.25, gridEvery = 2, labelEvery = 4)
  #'
  #'   # Compare labels and gridline offsetting (use the "labelFirst" argument)
  #'   plot(1:10, xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis_3(4, tickSeq = 0.5, gridKula = "blue", gridEvery = 2,
  #'              labelEvery = 2, labelFirst = 3, labelKula = "blue")
  #'   add_axis_3(2, tickSeq = 1, gridEvery = 2, gridKula = "red",
  #'              labelEvery = 2, labelKula = "red")
  #'
  #' @examples
  #'   # Understand alignMidPoint argument; compare these three matrix plots
  #'   # 1) doesn't align
  #'   image(matrix(c(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[1:12], alignMidPoint = FALSE)
  #'
  #'   # 2) Aligned with midpoints
  #'   image(matrix(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[1:12], alignMidPoint = FALSE)
  #'
  #'   # 3) An extra label is added to align at the edges
  #'   image(matrix(c(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[c(1:12, 1)], alignMidPoint = FALSE)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Retrieve figure coordinates
  figPoints <- graphics::par("usr")
  figLeft   <- figPoints[1]
  figRight  <- figPoints[2]
  figBottom <- figPoints[3]
  figTop    <- figPoints[4]
  marValues <- graphics::par("mar")        # for label offsets
  figMidX   <- mean(c(figLeft, figRight))  # for axis name location
  figMidY   <- mean(c(figBottom, figTop))  # for axis name location

  # Ticks ---------------------------------------------------------------------!
  if (is.null(labels)) {
    if (is.null(tickSeq) & is.null(tickCount)) {
      stop("Either provide labels, or set the tickSeq or tickCount arguments")
    }
  } else {
    tickCount <- length(labels)
  }

  if (isFALSE(alignMidPoints)) {
  # Option 1: Ticks should align with the values (i.e. for a scatter plot)
    if (!is.null(tickCount)) {
      tickLocations <- switch(axis,
                              seq(figLeft,   figRight,  length = tickCount),
                              seq(figBottom, figTop,    length = tickCount),
                              seq(figLeft,   figRight,  length = tickCount),
                              seq(figBottom, figTop,    length = tickCount))
    } else if (!is.null(tickSeq)) {
      tickLocations <- switch(axis,
                              seq(figLeft,   figRight,  tickSeq),
                              seq(figBottom, figTop,    tickSeq),
                              seq(figLeft,   figRight,  tickSeq),
                              seq(figBottom, figTop,    tickSeq))
    }
  } else if (isTRUE(alignMidPoints)) {
    if (!is.null(tickCount)) {
      tickLocations <- switch(axis,
                              seq(figLeft,   figRight, length = tickCount + 1),
                              seq(figBottom, figTop,   length = tickCount + 1),
                              seq(figLeft,   figRight, length = tickCount + 1),
                              seq(figBottom, figTop,   length = tickCount + 1))

      # But we need to offset them
      tickOffset    <- (tickLocations[2] - tickLocations[1]) / 2
      tickLocations <- switch(axis,
                       seq(figLeft   + tickOffset, figRight - tickOffset, length = tickCount),
                       seq(figBottom + tickOffset, figTop   - tickOffset, length = tickCount),
                       seq(figLeft   + tickOffset, figRight - tickOffset, length = tickCount),
                       seq(figBottom + tickOffset, figTop   - tickOffset, length = tickCount))
    } else if (!is.null(tickSeq)) {
      tickOffset <- tickSeq / 2
      tickLocations <- switch(axis,
                              seq(figLeft   + tickOffset, figRight - tickOffset, tickSeq),
                              seq(figBottom + tickOffset, figTop   - tickOffset, tickSeq),
                              seq(figLeft   + tickOffset, figRight - tickOffset, tickSeq),
                              seq(figBottom + tickOffset, figTop   - tickOffset, tickSeq))
    }
  }

  # Labels --------------------------------------------------------------------!
  if (is.null(labels)) labels <- tickLocations
  if (length(labelEvery) == 1) {
    # Option 1: Label every x ticks
    labels <- labels[seq(labelFirst, length(tickLocations), labelEvery)]
    labelLocations <- tickLocations[seq(labelFirst, length(tickLocations), labelEvery)]
  } else {
    # Option 2: Label only specific ticks
    labels <- labels[labelEvery]
    labelLocations <- tickLocations[labelEvery]
  }

  # Gridlines -----------------------------------------------------------------!
  gridFirst <- domR::set_if_null(gridFirst, labelFirst)
  gridEvery <- domR::set_if_null(gridEvery, labelEvery)
  if (length(gridEvery) == 1) {
    gridLocations <- tickLocations[seq(gridFirst, length(tickLocations), gridEvery)]
  } else {
    gridLocations <- tickLocations[gridEvery]
  }

  # Display --------------------------------------------------------------------
  # Add grid lines
  hGrids <- switch(axis, NA, gridLocations, NA, gridLocations)
  vGrids <- switch(axis, gridLocations, NA, gridLocations, NA)
  graphics::abline(v = vGrids, h = hGrids,
                   col = gridKula, lwd = gridLwd, lty = gridType)

  # Add ticks ------------------------------------------------------------------
  tickFirst <- domR::set_if_null(tickFirst, labelFirst)
  tickEvery <- domR::set_if_null(tickEvery, labelEvery)
  if (length(tickEvery) == 1) {
    # Option 1: Label every x ticks
    tickMarks <- tickLocations[seq(tickFirst, length(tickLocations), tickEvery)]
  } else {
    # Option 2: Label only specific ticks
    tickMarks <- tickLocations[tickEvery]
  }

  axis(side   = axis,
       at     = tickMarks,
       labels = FALSE,            # no labels yet
       tcl    = tickLength * -1,  # negative here so positive values go outwards
       col    = tickKula)

  # Add labels ----!
  labelLocationsX <- switch(axis,
                            labelLocations, figLeft,
                            labelLocations, figRight)

  labelLocationsY <- switch(axis,
                            figBottom, labelLocations,
                            figTop, labelLocations)

  if (!is.null(labelSrt)) labelSrt <- labelSrt * -1 # rotate clockwise
  labelOffset <- domR::set_if_null(labelOffset, 0.75)

  graphics::text(x = labelLocationsX,
                 y = labelLocationsY,
                 labels = labels,
                 col = labelKula, srt = labelSrt, cex = labelCex,
                 offset = labelOffset, pos = axis,
                 xpd = TRUE)

  # Add axis ----!
  axisStart <- switch(axis, figLeft, figBottom, figLeft, figBottom)
  axisEnd   <- switch(axis, figRight, figTop, figRight, figTop)
  graphics::axis(side = axis, at = c(axisStart, axisEnd),
                 labels = FALSE, tcl = 0,                       # no labels or ticks
                 col = axisKula, lwd = axisLwd, lty = axisType) # custom options

  # Add axis name ----!
  if (!is.null(name)) {
    nameAxis <- domR::set_if_null(nameAxis, axis)  # default name side to axis side
    figMidX <- switch(nameAxis, figMidX,   figLeft, figMidX, figRight)
    figMidY <- switch(nameAxis, figBottom, figMidY, figTop,  figMidY)
    nameSrt <- domR::set_if_null(nameSrt, switch(nameAxis, 0, 90, 0, 270))
    nameOffset <- switch(axis,
                         domR::set_if_null(nameOffset, 2.4),
                         domR::set_if_null(nameOffset, -1.7),
                         domR::set_if_null(nameOffset, 1.5),
                         domR::set_if_null(nameOffset, 1.75))

    namePos <- switch(axis, 1, 3, 3, 3)

    graphics::text(x = figMidX,
                   y = figMidY,
                   labels = name,
                   xpd = TRUE,
                   adj = c(0.5, nameOffset),
                   cex = nameCex, srt = nameSrt, col = nameKula)
  }

  return(invisible(list(tickLocations)))
}
