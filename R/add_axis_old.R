add_axis_old <- function(axis,
                     tickSeq        = NULL,
                     tickCount      = NULL,
                     alignMidPoints = FALSE,
                     tickEvery      = NULL,
                     tickFirst      = NULL,
                     tickLength     = 0.2,
                     tickKula       = "#1A1A1AFF",
                     labels         = NULL,
                     labelEvery     = 2,
                     labelFirst     = NULL,
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
  #' @description The syntax and logic for customising axes in base R are not
  #'   intuitive (to me). This function allows the different parts of the axis
  #'   to be customised more clearly.
  #'
  #'   This function only works properly if called after a [plot()] call where
  #'   `'xaxs' = "i"` and `'yaxs' = "i"`. See examples.
  #'
  #'   Everything is based on the concept of a scaffold, which is a mesh that
  #'   the tickmarks, gridlines, and labels align to. By deault, these tend to
  #'   align with each other, but they can be offset, or only a subset of them
  #'   included (for example, add a label every 4 scaffold locations, but a grid
  #'   line every 2, etc.)
  #' @param axis numeric: Which side of the plot should the axis be add to? 1
  #'   bottom, 2 left, 3 top, 4 right.
  #' @param interval numeric: What interval is between the scaffolding mesh? See
  #'   also the 'tickCount' and 'labels' arguments. Only one of these three can
  #'   be used, with the priority being 1. 'labels', 2. 'interval', 3.
  #'   'tickCount'.
  #' @param tickCount numeric: How many ticks should be created? Include the
  #'   ticks that would fall at the minimum and maximum values. See also the
  #'   'interval' and 'labels' arguments. Only one of these three can be used,
  #'   with the priority being 1. 'labels', 2. 'interval', 3. 'tickCount'.
  #'
  #' @param alignMidPoints BINARY: Should the scaffolding be aligned midway
  #'   between values? The default is FALSE, which is necessary for a scatter
  #'   plot - the ticks should line up with a single point value.
  #'
  #'   Set as TRUE for a plot such as an image of a matrix or a barchart, where
  #'   the ticks can sometimes sit in the  centre of the column. See the
  #'   examples for a visual explanation. If still unsure, call [plot()] using
  #'   `axes = TRUE` and add dummy data to see what aligns and what makes sense.
  #' @param tickEvery numeric: Draw an actual tick mark every how many
  #'   scaffolding poles? If null, matches 'labelEvery'; see there for details.
  #' @param tickFirst numeric: On which scaffolding pole should be the first
  #'   tick mark be shown? Counts from the bottom on the y-axes and the left on
  #'   the x-axes. Provide the index, not the corresponding label / value.
  #' @param tickLength numeric: How long should the tick marks be? Positive
  #'   values go outwards from the axis. Works using `tcl`; see [par()] for more
  #'   information.
  #' @param tickKula What colour should the ticks be?
  #' @param labels Which labels (i.e. axis values) should be displayed beside
  #'   the scaffolding? By default, this is NULL, and the actual numeric values
  #'   are added. However, customisable labels can also be added.
  #'
  #'   This argument can be a bit of a misnomer depending on how it is used - if
  #'   labels are provided, the scaffolding is calculated based on the number of
  #'   labels. To leave parts of the axis empty, it is necessary to provide
  #'   labels for all possible scaffolding, but then exclude them with the
  #'   "labelEvery" argument.
  #'
  #'   See also the 'interval' and 'tickCount' arguments. Only one of these
  #'   three can be used, with the priority being 1. 'labels', 2. 'interval', 3.
  #'   'tickCount'.
  #' @param labelEvery numeric: Add a label to every how many scaffold poles? If
  #'   a single value is provided, it is sequenced into the provided labels in
  #'   increments of that value (starting at the 'labelFirst' argument). If a
  #'   vector, it is indexed directly into the labels (i.e. provide the indices,
  #'   not the values).
  #' @param labelFirst numeric: Which should be the first label shown? Counts
  #'   from the bottom up on the y-axes and the left across on the x-axes.
  #'   Provide the index, not the tick value. Defaults to match 'labelEvery'.
  #' @param labelOffset numeric: How far away from the axis should the labels be
  #'   written? Works on the "offset" argument of [text()].
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
  #'   [text()]; the first is 0.5, to centre the text.
  #' @param gridEvery numeric: Add a gridline across the full plot every how
  #'   many scaffolding bars? If NULL, matches 'labelEvery'; see there for
  #'   details.
  #' @param gridFirst numeric: Which should be the first gridline shown? Counts
  #'   from the bottom up on the y-axes and the left across on the x-axes.
  #'   Provide the index, not the tick value.
  #' @param gridLwd numeric: How thick should the grid lines be? Set this or
  #'   'gridType' as 0 to suppress any grid lines.
  #' @param gridType numeric: What line type should the gridlines be? See the
  #'   'axisType' argument. Set this or gridLwd as 0 to suppress grid lines.
  #' @param gridKula What colour should the gridlines be?
  #'
  #' @examples
  #' \dontrun{
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
  #' }
  #'
  #' @examples
  #' \dontrun{
  #'   # Understand alignMidPoint argument; compare these three matrix plots
  #'   # 1) doesn't align
  #'   image(matrix(c(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[1:12], alignMidPoint = FALSE)
  #'
  #'   # 2) Aligned with midpoints
  #'   image(matrix(c(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[1:12], alignMidPoint = FALSE)
  #'
  #'   # 3) An extra label is added to align at the edges
  #'   image(matrix(c(1:144), ncol = 12, byrow = TRUE),
  #'         xaxs = "i", yaxs = "i", axes = FALSE)
  #'   add_axis(1, labels = month.abb[c(1:12, 1)], alignMidPoint = FALSE)
  #' }
  #'
  #' @export

  # Code ----------------------------------------------------------------------
  # Retrieve coordinates of the current figure
  figPoints <- graphics::par("usr")
  figLeft   <- figPoints[1]
  figRight  <- figPoints[2]
  figBottom <- figPoints[3]
  figTop    <- figPoints[4]
  marValues <- graphics::par("mar")        # for label offsets
  figMidX   <- mean(c(figLeft, figRight))  # for axis name location
  figMidY   <- mean(c(figBottom, figTop))  # for axis name location

  # Create scaffold ------------------------------------------------------------
  # The idea is to create a scaffold that the other parts can be aligned with.
  # The scaffold depends on the number of labels, or the interval, or number of
  # ticks (preference is that order).
  if (is.null(labels)) {
    if (is.null(interval) & is.null(tickCount)) {
      stop("Either provide labels, or set the tickSeq or tickCount arguments")
    }
  } else {
    tickCount <- length(labels)
  }

  # Calculate scaffold locations
  if (isFALSE(alignMidPoints)) {       # ticks on the values (for line graph)
    # Option 1: Ticks should align with the values (i.e. for a scatter plot)
    if (!is.null(tickCount)) {
      scaffold <- switch(axis,
                         seq(figLeft,   figRight,  length = tickCount),
                         seq(figBottom, figTop,    length = tickCount),
                         seq(figLeft,   figRight,  length = tickCount),
                         seq(figBottom, figTop,    length = tickCount))
    } else if (!is.null(tickSeq)) {
      scaffold <- switch(axis,
                         seq(figLeft,   figRight,  interval),
                         seq(figBottom, figTop,    interval),
                         seq(figLeft,   figRight,  interval),
                         seq(figBottom, figTop,    interval))
    }
  } else if (isTRUE(alignMidPoints)) { # ticks offset (for bars / matrices)
    # Option 2: Ticks should be offset (i.e. for a matric or bar chart)
    if (!is.null(interval)) {
      skOff <- tickSeq / 2       # offset by half, to the middle
      scaffold <- switch(axis,
                         seq(figLeft   + skOff, figRight - skOff, interval),
                         seq(figBottom + skOff, figTop   - skOff, interval),
                         seq(figLeft   + skOff, figRight - skOff, interval),
                         seq(figBottom + skOff, figTop   - skOff, interval))
    } else if (!is.null(tickCount)) {
      # For a tick count, need to calculate the interval first to offset it, so
      # add an extra tick, then "shift" the bar to exclude that extra one
      scaffold <- switch(axis,
                         seq(figLeft,   figRight, length = tickCount + 1),
                         seq(figBottom, figTop,   length = tickCount + 1),
                         seq(figLeft,   figRight, length = tickCount + 1),
                         seq(figBottom, figTop,   length = tickCount + 1))

      # But we need to offset by half an interval, so that they align
      skOff    <- (scaffold[2] - scaffold[1]) / 2 # offset to middle
      scaffold <- switch(axis,
                         seq(figLeft   + skOff, figRight - skOff, length = tickCount),
                         seq(figBottom + skOff, figTop   - skOff, length = tickCount),
                         seq(figLeft   + skOff, figRight - skOff, length = tickCount),
                         seq(figBottom + skOff, figTop   - skOff, length = tickCount))
    }
  }

  # Labels ---------------------------------------------------------------------
  labelFirst <- domR::set_if_null(labelFirst, labelEvery)

  # If no labels are provided, just use actual values calculated
  if (is.null(labels)) labels <- round(scaffold,
                                       signif(sd(scaffold), 3) |>
                                         domR::count_decimal_places())

  # Where do the labels go?
  if (length(labelEvery) == 1) {
    # Option 1: Label every x ticks
    labels <- labels[seq(labelFirst, length(scaffold), labelEvery)]
    labelLocations <- scaffold[seq(labelFirst, length(scaffold), labelEvery)]
  } else {
    # Options 2: Label only specific ticks (labelEvery is a vector of indices)
    labels <- labels[labelEvery]
    labelLocations <- scaffold[labelEvery]
  }

  # Tickmarks ------------------------------------------------------------------
  tickFirst <- domR::set_if_null(tickFirst, labelFirst)
  tickEvery <- domR::set_if_null(tickEvery, labelEvery)

  if (length(tickEvery) == 1) {
    # Options 1: Add ticks to every scaffold
    tickMarks <- scaffold[seq(tickFirst, length(scaffold), tickEvery)]
  } else {
    tickMarks <- scaffold[tickEvery]
  }

  # Gridlines ------------------------------------------------------------------
  gridFirst <- domR::set_if_null(gridFirst, labelFirst)
  gridEvery <- domR::set_if_null(gridEvery, labelEvery)

  if (length(gridEvery) == 1) {
    gridLocations <- scaffold[seq(gridFirst, length(scaffold), gridEvery)]
  } else {
    gridLocations <- scaffold[gridEvery]
  }

  # Display ====================================================================
  # Build up from the back: gridlines, tick marks, axis, labels, name

  # Add grid lines -------------------------------------------------------------
  if (gridLwd > 0) {
    # Base mesh background
    vGrids <- switch(axis, gridLocations, NA, gridLocations, NA)
    hGrids <- switch(axis, NA, gridLocations, NA, gridLocations)

    # Darker lines to align with the labels
    vGuide <- switch(axis, labelLocations, NA, labelLocations, NA)
    hGuide <- switch(axis, NA, labelLocations, NA, labelLocations)

    # Add them
    graphics::abline(v = vGrids, h = hGrids,
                     col = gridKula, lwd = gridLwd, lty = gridType)

    graphics::abline(v = vGuide, h = hGuide,
                     col = gridKula, lwd = gridLwd + 0.5, lty = gridType)
  }

  # Add tick marks -------------------------------------------------------------
  graphics::axis(side   = axis,
                 at     = tickMarks,
                 labels = FALSE,           # will add later
                 tcl    = tickLength * -1, # negative so positive is outwards
                 col    = tickKula)

  # Add axis -------------------------------------------------------------------
  # The above axis only goes from the ticks, not necessarily the full length of
  # the axis, so we'll add another one here
  axisStart <- switch(axis, figLeft, figBottom, figLeft, figBottom)
  axisEnd   <- switch(axis, figRight, figTop, figRight, figTop)
  graphics::axis(side = axis, at = c(axisStart, axisEnd),
                 labels = FALSE, tcl = 0,                       # no labels/ticks
                 col = axisKula, lwd = axisLwd, lty = axisType) # custom

  # Add labels -----------------------------------------------------------------
  labelX <- switch(axis,
                   labelLocations, figLeft,
                   labelLocations, figRight)
  labelY <- switch(axis,
                   figBottom, labelLocations,
                   figTop, labelLocations)

  if (!is.null(labelSrt)) labelSrt <- labelSrt * -1 # so input rotation is clockwise
  labelOffset <- domR::set_if_null(labelOffset, 0.9)

  graphics::text(x = labelX, y = labelY,
                 labels = labels,
                 col = labelKula, srt = labelSrt, cex = labelCex,
                 offset = labelOffset, pos = axis,
                 xpd = TRUE)

  # Add axis name --------------------------------------------------------------
  if (!is.null(name)) {
    # Location
    namePos  <- switch(axis, 1, 3, 3, 3)
    nameSide <- domR::set_if_null(nameSide, axis) # default is outside axis
    figMidX  <- switch(nameSide, figMidX, figLeft, figMidX, figLeft)
    figMidY  <- switch(nameSide, figBottom, figMidY, figBottom, figMidY)

    nameSrt  <- domR::set_if_null(nameSrt, switch(nameSide, 0, 90, 0, 270))
    nameOffset <- switch(axis,
                         domR::set_if_null(nameOffset, 2.4),
                         domR::set_if_null(nameOffset, -2.2),
                         domR::set_if_null(nameOffset, 1.5),
                         domR::set_if_null(nameOffset, 1.75))

    graphics::text(x = figMidX,
                   y = figMidY,
                   labels = name,
                   xpd = TRUE,
                   adj = c(0.5, nameOffset * -1), # positive outwards
                   cex = nameCex, srt = nameSrt, col = nameKula)
  }
  return(invisible(list(scaffold)))
}
