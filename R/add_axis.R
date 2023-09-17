add_axis <- function(axis,

                     labels    = NULL,
                     interval  = NULL,
                     meshlines = NULL,
                     alignMidPoints = FALSE,

                     gridEvery = NULL,
                     gridFirst = NULL,
                     gridKula  = "#E6E6E6AA",
                     gridLwd   = 1,
                     gridType  = 1,

                     tickEvery  = NULL,
                     tickFirst  = NULL,
                     tickKula   = "#1A1A1AFF",
                     tickLength = 0.2,

                     axisKula = "#1A1A1AFF",
                     axisLwd  = 1,
                     axisType = 1,

                     labelEvery  = NULL,
                     labelFirst  = NULL,
                     labelCex    = 0.92,
                     labelKula   = "#4D4D4DFF",
                     labelOffset = NULL,
                     labelSrt    = NULL,

                     name       = NULL,
                     nameSide   = NULL,
                     nameCex    = 1,
                     nameKula   = "#1A1A1AFF",
                     nameOffset = NULL,
                     nameSrt    = NULL) {
  #' Add easily customisable axes to a plot
  #'
  #' @description The syntax and logic for customising axes in R are not
  #'   intuitive to me. This function allows different parts of an axis to be
  #'   more customised with more clarity.
  #'
  #'   **IMPORTANT** This function **ONLY** works correctly if called after
  #'   using [plot()] with the 'xaxs' and 'yaxs' **BOTH** set to "i". If not,
  #'   the numbers on the axis will not correspond correctly to the location of
  #'   the values. See examples.
  #'
  #'   Everything is based on a 'scaffold' mesh, which tickmarks, gridlines and
  #'   labels align to.
  #'
  #'   **Labels**: The values that are indicated along the axis (e.g. 0, 1, 2,
  #'   ...)
  #'
  #'   **Ticks**: The notch along the axis that shows the precise location the
  #'   labels refer to.
  #'
  #'   **Gridlines**: Lines that extend from the axis to the opposite side of
  #'   the plot. To suppress them and just add an axis, set 'gridLwd' as 0.
  #'
  #'   By default, these tend to align with each other, but they can be offset,
  #'   or only a subset can be included. For example, tick marks can be included
  #'   every 4 locations along the scaffold mesh, gridlines every 2, and a label
  #'   every 1.
  #'
  #'   Also keep in mind these definitions when reading this documentation:
  #'
  #'   **Axis**: The line along the side of the plot.
  #'
  #'   **Name**: The name of the axis indicates what the values refer to, for
  #'   example "Temperature (K)".
  #'
  #' @param axis Which side of the plot should the axis be added to? 1 = bottom;
  #'   2 = left; 3 = top; 4 = right.
  #'
  #' @param labels Which labels should be displayed beside the axis? If
  #'   provided, this argument is also used to define the number of scaffold
  #'   meshlines. It usually makes most sense to provide a vector of numbers,
  #'   but strings can also be provided, which allows values such as "<0" on the
  #'   axis, for example.
  #'
  #'   **NOTE** The function assumes that all values in the 'labels' vector are
  #'   equally spaced. For example, both `c(2, 4, 6, 8)` and `c(-1, "a", 0,
  #'   7.7)` would create 4 equally split meshlines in the same locations. The
  #'   difference would just be the label beside the meshline. There is no check
  #'   that the values entered correspond to the real value of the location. The
  #'   labels should therefore be considered as decorative features.
  #'
  #'   **NOTE**: The name of this argument can be a misnomer depending on how it
  #'   is used. For example, if 'labelEvery' is not 1, then some of the values
  #'   entered as 'labels' will be skipped over. It is necessary to provide all
  #'   possible labels (e.g. `c(1:8)`), and then use 'labelEvery' and
  #'   'labelFirst' to establish which should be displayed.
  #'
  #'   If this argument is not provided, and the scaffold is built using the
  #'   'meshlines' or 'interval' arguments, then the labels will default to
  #'   their true values. Only one of these three arguments can be used at once;
  #'   the order of priority is 1. 'labels'; 2. 'interval'; and 3. 'meshlines'.
  #'
  #' @param interval What interval is between the scaffolding meshlines?
  #'   Automatically identifies the minimum and maximum values of the plot's
  #'   axis and uses [seq()] to calculate the intermediate tick locations. See
  #'   also the 'meshlines' and 'labels' argument. Only one of these three
  #'   arguments can be used at once; the order of priority is 1. 'labels'; 2.
  #'   'interval'; and 3. 'meshlines'.
  #'
  #' @param meshlines numeric: How many ticks should be created? Include all
  #'   possible ticks, including those that would fall at the minimum and
  #'   maximum values. See also the 'interval' and 'labels' arguments. Only one
  #'   of these three arguments can be used at once; the order of priority is 1.
  #'   'labels'; 2. 'interval'; and 3. 'meshlines'.
  #'
  #' @param alignMidPoints BINARY: Should the scaffold mesh be aligned midway
  #'   between values? The default is FALSE, which is necessary for a scatter
  #'   plot and makes sure that the tickmarks line up with a single point value.
  #'
  #'   Set as TRUE for a plot such as an image of a matrix or a barchart, where
  #'   the ticks can sit in the  centre of the column.
  #'
  #'   See the examples for a visual explanation of this difference. If still
  #'   unsure, call [plot()] using `axes = TRUE` and add dummy data with this
  #'   function to see what aligns and what makes sense.
  #'
  #' @param gridEvery numeric: Gridlines across the full plot should be added
  #'   every how many lines of the scaffold mesh? If NULL, matches 'labelEvery';
  #'   see there for details on the accepted input.
  #' @param gridFirst numeric: Which line of the scaffold mesh should the first
  #'   gridline be added to? 'gridEvery' subsequently counts mesh lines from
  #'   this line. If NULL (default), matches 'labelFirst'; see there for details
  #'   on the accepted input.
  #' @param gridKula What colour should the gridlines be? Defaults to a
  #'   mid-grey.
  #' @param gridLwd numeric: How thick should the gridlines be? If 0, the
  #'   drawing of gridlines is suppressed.
  #' @param gridType numeric: What line type should the gridlines be? See the
  #'   'axisType' argument. If 0, the drawing of gridlines is suppressed.
  #'
  #' @param tickEvery Tickmarks should be drawn every how many lines of the
  #'   scaffold mesh? If NULL, matches 'labelEvery'; see there for details on
  #'   the accepted input.
  #' @param tickFirst numeric: Which line of the scaffold mesh should the first
  #'   tickmark be added to? 'tickEvery' subsequently counts mesh lines from
  #'   this line. If NULL (default), matches 'labelFirst'; see there for details
  #'   on the accepted input.
  #' @param tickKula What colour should the tickmarks be? Defaults to black.
  #' @param tickLength numeric: How long should the tickmarks be? Works based on
  #'   the `tcl` argument of [axis()] (but reversed so positive values go away
  #'   from the axis). See [par()] for more details.
  #'
  #' @param axisKula What colour should the axis line be?
  #' @param axisLwd How thick should the axis line be?
  #' @param axisType numeric: What type of line should the axis line be? Use
  #'   numeric values for (1) solid; (2) dashed; (3) dotted lines; (4)
  #'   dot-dashed ; (5) long dashed ; and (6) dash-dotted.
  #'
  #' @param labelEvery numeric: Labels should be added every how many lines of
  #'   the scaffold mesh? If a single value is provided, it is sequenced into
  #'   the provided labels (or an automated calculation of them if labels is
  #'   NULL), starting from the 'labelFirst' argument. If a vector is provided,
  #'   it is used to directly index into the labels. Use the index of the
  #'   scaffold meshlines, **NOT** the actual values of the labels.
  #'
  #'   Meshlines are counted along y-axes from the bottom upwards, and along
  #'   x-axes from the left to the right.
  #'
  #'   By default, the function will set this as 2, unless that provides less
  #'   than 5 labels, in which case, it is set as 1.
  #'
  #' @param labelFirst numeric: Which line of the scaffold mesh should the first
  #'   label be added to? 'labelEvery' subsequently counts mesh lines from this
  #'   line. Provide the index, not the label value. Default is to match
  #'   'labelEvery'.
  #'
  #' @param labelCex numeric: What font size should the label text be?
  #' @param labelKula What colour should the label text be?
  #' @param labelOffset numeric: How far from the axis should the labels be?
  #'   Works on the 'offset' argument of [text()].
  #' @param labelSrt numeric: What angle should the label text be set at?
  #'   Positive values rotate the text clockwise (i.e. the tops of the letters
  #'   move right).
  #'
  #' @param name "string": What does the axis show?
  #' @param nameSide numeric: Which side of the axis (**NOT** the plot), should
  #'   the name be added? Default is to the outside of the axis.
  #' @param nameCex numeric: What font size should the name text be?
  #' @param nameKula What colour should the name text be?
  #' @param nameOffset numeric: How far from the axis should the name be? Works
  #'   as the second value in the vector for the 'adj' argument of [text()], but
  #'   reversed so positive values move away from the axis; the first value in
  #'   the vector is kept at 0.5 to centre the text.
  #' @param nameSrt numeric: What angle should the name text be set at? Positive
  #'   values rotate the text clockwise (i.e. the tops of the letters move
  #'   right).
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
    if (is.null(interval) & is.null(meshlines)) {
      stop("Either provide labels, or set the interval or meshlines arguments")
    }
  } else {
    meshlines <- length(labels)
  }

  # Calculate scaffold locations
  if (isFALSE(alignMidPoints)) {       # ticks on the values (for line graph)
    # Option 1: Ticks should align with the values (i.e. for a scatter plot)
    if (!is.null(meshlines)) {
      scaffold <- switch(axis,
                         seq(figLeft,   figRight,  length = meshlines),
                         seq(figBottom, figTop,    length = meshlines),
                         seq(figLeft,   figRight,  length = meshlines),
                         seq(figBottom, figTop,    length = meshlines))
    } else if (!is.null(interval)) {
      scaffold <- switch(axis,
                         seq(figLeft,   figRight,  interval),
                         seq(figBottom, figTop,    interval),
                         seq(figLeft,   figRight,  interval),
                         seq(figBottom, figTop,    interval))
    }
  } else if (isTRUE(alignMidPoints)) { # ticks offset (for bars / matrices)
    # Option 2: Ticks should be offset (i.e. for a matric or bar chart)
    if (!is.null(interval)) {
      skOff <- interval / 2       # offset by half, to the middle
      scaffold <- switch(axis,
                         seq(figLeft   + skOff, figRight - skOff, interval),
                         seq(figBottom + skOff, figTop   - skOff, interval),
                         seq(figLeft   + skOff, figRight - skOff, interval),
                         seq(figBottom + skOff, figTop   - skOff, interval))
    } else if (!is.null(meshlines)) {
      # For a tick count, need to calculate the interval first to offset it, so
      # add an extra tick, then "shift" the bar to exclude that extra one
      scaffold <- switch(axis,
                         seq(figLeft,   figRight, length = meshlines + 1),
                         seq(figBottom, figTop,   length = meshlines + 1),
                         seq(figLeft,   figRight, length = meshlines + 1),
                         seq(figBottom, figTop,   length = meshlines + 1))

      # But we need to offset by half an interval, so that they align
      skOff    <- (scaffold[2] - scaffold[1]) / 2 # offset to middle
      scaffold <- switch(axis,
                         seq(figLeft   + skOff, figRight - skOff, length = meshlines),
                         seq(figBottom + skOff, figTop   - skOff, length = meshlines),
                         seq(figLeft   + skOff, figRight - skOff, length = meshlines),
                         seq(figBottom + skOff, figTop   - skOff, length = meshlines))
    }
  }

  # Labels ---------------------------------------------------------------------
  # If no labels are provided, just use actual values calculated
  if (is.null(labels)) labels <- round(scaffold,
                                       signif(sd(scaffold), 3) |>
                                         domR::count_decimal_places())

  # Where do the labels go?
  if (is.null(labelEvery)) {
    # Option 1: Defaults, but we want at least 5 (if possible) and less than 10
    labelEvery <- 2
    labelFirst <- domR::set_if_null(labelFirst, labelEvery)
    labelTest <- labels[seq(2, length(scaffold), labelEvery)]

    if (length(labelTest) <= 5) {
      labelEvery <- 1
      labelFirst <- 1
    } else if (length(labelTest) > 10) {
      labels <- labelTest
      if (length(labelTest) > 10) {
        labelEvery <- ceiling(length(labelTest) / 8)
      }
    }
    labelLocations <- scaffold[seq(labelFirst, length(scaffold), labelEvery)]
  } else if (length(labelEvery) == 1) {
    # Option 2: Label every x ticks (user-defined)
    labelFirst <- domR::set_if_null(labelFirst, labelEvery)
    labels     <- labels[seq(labelFirst, length(scaffold), labelEvery)]
    labelLocations <- scaffold[seq(labelFirst, length(scaffold), labelEvery)]
  } else {
    # Option 3: Label only specific ticks (labelEvery is a vector of indices)
    labels <- labels[labelEvery]
    labelLocations <- scaffold[labelEvery]
    labelFirst <- domR::set_if_null(labelFirst, labelEvery[1]) # for others
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
    guides <- labelLocations[which(labelLocations %in% gridLocations)]
    vGuide <- switch(axis, guides, NA, guides, NA)
    hGuide <- switch(axis, NA, guides, NA, guides)

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
  labelOffset <- domR::set_if_null(labelOffset,
                                   switch(axis, 0.75, 0.5, 0.65, 0.5))

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
    nameX  <- switch(nameSide, figMidX, figLeft, figMidX, figRight)
    nameY  <- switch(nameSide, figBottom, figMidY, figTop, figMidY)

    nameSrt  <- domR::set_if_null(nameSrt, switch(nameSide, 0, 90, 0, 270))
    nameOffset <- switch(axis,
                         domR::set_if_null(nameOffset, 3.75),
                         domR::set_if_null(nameOffset, 3.5),
                         domR::set_if_null(nameOffset, 1.5),
                         domR::set_if_null(nameOffset, 1.75))
    if (axis == 2) nameOffset <- nameOffset * -1  # positive is outwards for all

    graphics::text(x = nameX,
                   y = nameY,
                   labels = name,
                   xpd = TRUE,
                   adj = c(0.5, nameOffset),
                   cex = nameCex, srt = nameSrt, col = nameKula)
  }

  return(invisible(list(scaffold)))
}
