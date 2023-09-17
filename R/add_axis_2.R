add_axis_2 <- function(axis,

                     labels    = NULL,
                     interval  = NULL,
                     tickCount = NULL,
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

                     axisLwd  = 1,
                     axisType = 1,
                     axisKula = "#1A1A1AFF",

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
      stop("Either provide labels, or set the interval or tickCount arguments")
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
  # If no labels are provided, just use actual values calculated
  if (is.null(labels)) labels <- round(scaffold,
                                       signif(sd(scaffold), 3) |>
                                         domR::count_decimal_places())

  # Where do the labels go?
  if (!is.null(labelEvery)) {
    # Option 1: Defaults, but we want at least 5 (if possible)
    labelTest <- labels[seq(2, length(scaffold), 2)]
    if (length(labelTest) < 5) {
      labels <- labels[seq(1, length(scaffold), 1)]
      labelEvery <- 1
      labelFirst <- 1
    } else {
      labels <- labelTest
      labelEvery <- 2
      labelFirst <- 2
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
  labelOffset <- domR::set_if_null(labelOffset,
                                   switch(axis, 0.9, 0.5, 0.9, 0.5))
  cat2(labelX)
  cat2(labelY)
  cat2(labels)
  cat2(labelKula)
  cat2(labelSrt)
  cat2(labelCex)
  cat2(labelOffset)
  print_line(">")

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
