calc_intervals <- function(x0, x1 = NULL,
                           intMin = 5, intIdeal = 12, intMax = 100,
                           forceZero = NULL,
                           preferError = FALSE) {
  #' Calculate pretty intervals and include the boundaries exactly
  #'
  #' @description This function is an alternative attempt at creating pretty
  #'   intervals. It differs from [pretty()] because it forces the min and max
  #'   values to be included in the returned vector. It also allows many more
  #'   values to be considered as "pretty" (i.e. not just multiples of 1, 2, 5,
  #'   but also values such as e.g. 0.25, 0.75, and 4).
  #'
  #'   It still needs further testing.
  #'
  #' @param x0 The first value for the interval.
  #'
  #'   If less than 'x1', an ascending vector is returned; if greater than 'x1',
  #'   a descending vector is returned. If a vector of length two is used, the
  #'   second value will be used as 'x1'. If a vector of length > 2 is used, a
  #'   range will be calculated from it; a descending vector is returned if the
  #'   first value is greater than the last value, otherwise an ascending vector
  #'   is returned.
  #' @param x1 The second value.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param intIdeal The ideal upper limit on the number of numbers in the
  #'   vector.
  #' @param intMax The absolute maximum number of numbers in the vector. By
  #'   default, vector lengths between 'intMin' and 'intIdeal' are searched for.
  #'   If this argument is not NULL, the function will try again, after
  #'   resetting 'intIdeal' to 'intMax' (i.e. it then ranges from 'intMin' to
  #'   'intMax'). If a suitable vector is still not found, 'preferError'
  #'   determines what happens. If 'intMax' is NULL, 'intIdeal' is not reset,
  #'   and the function jumps straight to the 'preferError' behaviour.
  #'
  #'   Think about the use case; 100 intervals could be too many intervals for a
  #'   colour bar, but on an axis, the labels may only need to be drawn every so
  #'   many ticks, and 100 makes no discernible difference. (However, consider
  #'   that this is the equivalent to an alternative larger interval that was
  #'   not selected here because the numbers were too "weird").
  #' @param forceZero Does zero have to be included in the vectors? By default,
  #'   if 'x0' and 'x1' fall either side of 0, this will be TRUE; if both 'x0'
  #'   and 'x1' have the same sign, it will be FALSE. These defaults can be
  #'   overwritten.
  #' @param preferError If a suitable interval cannot be found using the
  #'   'intMin' and 'intIdeal' or 'intMax' arguments (see 'intMax'), is it
  #'   preferable to throw an error (TRUE), or default to using [pretty()]? In
  #'   some (many / most) cases [pretty()] will not be acceptable because it
  #'   doesn't guarantee that the exact limits are not included in hte returned
  #'   vector.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle if labels (character strings) are provided instead of numbers -------
  if ("character" %in% methods::is(x0)) {
    x0  <- length(x0)
    x1  <- 1
    xLabels <- TRUE        # add to metadata; for other functions
  } else {
    xLabels <- FALSE
  }

  # Handle if more than one value is provided as v1 ----------------------------
  if (length(x0) == 2) {                # x0 is c(x0, x1)
    x1 <- x0[2]
    x0 <- x0[1]
  } else if (length(x0) > 2) {          # x0 is a full vector
    if (x0[1] > x0[length(x0)]) {
      x1 <- min(x0, na.rm = TRUE)
      x0 <- max(x0, na.rm = TRUE)
    } else {
      x1 <- max(x0, na.rm = TRUE)
      x0 <- min(x0, na.rm = TRUE)
    }
  }

  # Basic Set-Up ---------------------------------------------------------------
  # We need to know a few things at the offset
  xx <- c(x0, x1)

  x0prec <- count_decimal_places(x0)
  x1prec <- count_decimal_places(x1)
  xxPrec <- max(x0prec, x1prec, na.rm = TRUE)

  xxDiff <- (max(xx, na.rm = TRUE) - min(xx, na.rm = TRUE)) |>
    round(xxPrec + 1) # range

  # Is it necessary to add a 0? ------------------------------------------------
  if ((x0 < 0 & x1 > 0) | (x0 > 0 & x1 < 0)) {
    forceZero <- set_if_null(forceZero, TRUE)   # if diverging, use a zero
  } else {
    forceZero <- set_if_null(forceZero, FALSE)  # if same sign, no zero
  }

  # Prepare intervals to check--------------------------------------------------
  baseIntervals <- c(1:10, 1.2, 1.5, 2.5, 7.5)
  toCheck <- lapply(baseIntervals, `*`,
                    10^((-xxPrec - 1):(xxPrec + 1))) |> unlist()
  toCheck <- unique(toCheck) |> sort(TRUE)  # sort so will stop at largest first
  toCheck <- toCheck[toCheck < xxDiff]      # interval must be less than range

  # Check which are exact intervals --------------------------------------------
  incIntervals <- c() # preallocate to hold exact intervals

  # Check each possible base interval
  for (ii in toCheck) {
    # Decimals don't work well with modulo, so multiply up magnitudes to compare
    iiPrec <- max(count_decimal_places(ii), xxPrec)

    if (iiPrec != 0) {
      iiCheck <- ii * 10^(iiPrec)   #
      ixDiff  <- xxDiff * 10^(iiPrec)
    } else {
      iiCheck <- ii
      ixDiff  <- xxDiff
    }

    # Check if it is exact
    iiRemains <- (ixDiff %% iiCheck)
    iiRemain2 <- round(iiRemains, xxPrec + 1)   # matches within tolerance

    # Store if it is exact
    if (iiRemain2 == 0) {
      incIntervals <- c(incIntervals, ii)
    }
  }

  # Choose an interval ---------------------------------------------------------
  ## Prep
  acceptInt <- FALSE   # controls the while loop; stops when we're happy
  ii <- 1              # tracks count so not infinite while looping

  # Don't bother searching if no intervals are even a possibility
  if (length(incIntervals) < 1) {
    if (isTRUE(preferError)) {
      stop("No exact intervals found!")
    } else {
      warning("No exact intervals found! Ignoring exact bounds & using `pretty()`\n")
      xVector    <- pretty(xx)  # default if accepted; will not hit exact limits
      usesPretty <- TRUE        # metadata
      acceptInt  <- TRUE
    }
  }

  ## Do the possible intervals identified meet the criteria? -------------------
  ## Criteria are forceZero and vector length
  ## Use while loop because outcome updates our options
  while(isFALSE(acceptInt)) {
    # Create the vector to check
    xVector <- seq(min(xx), max(xx), incIntervals[ii])

    # To check the length
    xLength <- length(xVector)

    # To check if zero (sidesteps rounding / floating errors)
    xString <- as.character(xVector)

    # Is the vector suitable?
    if (xLength < intMin | xLength > intIdeal) {
       ii <- ii + 1
      acceptInt <- FALSE
    } else if ((isTRUE(forceZero)) & as.character(0) %notIn% xString) {
      ii <- ii + 1
      acceptInt <- FALSE
    } else if ( (as.character(x0) %notIn% xString) |
                (as.character(x1) %notIn% xString) ) {
      # `seq()` doesn't have to hit the limits, so check if our values are used
      ii <- ii + 1
      acceptInt <- FALSE
    } else {
      usesPretty <- FALSE     # for metadata
      acceptInt  <- TRUE      # end the while loop
    }

    # Handle if no acceptable intervals are found ------------------------------
    if (ii > length(incIntervals)) {
       if (is.null(intMax)) {
         if (isTRUE(preferError)) {
           stop("No exact interval found!")
        } else if (isFALSE(preferError)) {
           warning("Using pretty instead!\n")
           xVector    <- pretty(xx)
           usesPretty <- TRUE
           acceptInt  <- TRUE
        }
      } else {
        ii <- 1                # reset and go again!
        if (intIdeal == intMax) {
          intMax    <- NULL    # if it fails again, it goes to the block above
          acceptInt <- FALSE
        } else {
          warning("Ignoring intIdeal & trying again!\n")
          intIdeal  <- intMax
          acceptInt <- FALSE
        }
      }
    }
  }

  # Change from ascending to descending?
  xRange <- range(xVector)
  if (x0 > x1) {
    xVector <- rev(xVector)
    xRange  <- rev(xRange)
  }

  # Return with metadata for other functions to understand what happened here!
  return(list("vector"     = xVector,
              "range"      = xRange,
              "labels"     = xLabels,
              "interval"   = xVector[2] - xVector[1],
              "usesPretty" = usesPretty))
}
