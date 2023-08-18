calc_intervals <- function(v1, v2,
                           intMin = 5, intIdeal = 12, intMax = 100,
                           forceZero = NULL,
                           preferError = FALSE) {
  #' Calculate pretty intervals and include the boundaries exactly
  #'
  #' @description This function is my attempt at creating pretty intervals. It
  #'   differs from `pretty()` because it forces the min and max values to be
  #'   included in the returned vector. It also allows many more values to be
  #'   considered pretty (i.e. not just multiples of 1, 2, 5, but also values
  #'   such as e.g. 0.25, 0.75, and 4). It still needs further testing.
  #'
  #' @param v1 The first value. If less than v2, an ascending vector is
  #'   returned; if greater than v2, a descending vector is returned. If a
  #'   vector of length two is used, the second value will be used as v2. If a
  #'   vector of length > 2 is used, a range will be calculated and an ascending
  #'   vector returned.
  #' @param v2 The second value.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param intIdeal The ideal upper limit on the number of numbers in the
  #'   vector.
  #' @param intMax The absolute maximum number of numbers in the vector. By
  #'   default, vector lengths between 'intMin' and 'intIdeal' are searched for.
  #'   If this argument is not NULL, the function will try again, after
  #'   resetting 'intIdeal' to 'intMax' (i.e. it ranges from 'intMin' to
  #'   'intMax'). If a suitable vector is still not found, 'preferError'
  #'   determines what happens. If 'intMax' is NULL, 'intHigh' is not reset, and
  #'   the function jumps straight the 'preferError' behaviour.
  #'
  #'   Think about the use case; 100 intervals could be too many intervals for a
  #'   colour bar, but on an axis, the labels may only need to be drawn every so
  #'   many ticks, and 100 makes no discernible difference. (However, consider
  #'   that this is the equivalent to an alternative larger interval that was
  #'   not selected here because the numbers were too "weird").
  #' @param forceZero Does zero have to be included in the vectors? By default,
  #'   if v1 and v2 fall either side of 0, this will be TRUE; if both v1 and v2
  #'   have the same sign, it will be FALSE. These defaults can be overwritten.
  #' @param preferError If a suitable interval cannot be found using the
  #'   'intMin' and 'intIdeal' or 'intMax' arguments (see 'intMax'), is it
  #'   preferable to throw an error (TRUE), or default to using 'pretty()'? In
  #'   some cases `pretty` is not acceptable because it doesn't guarantee that
  #'   the exact limits are not included.-
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle if v1 and v2 are not numeric (i.e. they are labels) ----
  if ("character" %in% methods::is(v1)) {
    v2 <- length(v1)
    v1 <- 1
    vLabels <- TRUE
  } else {
    vLabels <- FALSE
  }

  # Handle if more than one value is provided to v1 ----
  if (length(v1) == 2) {
    v2 <- v1[2]
    v1 <- v1[1]
  } else if (length(v1) > 2) {
    v2 <- max(v1)
    v1 <- min(v1)
  }

  # Basic Set-Up ----
  vv     <- c(v1, v2)
  vvDiff <- max(vv) - min(vv)
  v1Prec <- domR::count_decimal_places(v1)
  v2Prec <- domR::count_decimal_places(v2)
  vvPrec <- max(v1Prec, v2Prec)
  # cat2(vv)
  # cat2(vvPrec)

  # Don't try to be too precise - `seq()` cannot handle such small intervals
  if (vvPrec > 5) {
    if (isTRUE(preferError)) {
      stop("Values are too precise to calculate exact intervals!")
    } else {
      warning("Values are too precise! Ignoring exact bounds & using `pretty()`\n")
      vVector    <- pretty(vv)
      usesPretty <- TRUE
    }
  }

  # The actual meat of this function
  if (vvPrec <= 5) {
    # Should zero be forced into the vector? ----
    if ((v1 < 0 & v2 > 0) | c(v1 > 0 & v2 < 0)) {
      forceZero <- domR::set_if_null(forceZero, TRUE)      # if diverging, use zero
      # cat("\nforcing Zero\n")
    } else {
      forceZero <- domR::set_if_null(forceZero, FALSE)     # if same sign, no zero
      # cat("\n not forcing Zero\n")
    }

    # Which intervals should be checked? ----
    baseIntervals  <- c(1:10, 12, 14, 15, 18, 25, 75)
    checkIntervals <- c()
    for (ii in c((-vvPrec:-1), (1:vvPrec))) {
      iiChecks <- baseIntervals * (10^ii)
      checkIntervals <- c(checkIntervals, iiChecks)
    }
    checkIntervals <- unique(checkIntervals) |> sort(TRUE)
    checkIntervals <- checkIntervals[checkIntervals < vvDiff]

    # Find exact intervals ----
    incIntervals <- c()          # preallocate to hold exact intervals
    for (ii in checkIntervals) {
      iiRemains <- (vvDiff %% ii) |> round(vvPrec + 1)
    # cat(ii, ", remainder:", iiRemains, "\n")
      if (iiRemains == 0) {
        incIntervals <- c(incIntervals, ii)
      }
    }
    # domR::cat2(incIntervals)

    # Check intervals ----
    acceptInterval <- FALSE # will use a while soon
    ii <- 1

    # Don't bother searching if no intervals are even a possibility
    if (length(incIntervals) < 1) {
      if (isTRUE(preferError)) {
        stop("No exact intervals found!")
      } else {
        warning("No exact intervals found! Ignoring exact bounds & using `pretty()`\n")
        vVector    <- pretty(vv)
        usesPretty <- TRUE
        acceptInterval <- TRUE
      }
    }

    # Do the intervals meet the criteria? (forceZero and vector length) ----
    # Use while because we need to update based on the outcome
    while (isFALSE(acceptInterval)) {
      # Create the vector
      vVector <- seq(v1, v2, incIntervals[ii]) |> round(vvPrec + 1)
      vLength <- length(vVector)
      vString <- as.character(vVector)
      vString <- round(vVector, vvPrec + 1) |> as.character()
      # cat("Interval of:", incIntervals[ii], "= vector length:", vLength, "\n")
      # Is the vector suitable?
      if (vLength < intMin | vLength > intIdeal) {
        # cat("Wrong length.\n")
        ii <- ii + 1
        acceptInterval <- FALSE
      } else if ( (isTRUE(forceZero)) & (as.character(0) %notIn% vString) ) {
        # cat("No zero.\n")
        ii <- ii + 1
        acceptInterval <- FALSE
      } else if ( (as.character(v1) %notIn% vString) |
                  (as.character(v2) %notIn% vString) ) {
        # seq doesn't have to hit the limits, so check if the values do
        # cat("Misses the limits!", vv, "not in", vString, "\n")
        ii <- ii + 1
        acceptInterval <- FALSE
      } else {
        # cat("\n The solution is:", vVector, "\n\n")
        usesPretty     <- FALSE
        acceptInterval <- TRUE
      }

      # Handle if no acceptable interval in found ----
      if (ii > length(incIntervals)) {
        # print("too long")
        if (is.null(intMax)) {
          # print("is null")
          if (isTRUE(preferError)) {
            # print("prefer error")
            stop("No exact interval found!")
          } else if (isFALSE(preferError)) {
            # print("prefer pretty")
            warning("Using pretty instead!\n")
            # cat("\nGoing pretty instead! \n")
            vVector        <- pretty(vv)
            usesPretty     <- TRUE
            acceptInterval <- TRUE
          }
        } else {
          # print("not null")
          ii <- 1                # reset and go again!
          if (intIdeal == intMax) {
            # print("ideal and max match")
            intMax <- NULL       # if it fails again, it goes to the block above
            acceptInterval <- FALSE
          } else {
            # print("ideal and max don't match - resetting")
            warning("Ignoring intIdeal & trying again!\n")
            # cat("\nIgnoring intIdeal & trying again! \n\n")
            intIdeal <- intMax
            acceptInterval <- FALSE
          }
        }
      }
    }
  }

  # Which direction should the interval go? ----
  if (v1 > v2) {
      vVector <- rev(vVector)  # change from descending to ascending
  }

  return(list("vVector"  = vVector,
              "vRange"   = range(vVector),
              "vLabels"  = vLabels,
              "vSeq"     = vVector[2] - vVector[1],
              "vPretty"  = usesPretty))
}
