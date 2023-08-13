calc_intervals <- function(x1, x2, intMax = 12, intMin = 4, forceZero = TRUE) {
  #' Calculate pretty intervals and include the boundaries exactly
  #'
  #' @description This function is my attempt at creating pretty intervals. It
  #'   differs from `pretty()` because it forces the min and max values to be
  #'   included in the returned vector. It still needs further testing.
  #'
  #' @param x1 The first value. If less than x2, an ascending vector is
  #'   returned; if greater than x2, a descending vector is returned.
  #' @param x2 The second value.
  #' @param intMax The maximum number of numbers in the vector.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param forceZero If the values are either side of 0, does the vector have
  #'   to include zero?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xRange <- x2 - x1
  incInt <- c()   # to hold possible intervals

  if ((x1 > 0 & x2 > 0) | c(x1 < 0 & x2 < 0)) {
    forceZero <- FALSE
    print("not forcing zero")
  }

  # Check basic divisions
  xChecks <- c(0.05, 0.1, 0.2, 0.25, 0.4, 0.5, 0.75, 1:25)

  for (ii in xChecks) {
    if (round(xRange %% ii) == 0) {  # round necessary - ?for negatives in xRange?
      incInt <- c(incInt, ii)
    }
  }

  # Check scale
  for (ii in c(-5:-1, 1:5)) {
    for (jj in incInt) {
      ij <- 10^ii * jj
      if ((xRange %% ij) == 0) {
        incInt <- c(incInt, ii)
      }
    }
  }

  print(incInt)

  # Calculate vectors
  xIntervals <- seq(x2, x1, -max(incInt)) # descending

    # but we don't only want 2 numbers, or tonnes of numbers
    tt <- 0
    while (length(xIntervals) < intMin | length(xIntervals) > intMax) {
      tt <- tt + 1
      if (tt > 20 | tt > length(incInt) + 1) {
        stop("Can't converge, try a different vector length.")
      }
      xIntervals <- seq(x2, x1, -incInt[length(incInt) - tt]) # descending
    }

    if (x1 < x2) {
      xIntervals <- rev(xIntervals)         # ascending
    }

  return(xIntervals)
}
