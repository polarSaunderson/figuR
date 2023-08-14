calc_intervals <- function(x1, x2,
                           intMin = 5, intMax = 11,
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
  #' @param x1 The first value. If less than x2, an ascending vector is
  #'   returned; if greater than x2, a descending vector is returned. If a
  #'   vector of length two is used, the second value will be used as x2. If a
  #'   vector of length > 2 is used, a range will be calculated and an ascending
  #'   vector returned.
  #' @param x2 The second value.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param intMax The maximum number of numbers in the vector; if 'preferError'
  #'   is FALSE, this argument is overruled (essentially reset to 100) and
  #'   becomes more of a guideline for deeming whether the initial intervals are
  #'   suitable.
  #' @param forceZero Does zero have to be included in the vectors? By default,
  #'   if x1 and x2 fall either side of 0, this will be TRUE; if both x1 and x2
  #'   have the same sign, it will be FALSE. These defaults can be overwritten.
  #' @param preferError If a suitable interval cannot be found with the existing
  #'   'intMax', is it preferable to throw an error (TRUE), or ignore the
  #'   'intMax' argument (FALSE; default)?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (length(x1) == 2) {
    x2 <- x1[2]
    x1 <- x1[1]
  } else if (length(x1) > 2) {
    x2 <- max(x1)
    x1 <- min(x1)
  }

  xPair  <- c(x1, x2)
  xRange <- x2 - x1
  x1Prec <- domR::count_decimal_places(x1)
  x2Prec <- domR::count_decimal_places(x2)

  # Preallocate
  incInt <- c()   # to hold possible intervals

  # Should zero be forced into the line?
  if ((x1 < 0 & x2 > 0) | c(x1 > 0 & x2 < 0)) {   # if diverging, use zero
   forceZero <- set_if_null(forceZero, TRUE)
  } else {
   forceZero <- set_if_null(forceZero, FALSE)     # if same sign, no zero
  }

  # Check basic divisions
  xChecks <- c(0.05, 0.1, 0.2, 0.25, 0.4, 0.5, 0.75, 1:25)

  for (ii in xChecks) {
    if (round(xRange %% ii) == 0) { # round necessary - ?for negatives in xRange?
      incInt <- c(incInt, ii)
    }
  }

  # Check scale
  for (ii in c(-5:-1, 1:5)) {
    for (jj in incInt) {
      ij <- 10^ii * jj
      if ((xRange %% ij) == 0) {
        incInt <- c(incInt, ij)
      }
    }
  }

  # Figuring out the vector interval
  acceptInterval <- FALSE
  incInt <- unique(incInt) |> sort(TRUE); # print(incInt) # descending
  ii <- 1

  # Check whether the intervals are acceptable, and try alternatives if not
  while (isFALSE(acceptInterval)) {
    # cat(ii)
    xIntervals <- seq(max(xPair), min(xPair), -incInt[ii])    # create the vector
    xLength <- length(xIntervals)                             # length
    # cat("Interval of:", incInt[ii],
    # "and a length of:", xLength, "\n")
    if (xLength < intMin | xLength > intMax) {
      # cat("wrong length\n")
      acceptInterval <- FALSE
      ii <- ii + 1
    } else if (isTRUE(forceZero) & (as.character(0) %notIn% as.character(xIntervals))) {
      # cat("no zero\n")
      # print(xIntervals)
      acceptInterval <- FALSE
      ii <- ii + 1
    } else if (as.character(x1) %notIn% as.character(round(xIntervals, x1Prec)) |
               as.character(x2) %notIn% as.character(round(xIntervals, x2Prec))) {
      # uses as.character because numeric are difficult to match
      # cat("excludes limits:", c(x1, x2), "not in", xIntervals, "\n")
      acceptInterval <- FALSE
      ii <- ii + 1
    } else {
      # cat("The solution is:", xIntervals, "\n\n")
      acceptInterval <- TRUE
    }

    # If nothing is found, can we ignore the maxInt count? Or throw an error?
    if (ii > length(incInt)) {
      if (isTRUE(preferError)) {
        stop("No suitable interval found.")
      } else {
        ii <- 1
        cat("\n")
        warning("Ignoring the maximum interval count & trying again\n")
        # print("abandoning max interval count & trying again")
        intMax <- intMax * 2
        if (intMax > 100) {preferError <- TRUE}
      }
    }
  }

  # Which direction should the interval go?
  if (x1 < x2) {
    xIntervals <- rev(xIntervals)         # ascending
  }

  return(xIntervals)
}


  # # Calculate vectors
  # xIntervals <- seq(x2, x1, -max(incInt)) # descending
  #
  #   # but we don't only want 2 numbers, or tonnes of numbers
  #   tt <- 0
  #   while (length(xIntervals) < intMin | length(xIntervals) > intMax) {
  #     tt <- tt + 1
  #     if (tt > 20 | tt > length(incInt) + 1) {
  #       stop("Can't converge, try a different vector length.")
  #     }
  #     xIntervals <- seq(x2, x1, -incInt[length(incInt) - tt]) # descending
  #   }





## SCRAPS ##
# ## ```{r}
# devtools::load_all("../../domiProjects/Coding/domR_Packages/figuR/")
#
# # print(calc_intervals(2, 5))
# # print_line()
# # print(calc_intervals(-2.5, 5.5, forceZero = FALSE))
# # print_line()
# print(calc_intervals(-2.5, 5.5, intMax = 20))
# ```
#
# ```{r}
# length(seq(-2.5, 5.5, 0.5))
# ```
#
#
#
# ```{r}
# tt <- 1
# while (tt < 5) {
#   tt <- tt + 1
#   if (tt == 4) {
#     next
#   }
#   print(tt)
# }
# ```
#
# ```{r}
# x1 <- 20
# x2 <- 170
#
# xx <- c(x1, x2)
# xD <- x2 - x1
#
# incX <- c()
# for (ii in 1:10) {
#   # cat(ii, xD %% ii, "\n")
#   if ((xD %% ii) == 0) {
#     incX <- c(incX, ii)
#   }
# }
#
# for (ii in 1:5) {
#   for (jj in incX) {
#     ij <- 10^ii * jj
#     # cat(ij, xD %% ij, "\n")
#     if ((xD %% ij) == 0) {
#       incX <- c(incX, ij)
#     }
#   }
# }
#
#
# print_line("@")
# max(incX)
# print(incX)
#
# seq(x1, x2, max(incX))
#
# ```
#
# ```{r}
# seq(-3, 10, 2)
# ```
#
#
# ```{r}
# x1 <- 2
# x2 <- 17
#
# xx <- c(x1, x2)
#
# xP <- pretty(xx)
#
#   print(xP)
# if (sum(xx %notIn% xP) != 0) {
#   print("pretty doesn't include limits")
# }else {
#   stop()
# }
#
# xP <- pretty(xx, n = length(xP) * 2)
#   print(xP)
# if (sum(xx %notIn% xP) != 0) {
#   print("pretty doesn't include limits")
# } else {
#   stop()
# }
#
# xP <- pretty(xx, n = length(xP) * 2.25, )
#   print(xP)
# if (sum(xx %notIn% xP) != 0) {
#   print("pretty doesn't include limits")
# }else {
#   stop()
# }
#
# ```
#
#
# ```{r}
# xx
# pretty(xx, bound = TRUE)
# ```
#
#
