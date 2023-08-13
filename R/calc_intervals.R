calc_intervals <- function(x1, x2, forceZero = TRUE,
                           intMin = 4, intMax = 12,
                           preferError = FALSE) {
  #' Calculate pretty intervals and include the boundaries exactly
  #'
  #' @description This function is my attempt at creating pretty intervals. It
  #'   differs from `pretty()` because it forces the min and max values to be
  #'   included in the returned vector. It still needs further testing.
  #'
  #' @param x1 The first value. If less than x2, an ascending vector is
  #'   returned; if greater than x2, a descending vector is returned.
  #' @param x2 The second value.
  #' @param forceZero Does zero have to be included in the vectors? By default,
  #'   if x1 and x2 fall either side of 0, this will be TRUE, and if both x1 and
  #'   x2 have the same sign, it will be FALSE. Can be overwritten.
  #' @param intMax The maximum number of numbers in the vector.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param preferError If a suitable interval cannot be found with the existing
  #'   'intMax', is it preferable to throw an error (TRUE), or ignore the
  #'   'intMax' argument (FALSE; default)?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xRange <- x2 - x1
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
        incInt <- c(incInt, ii)
      }
    }
  }


  acceptInterval <- FALSE
  incInt <- unique(incInt) |> sort(TRUE) # descending
  print(incInt)
  print_line()
  ii <- 1

  # Initial interval guess
  xIntervals <- seq(x2, x1, -incInt[ii]) # descending

  # Check whether these intervals are acceptable, and try alternatives if not
  while (isFALSE(acceptInterval)) {
    xIntervals <- seq(x2, x1, -incInt[ii])
    xLength <- length(xIntervals)
    cat("Interval of:", incInt[ii], "and a length of:", xLength, "\n")
    if (xLength < intMin | xLength > intMax) {
      cat("wrong length\n")
      acceptInterval <- FALSE
      ii <- ii + 1
    } else if (isTRUE(forceZero) & (0 %notIn% xIntervals)) {
      cat("no zero\n")
      print(xIntervals)
      acceptInterval <- FALSE
      ii <- ii + 1
    } else if (sum(c(x1, x2) %notIn% xIntervals) != 0) {
      cat("misses limits:", xIntervals)
      acceptInterval <- FALSE
      ii <- ii + 1
    } else {
      cat(". The solution is:", xIntervals)
      acceptInterval <- TRUE
    }

    print_line()
    # if (ii > length(incInt)) {stop("No matches found!")}
    # if (ii > length(incInt)) {
    #   print("resetting ii")
    #   ii <- 1
    #   if (isTRUE(forceZero)) {
    #     print("abandoning zero requirement")
    #     forceZero <- FALSE
    #   } else {
    #     print("abandoning max interval count")
    #     intMax <- intMax * 2
    #   }
    # }

    if (ii > length(incInt)) {
      if (isTRUE(preferError)) {
        stop("No suitable interval found.")
      } else {
        print_line("@_")
        ii <- 1
        print("abandoning max interval count & trying again")
        intMax <- intMax * 2
      }
    }
  }

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
