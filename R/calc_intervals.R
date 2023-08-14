calc_intervals <- function(v1, v2,
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
  #' @param v1 The first value. If less than v2, an ascending vector is
  #'   returned; if greater than v2, a descending vector is returned. If a
  #'   vector of length two is used, the second value will be used as v2. If a
  #'   vector of length > 2 is used, a range will be calculated and an ascending
  #'   vector returned.
  #' @param v2 The second value.
  #' @param intMin The minimum number of numbers in the vector.
  #' @param intMax The maximum number of numbers in the vector; if 'preferError'
  #'   is FALSE, this argument is reset to 100; overruled (essentially reset to
  #'   100) and becomes more of a guideline for deeming whether the initial
  #'   intervals are suitable.
  #' @param forceZero Does zero have to be included in the vectors? By default,
  #'   if v1 and v2 fall either side of 0, this will be TRUE; if both v1 and v2
  #'   have the same sign, it will be FALSE. These defaults can be overwritten.
  #' @param preferError If a suitable interval cannot be found with the existing
  #'   'intMax', is it preferable to throw an error (TRUE), or ignore the
  #'   'intMax' argument (FALSE; default) and reset it to 100? If FALSE, and it
  #'   has still not converged, `pretty` limits will be returned, which are not
  #'   guaranteed to include the exact boundaries.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (length(v1) == 2) {
    v2 <- v1[2]
    v1 <- v1[1]
  } else if (length(v1) > 2) {
    v2 <- max(v1)
    v1 <- min(v1)
  }

  vPair  <- c(v1, v2)
  # cat("\n Min & max:", vPair)
  vRange <- v2 - v1
  v1Prec <- domR::count_decimal_places(v1)
  v2Prec <- domR::count_decimal_places(v2)
  vvPrec <- max(v2Prec, v1Prec)
  if (vvPrec > 5) {
    warning("Data is too precise, so ignoring the exact boundaries & using pretty intervals\n")
    # print("going pretty")
    vIntervals <- pretty(vPair)
    usesPretty <- TRUE
  } else {
    # Should zero be forced into the line?
    if ((v1 < 0 & v2 > 0) | c(v1 > 0 & v2 < 0)) {   # if diverging, use zero
      forceZero <- set_if_null(forceZero, TRUE)
      # cat("\nforcing Zero\n")
    } else {
      forceZero <- set_if_null(forceZero, FALSE)     # if same sign, no zero
      # cat("\n not forcing Zero\n")
    }

    # Preallocate
    incInt <- c()   # to hold possible intervals

    # Check basic divisions
    vChecks <- c(0.05, 0.1, 0.2, 0.25, 0.4, 0.5, 0.75, 1:25)
    vChecks <- c(c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5,
                   3, 4, 5, 6, 7, 7.5, 8, 9) * (10^(-(max(vvPrec) + 1))),
                 1:25)
    # print(vChecks)
    for (ii in vChecks) {
      # print_line()
      # cat("_v1:_", v1, "\n")
      # cat("ii: ", ii, "remainder:", round((v1 %% ii), v1Prec))
      if (round((v1 %% ii), v1Prec) == 0) {
        # cat("\n_v2:_", v2, "\n")
        # cat("\n", ii, "remainder:", round((v2 %% ii), v2Prec))
        if (round((v2 %% ii), v2Prec) == 0) {
          # cat("\n in here:", ii)
          incInt <- c(incInt, ii)
        }
      }
    }

    if (length(incInt) == 0) {
      warning("Nothing goes into these numbers precisely; using pretty intervals\n")
      vIntervals <- pretty(vPair)
      usesPretty <- TRUE
    } else {

    # Check scale
    for (ii in c(-vvPrec:-1, 1:vvPrec)) {
      for (jj in incInt) {
        ij <- 10^ii * jj
        if ((vRange %% ij) == 0) {
          incInt <- c(incInt, ij)
        }
      }
    }


    # Figuring out the vector interval
    acceptInterval <- FALSE
    incInt <- unique(incInt) |> sort(TRUE)#; print(incInt) # descending
    ii <- 1

    # print_line("(3)")
    # print(incInt)
    # print_line("(3)")

    # Check whether the intervals are acceptable, and try alternatives if not
    while (isFALSE(acceptInterval)) {
      # cat("\n", ii)
      vIntervals <- seq(max(vPair), min(vPair), -incInt[ii])  # create the vector
      vLength <- length(vIntervals)                           # length
      # cat("Interval of:", incInt[ii],
          # "and a length of:", vLength, "\n")
      if (vLength < intMin | vLength > intMax) {
        # cat("wrong length\n")
        acceptInterval <- FALSE
        ii <- ii + 1
      } else if (isTRUE(forceZero) & (as.character(0) %notIn% as.character(vIntervals))) {
        # cat("no zero\n")
        # print(vIntervals)
        acceptInterval <- FALSE
        ii <- ii + 1
      } else if (as.character(v1) %notIn% as.character(round(vIntervals, v1Prec)) |
                 as.character(v2) %notIn% as.character(round(vIntervals, v2Prec))) {
  #      uses as.character because numeric are difficult to match
        # cat("excludes limits:", c(v1, v2), "not in", vIntervals, "\n")
        acceptInterval <- FALSE
        ii <- ii + 1
      } else {
        # cat("The solution is:", vIntervals, "\n\n")
        usesPretty <- FALSE
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
          if (intMax > 100) {
            warning("Ignoring the exact boundaries & using pretty intervals\n")
            vIntervals <- pretty(vPair)
            usesPretty <- TRUE
            acceptInterval <- TRUE
          }
        }
      }
    }
    }
}

  # Which direction should the interval go?
  if (v1 < v2) {
    if (intMax > 100) {
      vIntervals <- vIntervals    # calculated with pretty, so already ascending
    } else {
      vIntervals <- rev(vIntervals) # change from descending to ascending
    }
  }

  return(list("vIntervals" = vIntervals,
              "vRange"     = range(vIntervals),
              "vSeq"       = vIntervals[2] - vIntervals[1],
              "usesPretty" = usesPretty))
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
