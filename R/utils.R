# Useful function here
# These are all taken from domR, v0.0.1.10

##
`%notIn%` <- function(x, y) {    # copied from domR so no need to install it
  # Code -----------------------------------------------------------------------
  match(x, y, nomatch = 0) == 0
}

##
set_if_null <- function(x, defaultValue) {
  # Code -----------------------------------------------------------------------
    if (is.null(x)) x <- defaultValue
    return(x)
}

##
count_decimal_places <- function(x) {
  # Code -----------------------------------------------------------------------
  # Preallocate
  xDP <- integer(length(x))
  machPrec <- .Machine$double.eps^0.5 # computer precision is min. diff possible

  # Loop through values
  for (ii in x) {
    iiIndex <- which(x == ii)
    if (!is.na(ii)) {
      if (abs(ii - round(ii)) > machPrec) { # diff is enough for computer to see
        # convert to text to count
        iiSplit <-  strsplit(sub('0+$', '',
                                 format(ii, digits = 15,
                                        scientific = FALSE)), # not Xe+Y
                             ".", fixed = TRUE)[[1]]
        if (length(iiSplit) == 1) {       # if nothing after decimal
          xDP[iiIndex] <- 0
        } else {                          # get part after decimal place
          xDP[iiIndex] <- nchar(iiSplit[[2]])
        }
      } else {
        xDP[iiIndex] <- 0
      }
    } else {
      xDP[iiIndex] <- NaN
    }
  }
  return(xDP)
}
