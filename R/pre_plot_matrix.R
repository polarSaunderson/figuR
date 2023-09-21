pre_plot_matrix <- function(x, ...) {
  #' Prepares to image-plot a matrix
  #'
  #' @description When using [pre_plot], it is not possible to subsequently plot
  #'   a matrix using  `image(x, add = TRUE)` because the limits don't line up.
  #'   However, it appears (at least so far) that the extension to each side is
  #'   predictable, and depends on the number of rows and cols. This function
  #'   accounts for this, and thus allows a matrix to be image plotted onto a
  #'   customisable frame from [pre_plot()].
  #'
  #'   This function is mainly intended to be used by [plot_matrix()].
  #'
  #' @param x matrix: The data that will be added afterwards.
  #' @param ... Any arguments (except 'xLimits' and 'yLimits') that can be added
  #'   to [pre_plot()].
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Everything depends on the number of rows and columns
  cols <- ncol(x)
  rows <- nrow(x)

  # Account for a single column or row
  if (rows == 1) {
    rows <- 2
  }
  if (cols == 1) {
    cols <- 2
  }

  # Find row extremities (necessary for x-axis)
  rr <- 1/(rows - 1)
  r1 <- 0 - rr/2
  r2 <- 1 + rr/2

  # Find col extremities (necessary for y-axis)
  cc <- 1/(cols - 1)
  c1 <- 0 - cc/2
  c2 <- 1 + cc/2

  r1 <- round(r1, 4)
  r2 <- round(r2, 4)
  c1 <- round(c1, 4)
  c2 <- round(c2, 4)

  tickLocations <- pre_plot(xLimits = c(r1, r2),
                            yLimits = c(c1, c2),
                            # xMeshlines = (cols * 2) + 1,
                            # yMeshlines = (rows * 2) + 1,
                            # yAlignMidPoints = FALSE,
                            # labelFirst = 1,
                            # labelEvery = 4,
                            # tickEvery = 1,
                            addOrigin = FALSE, ...)
  return(invisible(tickLocations))
}
