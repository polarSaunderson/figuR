show_pch <- function(){
  #' Plot each of the pch symbols
  #'
  #' @description Who can ever remember the pch codes for the different point
  #'   markers? This function creates a plot showing each of them in order.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  pre_plot(c(0, 25), c(0, 25), main = "pch codes")

  # Add the points
  graphics::points(1:25, 1:25, pch = c(1:25), cex = 2, xpd = TRUE)
}
