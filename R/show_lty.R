show_lty <- function(lwd = 1, kula = "black"){
  #' Plot each of the lty line styles
  #'
  #' @description Who can ever remember the lty codes for the different line
  #'   styles. This create a plot showing each of them in order.
  #'
  #' @param lwd numeric: How thick should the lines be?
  #' @param kula What colours should the lines be?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  pre_plot(c(0,7), c(0, 7), yGridLwd = 0, xGridLwd = 0, main = "lty Types")

  for (ii in 1:6) {
    graphics::abline(h = ii, lty = ii, lwd = lwd, col = kula)
  }
}
