show_pch <- function(cex = 2, kula = "red"){
  #' Plot each of the pch symbols
  #'
  #' @description Who can ever remember the pch codes for the different point
  #'   markers? This function creates a plot showing each of them in order. (An
  #'   alternative is just to run ?pch and scroll down, but that's less fun than
  #'   creating a new function!)
  #'
  #' @param cex What size should the markers be?
  #' @param kula What colour should the markers be?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  plot_points(1:25, 1:25,
              pch = 1:25,
              main = "pch codes",
              cex = cex, col = kula)
}
