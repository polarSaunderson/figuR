add_plot_frame <- function(sides = c(1:4), kula = "black") {
  #' Add a frame around a plot
  #'
  #' @param sides vector: Which sides should the frame be added to? Bottom (1),
  #'   left (2), top (3), and / or right (4). Default is to plot all four.
  #' @param kula What colour should the frame be?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  for (ii in sides) {
    add_axis(ii, labels = rep("", 2), tickLength = 0,
             axisKula = kula, gridLwd = 0)
  }
}
