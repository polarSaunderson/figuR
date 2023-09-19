add_plot_frame <- function(sides = c(1:4),
                           kula = "black",
                           type = 1,
                           lwd = 1) {
  #' Add a frame around a plot
  #'
  #' @param sides vector: Which sides should the frame be added to? Bottom (1),
  #'   left (2), top (3), and / or right (4). Default is to plot all four.
  #' @param kula What colour should the frame be?
  #' @param type What type of line should the frame be?
  #' @param lwd How thick should the frame be?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  for (ii in sides) {
    add_axis(ii, labels = rep("", 2), tickLength = 0,
             gridLwd = 0, gridKula = "#00000000",    # hide them
             axisKula = kula, axisType = type, axisLwd = lwd)
  }
}
