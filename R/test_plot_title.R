test_plot_title <- function(text) {
  #' Test how text displays on a plot
  #'
  #' @description Using functionality like bquote never really makes sense.
  #'   Often we only figure that out after running slow code, and getting back a
  #'   plot with an incorrect title (or just an error). This function simply
  #'   creates a basic plot, and shows how the text is displayed.
  #'
  #' @param text "string": The plot title to test
  #'
  #' @examples
  #' \dontrun{
  #'   test_plot_title(bquote("W"~m^2~ "(magic)"))
  #' }
  #' @export

  # Code -----------------------------------------------------------------------
  graphics::par(mar = c(5, 5, 5, 5))
  graphics::plot(1:10,
       main = text, xlab = text, ylab = text, sub = text)
}
