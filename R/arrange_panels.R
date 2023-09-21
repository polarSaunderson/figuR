arrange_panels <- function(locations,
                           useSize   = FALSE,
                           plotOrder = NULL) {
  #' Creates a layout for subplots with fine-grained control
  #'
  #' @description This function is largely a wrapper around the `layout()`
  #'   function as a I don't find the logic and syntax intuitive. Locations of
  #'   the subplots / panels can be arranged using a 100 x 100 grid; the values
  #'   from 1,1 (top-left) to 100,100 (bottom-right). Grid cell numbers should
  #'   not be repeated (unless an overlap is required, but take care with that).
  #'   There are two ways to define the locations; see the "useSize" argument
  #'   and the examples.
  #'
  #' @param locations "list": A list of vectors, with each vector containing the
  #'   locations defining a single subplot. The locations in each vector can be
  #'   ordered in two ways:
  #'   * Option 1    c(xmin, xmax, ymin, ymax)       # ymin is top, ymax bottom
  #'   * Option 2    c(xmin, ymin, width, height)    # xmin, ymin = top-left
  #' @param useSize BINARY: Do the locations contain the subplot size? The
  #'   default is FALSE, so the locations need to follow Option 1 and contain
  #'   the four vertices. If TRUE, the locations should be anchored at the
  #'   top-left, and contain a width and height.
  #' @param plotOrder vector: Which order will the subplots be created in? By
  #'   default (i.e. NULL), subplots are created in the order of the vectors in
  #'   the locations list.
  #'
  #' @examples
  #'   # These both produce the same output:
  #'   t1 <- list(c(1, 2, 1, 2),
  #'              c(50, 70, 50, 80),
  #'              c(88, 94, 89, 100))
  #'   arrange_panels(t1, useSize = FALSE) # uses the 4 coordinates
  #'   layout.show(length(t1))
  #'
  #'   t2 <- list(c(1, 1, 1, 1),
  #'              c(50, 50, 20, 30),
  #'              c(88, 89, 6, 11))
  #'   arrange_panels(t2, useSize = TRUE) # uses top-left, width & height
  #'   layout.show(length(t2))
  #'
  #'   # Demonstrating plotOrder; compare with above
  #'   arrange_panels(t1, plotOrder = c(3, 1, 2))
  #'   layout.show(length(t1))
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # We'll index into a matrix for producing the layout
  quilt <- matrix(0, nrow = 100, ncol = 100)

  if (isFALSE(useSize)) {
    for (ii in seq_along(locations)) {
      iiData   <- locations[[ii]]
      iiXmin   <- iiData[1]         # left
      iiXmax   <- iiData[2]         # right
      iiYmin   <- iiData[3]         # top
      iiYmax   <- iiData[4]         # bottom

      # Have we specified a different plotting order?
      if (!is.null(plotOrder)) {
        orda <- plotOrder[ii]
      } else {
        orda <- ii
      }

      # index into the matrix
      quilt[iiXmin:iiXmax, iiYmin:iiYmax] <- orda
    }
  } else {
    for (ii in seq_along(locations)) {
      iiData   <- locations[[ii]]
      iiXmin   <- iiData[1]          # left
      iiYmin   <- iiData[2]          # top
      iiWidth  <- iiData[3]          # width
      iiHeight <- iiData[4]          # height
      iiXmax   <- iiXmin + iiWidth   # right
      iiYmax   <- iiYmin + iiHeight  # bottom

      # Have we specified a different plotting order?
      if (!is.null(plotOrder)) {
        orda <- plotOrder[ii]
      } else {
        orda <- ii
      }

      # index into the matrix
      quilt[iiXmin:iiXmax, iiYmin:iiYmax] <- orda
    }
  }

  # Use our quilt matrix to set the layout
  quilt <- matrix(quilt, nrow = 100, ncol = 100, byrow = TRUE)
  graphics::layout(quilt)
  graphics::par(cex = 1)              # reset cex, as layout changes it!

  return(invisible(quilt))
}
