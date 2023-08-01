pre_plot <- function(xlim, ylim,
                     main = "",
                     mainOffset = 0.25,
                     mainCex = 1.25,
                     xLabels = NULL,
                     xSeq = NULL,
                     xAxisSide = 1,
                     yLabels = NULL,
                     ySeq = NULL,
                     yAxisSide = 2,
                     addOrigin = TRUE,
                     originLwd = 1,
                     originType = 1,
                     originKula = "#B3B3B388",
                     add121 = FALSE,
                     annotationLocation = c(0, 0),
                     annotationText = "",
                     annotationCex = 0.9,
                     mar = c(2.75, 2.75, 1.5, 1.25)) {
  # Set margins
  graphics::par(mar = mar)

  # Blank plot area
  graphics::plot(xlim, ylim,
                 type = "n",
                 xlim = xlim, ylim = ylim,
                 xaxs = "i", yaxs = "i",
                 axes = FALSE,
                 main = NA,
                 ylab = "", xlab = "")

  graphics::mtext(main, side = 3, line = mainOffset, cex = mainCex)

  # x-axis ---------------------------------------------------------------------
  # Labels
  if (is.null(xLabels)) {
    if (is.null(xSeq)) xSeq <- 1                       # default
    if (xlim[1] - xlim[2] < 0) xSeq <-  sqrt(xSeq^2)   # ascending
    if (xlim[1] - xlim[2] > 0) xSeq <- -sqrt(xSeq^2)   # descending
    xLabels <- seq(xlim[1], xlim[2], xSeq)             # new labels
    if (sum(xlim %notIn% xLabels) != 0) {
      stop("Choose a nicer interval! ",
          xSeq, " doesn't go into ", xlim[1], ":", xlim[2])
    }
  }

  # Add to the plot
  xTickLocations <- add_axis(axis     = xAxisSide,
                             labels   = xLabels)

  # y-axis ---------------------------------------------------------------------
  # Labels
  if (is.null(yLabels)) {
    if (is.null(ySeq)) ySeq <- 1                       # default
    if (ylim[1] - ylim[2] < 0) ySeq <-  sqrt(ySeq^2)   # ascending
    if (ylim[1] - ylim[2] > 0) ySeq <- -sqrt(ySeq^2)   # descending
    yLabels <- seq(ylim[1], ylim[2], ySeq)             # new labels
    if (sum(ylim %notIn% yLabels) != 0) {
      stop("Choose a nicer interval! ",
          ySeq, " doesn't go into ", ylim[1], ":", ylim[2])
    }
  }

  # Add to the plot
  yTickLocations <- add_axis(axis     = yAxisSide,
                             labels   = yLabels)

  # Additional Decoration ------------------------------------------------------
  # Add origin
  if (!isFALSE(addOrigin)) {
    if (isTRUE(addOrigin)) {
      addOrigin <- c(0, 0)
    }
    graphics::abline(h = addOrigin[1], v = addOrigin[2],
                     col = originKula, lwd = originLwd, lty = originType)
  }

  # Add line with a gradient of 1 (i.e. x:y = 1:1 (1 to 1) (1 2 1))
  if (isTRUE(add121)) {
    graphics::abline(c(0, 1), lty = 2, col = "#EE6677FF")
  }

  # Add a subplot number
  graphics::text(x = annotationLocation[1],
                 y = annotationLocation[2],
                 annotationText,
                 cex = annotationCex)

  # Add plot frame
  add_plot_frame()

  return(invisible(list("xTicks" = xTickLocations,
                        "yTicks" = yTickLocations)))
}
