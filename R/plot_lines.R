plot_lines <- function(x, y,
                       addPoints = TRUE,
                       lineKula = "red",
                       lty = 1,
                       borderKula = NULL,
                       pointKula  = "white",
                       cex = 1.5, pch = 21,
                       lwd = 4, xpd = TRUE, ...) {

  # Create xlim and ylim - these are the only things that pre_plot must have
  xInfo <- kulaR::get_kulaInfo(x)
  xxlim <- c(xInfo$zRange[1] - xInfo$zIncrements,
             xInfo$zRange[2] + xInfo$zIncrements)
  xxlim <- xInfo$zRange

  yInfo <- kulaR::get_kulaInfo(y)
  yylim <- c(yInfo$zRange[1] - yInfo$zIncrements,
             yInfo$zRange[2] + yInfo$zIncrements)
  # yylim <- yInfo$zRange

  # Use created xlim and ylim if necessary
  defArgs <- list(ylim = yylim, xlim = xxlim)
  dotArgs <- list(...)
  defArgs[names(dotArgs)] <- dotArgs
  do.call(figuR::pre_plot, defArgs)   # run the function

  # Add points
  lines(x, y, col = lineKula,
         lwd = lwd, xpd = xpd)
  if (isTRUE(addPoints)) {
    borderKula <- domR::set_if_null(borderKula, lineKula)
    points(x, y,
           col = borderKula, bg = pointKula,
           cex = cex, pch = pch,
           xpd = xpd)
  }
}
