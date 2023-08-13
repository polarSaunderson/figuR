plot_points <- function(x, y,
                        borderKula = "red",
                        pointKula  = "white",
                        cex = 1, pch = 21,
                        lwd = 2, xpd = TRUE, ...) {


  # Create xlim and ylim - these are the only things that pre_plot must have
  xInfo <- kulaR::get_kulaInfo(x)
  xxlim <- c(xInfo$zRange[1] - xInfo$zIncrements,
             xInfo$zRange[2] + xInfo$zIncrements)
  # xxlim <- xInfo$zRange

  yInfo <- kulaR::get_kulaInfo(y)
  yylim <- c(yInfo$zRange[1] - yInfo$zIncrements,
             yInfo$zRange[2] + yInfo$zIncrements)

  # Use created xlim and ylim if necessary
  defArgs <- list(ylim = yylim, xlim = xxlim)
  dotArgs <- list(...)
  defArgs[names(dotArgs)] <- dotArgs
  do.call(figuR::pre_plot, defArgs)   # run the function

  # Add points
  points(x, y,
         col = borderKula, bg = pointKula,
         cex = cex, pch = pch,
         lwd = lwd, xpd = xpd)
}




# xx <- sample(1:100, 25)
# yy <- sample(-100:99, 25)
# #
# plot_points <- function(x, y,
#                         borderKula = "red",
#                         pointKula  = "white",
#                         cex = 1, pch = 21,
#                         lwd = 1, ...) {
#   xInfo <- kulaR::get_kulaInfo(xx)
#   xlim <- c(xInfo$zRange[1] - xInfo$zIncrements,
#             xInfo$zRange[2] + xInfo$zIncrements)
#
#   yInfo <- kulaR::get_kulaInfo(yy)
#   ylim <- c(yInfo$zRange[1] - yInfo$zIncrements,
#             yInfo$zRange[2] + yInfo$zIncrements)
#
#   pre_plot(xlim, ylim,
#            xTickSeq = xInfo$zIncrements,
#            xTickFirst = 2, xTickEvery = 2,
#            xGridFirst = 2, xGridEvery = 2,
#            xLabelFirst = 2, xLabelEvery = 2,
#            yTickSeq = yInfo$zIncrements,
#            yTickFirst = 2, yTickEvery = 2,
#            yGridFirst = 2,  yGridEvery = 2,
#            yLabelFirst = 2, yLabelEvery = 2, ...)
#
#   points(xx, yy,
#          col = borderKula, bg = pointKula,
#          cex = cex, pch = pch,
#          lwd = lwd, xpd = TRUE)
# }
#
# plot_points(xx, yy, addOrigin = FALSE)
# plot(xx, yy)
