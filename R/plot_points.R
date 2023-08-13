# xx <- sample(1:100, 25)
# yy <- sample(-100:99, 25)
#
# plot_points <- function(x, y, kula = "black", cex = 1, pch = 19, lwd = 1, ...) {
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
#   points(xx, yy, col = kula, cex = cex, pch = pch, lwd = lwd)
# }
#
# plot_points(xx, yy, addOrigin = FALSE)
# plot(xx, yy)
