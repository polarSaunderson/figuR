plot_bars <- function(x, y = NULL,
                      lwd = 10,
                      barKula = "#4477AAFF", xpd = TRUE,
                      ...) {
  #' Plot a bar chart
  #'
  #'

  # Code -----------------------------------------------------------------------
  # Handle if only a single vector is provided
  if (is.null(y)) {
    y <- x
    x <- seq_along(x)
  }

  # Create xxLim & yyLim for xLimits and yLimits (both mandatory for pre_plot)
  xyLimits <- calc_plot_limits(x, y)
  xxLimits <- xyLimits$xxLim
  yyLimits <- xyLimits$yyLim

  # If provided, yLabels and xLabels need special handling BEFORE pre_plot
  dotArgs <- list(...)
  if ("yLabels" %in% names(dotArgs)) {
    yyLimits <- range(seq_along(dotArgs$yLabels))
  }
  if ("xLabels" %in% names(dotArgs)) {
    xxLimits <- range(seq_along(dotArgs$xLabels))
  }

  # Use created xxLimits and yyLimits if necessary
  defArgs <- list(yLimits = yyLimits, xLimits = xxLimits) # defaults
  dotArgs[names(defArgs)] <- defArgs

  # Account for multiple y vectors against a single x
  if (length(y) != length(x)) {
    # print("no match!")
    if (length(y) %% length(x) == 0) {
      # print("goes into it!!")
      xRep <- length(y) / length(x)
    } else {
      stop("x and y are different lengths!")
    }
  }

  # run the function
  do.call(pre_plot, dotArgs)

  # Add bars
  lines(x, y,
        typ = "h",
        lwd = lwd,
        col = barKula, xpd = TRUE, lend = 3)
}
