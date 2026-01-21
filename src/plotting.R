# Helper for plotting VO2 ml/min with error bars
plot_vo2_ml_min <- function(x_vals, y_est, y_lower, y_upper, xlab, ylab, x_axis_labels = NULL, col = NULL, pch = 16) {
  par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
  plot(
    x = x_vals, y = y_est, ylim = range(y_lower, y_upper),
    xlab = xlab, ylab = ylab, xaxt = if (is.null(x_axis_labels)) "s" else "n", pch = pch, col = col
  )
  if (!is.null(x_axis_labels)) {
    axis(1, at = x_vals, labels = x_axis_labels)
  }
  arrows(
    x0 = x_vals, y0 = y_lower, x1 = x_vals, y1 = y_upper,
    angle = 90, code = 3, length = 0.07, col = col
  )
}
