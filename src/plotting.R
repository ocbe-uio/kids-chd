# Generic helper for plotting group-based metrics (e.g., VO2, VO2/kg)
plot_metric_by_group <- function(
  height, bmi, metric_fun, ylab, main,
  groups = c("simple", "moderate", "fontan"),
  group_labels = c("Simple", "Moderate", "Fontan")
  ) {
  y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(groups))
  for (sex in 0:1) {
    for (i in seq_along(groups)) {
      g <- get(groups[i])
      p <- person(sex = sex, height = height, bmi = bmi)
      vals <- metric_fun(g, p)[1, ]
      y_est[sex+1, i] <- vals[1]
      y_lower[sex+1, i] <- vals[2]
      y_upper[sex+1, i] <- vals[3]
    }
  }
  par(bg = panel_bg)
  matplot(1:length(groups), t(y_est), type = "p", pch = 16, lty = 1, col = strong_col,
          xlab = "Diagnostic group", ylab = ylab, xaxt = "n", ylim = range(y_lower, y_upper, na.rm = TRUE),
          main = main)
  axis(1, at = 1:length(groups), labels = group_labels)
  for (sex in 0:1) {
    arrows(1:length(groups), y_lower[sex+1,], 1:length(groups), y_upper[sex+1,], angle = 90, code = 3, length = 0.07, col = strong_col[sex+1], lwd = 1)
    points(1:length(groups), y_est[sex+1,], pch = 16, col = strong_col[sex+1], cex = 1)
  }
  legend("topright", legend = c("Boy", "Girl", "",
                                 sprintf("Height: %.0f cm", height),
                                 sprintf("BMI: %.1f kg/m²", bmi)),
         col = c(rev(strong_col), NA, NA, NA), pch = c(16, 16, NA, NA, NA), lty = c(1, 1, NA, NA, NA))
}
color_boy_faded <- rgb(0.2, 0.6, 0.9, 0.4)
color_boy_strong <- rgb(0.2, 0.6, 0.9, 1)
color_girl_faded <- rgb(1, 0.3, 0.6, 0.4)
color_girl_strong <- rgb(1, 0.3, 0.6, 1)
strong_col <- c(color_girl_strong, color_boy_strong)
panel_bg <- rgb(0.9607843, 0.9607843, 0.9607843)

# Helper for plotting VO2 ml/min with error bars
plot_vo2_ml_min <- function(x_vals, y_est, y_lower, y_upper, xlab, ylab, x_axis_labels = NULL, col = NULL, pch = 16) {
  par(bg = panel_bg)
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
