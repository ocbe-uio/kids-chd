## Generic helper for plotting a metric by height (for both sexes, with CIs)
plot_metric_by_height <- function(
  group, bmi, metric_fun, ylab, main,
  heights = 100:210,
  group_label = NULL,
  show_ci = TRUE,
  legend_pos = "topleft"
) {
  y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(heights))
  for (sex in 0:1) {
    for (i in seq_along(heights)) {
      p <- person(sex = sex, height = heights[i], bmi = bmi)
      res <- metric_fun(group, p)
      y_est[sex+1, i] <- res[1, 1]
      if (show_ci) {
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
  }
  faded_col <- c(color_girl_faded, color_boy_faded)
  par(bg = panel_bg)
  if (show_ci) {
    ylim <- range(y_lower, y_upper, na.rm = TRUE)
  } else {
    ylim <- range(y_est, na.rm = TRUE)
  }
  matplot(heights, t(y_est), type = "n", lty = 1, col = strong_col,
          xlab = "Height (cm)", ylab = ylab, ylim = ylim,
          main = main)
  if (show_ci) {
    for (sex in 0:1) {
      polygon(c(heights, rev(heights)),
              c(y_lower[sex+1,], rev(y_upper[sex+1,])),
              col = faded_col[sex+1], border = NA)
    }
  }
  matlines(heights, t(y_est), lty = 1, col = strong_col, lwd = 2)
  if (show_ci) {
    matlines(heights, t(y_lower), lty = 2, col = strong_col)
    matlines(heights, t(y_upper), lty = 2, col = strong_col)
  }
  legend_labels <- c("Boy (estimate)", "Boy (CI)", "Girl (estimate)", "Girl (CI)", "")
  legend_cols <- c(strong_col[2], strong_col[2], strong_col[1], strong_col[1], NA)
  legend_lty <- c(1, 2, 1, 2, NA)
  legend_lwd <- rep(2, 5)
  legend_extra <- c()
  if (!is.null(group_label)) legend_extra <- c(sprintf("Group: %s", group_label))
  legend_extra <- c(legend_extra, sprintf("BMI: %.1f kg/m²", bmi))
  legend(
    legend_pos,
    legend = c(legend_labels, legend_extra),
    col = c(legend_cols, rep(NA, length(legend_extra))),
    lty = c(legend_lty, rep(NA, length(legend_extra))),
    lwd = c(legend_lwd, rep(NA, length(legend_extra))),
    seg.len = 3
  )
}
# Generic helper for plotting group-based metrics (e.g., VO2, VO2/kg)
plot_metric_by_group <- function(
  height, bmi, metric_fun, ylab, main,
  groups = c("simple", "moderate", "fontan"),
  group_labels = c("Simple", "Moderate", "Fontan"),
  show_ci = TRUE,
  legend_pos = "topright"
  ) {
  y_est <- matrix(NA, nrow = 2, ncol = length(groups))
  y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(groups))
  for (sex in 0:1) {
    for (i in seq_along(groups)) {
      g <- get(groups[i])
      p <- person(sex = sex, height = height, bmi = bmi)
      vals <- metric_fun(g, p)[1, ]
      y_est[sex+1, i] <- vals[1]
      y_range <- range(y_est, na.rm = TRUE)
      if (show_ci) {
        y_lower[sex+1, i] <- vals[2]
        y_upper[sex+1, i] <- vals[3]
        y_range <- range(y_lower, y_upper, na.rm = TRUE)
      }
    }
  }
  par(bg = panel_bg)
  matplot(
    1:length(groups), t(y_est), type = "p", pch = 16, lty = 1, col = strong_col,
    xlab = "Diagnostic group", ylab = ylab, xaxt = "n", ylim = y_range,
    main = main
  )
  axis(1, at = 1:length(groups), labels = group_labels)
  for (sex in 0:1) {
    points(
      1:length(groups), y_est[sex+1,], pch = 16, col = strong_col[sex+1],
      cex = 1
    )
  }
  legend(
    legend_pos,
    legend = c(
      "Boy", "Girl", "",
      sprintf("Height: %.0f cm", height), sprintf("BMI: %.1f kg/m²", bmi)
    ),
    col = c(rev(strong_col), NA, NA, NA),
    pch = c(16, 16, NA, NA, NA),
    lty = c(1, 1, NA, NA, NA)
  )
  if (show_ci) {
    for (sex in 0:1) {
      arrows(
        1:length(groups), y_lower[sex+1,], 1:length(groups), y_upper[sex+1,],
        angle = 90, code = 3, length = 0.07, col = strong_col[sex+1], lwd = 1
      )
    }
  }
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
