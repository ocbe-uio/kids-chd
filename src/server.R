source("classes.R")
source("methods.R")
source("models.R")
source("plotting.R")

server <- function(input, output) {
  output$results_table <- renderTable({
    group <- get(input$group)
    # Calculate for both sexes: 1 = Boy, 0 = Girl
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)

    # Helper to get results for both sexes
    get_metric <- function(fun) {
      c(male = fun(group, person_male), female = fun(group, person_female))
    }

    # For metrics with confidence intervals (assume matrix with 3 columns)
    get_metric_ci <- function(fun) {
      male <- fun(group, person_male)
      female <- fun(group, person_female)
      list(male = male, female = female)
    }

    vo2_ml_min_results <- get_metric_ci(group$vo2_ml_min)
    vo2_ml_kg_min_results <- get_metric_ci(group$vo2_ml_kg_min)
    heart_rate_results <- get_metric_ci(group$heart_rate)
    ventilation_results <- get_metric_ci(group$ventilation)
    oxygen_pulse_results <- get_metric_ci(group$oxygen_pulse)
    ve_vco2_slope_results <- get_metric_ci(group$ve_vco2_slope)
    breathing_frequency_results <- get_metric_ci(group$breathing_frequency)

    metrics <- c(
      "VO2 ml/min (VO₂)", "VO2 ml/kg/min (VO₂/kg)", "Heart rate (HR)", "Ventilation (VE)",
      "Oxygen pulse (O₂ pulse)", "VE/VCO2 slope (VE/VCO₂)", "Breathing frequency (fR)"
    )

    # Format values with 2 decimals for proper decimal alignment when right-aligned
    value_male <- c(
      sprintf("%.2f", vo2_ml_min_results$male[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$male[1, 1]),
      sprintf("%.2f", heart_rate_results$male[1, 1]),
      sprintf("%.2f", ventilation_results$male[1, 1]),
      sprintf("%.2f", oxygen_pulse_results$male[1, 1]),
      sprintf("%.2f", ve_vco2_slope_results$male[1, 1]),
      sprintf("%.2f", breathing_frequency_results$male[1, 1])
    )
    value_female <- c(
      sprintf("%.2f", vo2_ml_min_results$female[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$female[1, 1]),
      sprintf("%.2f", heart_rate_results$female[1, 1]),
      sprintf("%.2f", ventilation_results$female[1, 1]),
      sprintf("%.2f", oxygen_pulse_results$female[1, 1]),
      sprintf("%.2f", ve_vco2_slope_results$female[1, 1]),
      sprintf("%.2f", breathing_frequency_results$female[1, 1])
    )
    # Format confidence intervals with appropriate precision
    format_ci <- function(mat, digits = 2) {
      if (is.null(mat) || any(is.na(mat[1, 2:3]))) {
      return(NA)
      }
      paste(
      formatC(mat[1, 2], width = 6, format = "f", digits = digits, flag = "#"),
      "-",
      formatC(mat[1, 3], width = 6, format = "f", digits = digits, flag = "#")
      )
    }

    ci_male <- c(
      format_ci(vo2_ml_min_results$male, digits = 2),
      format_ci(vo2_ml_kg_min_results$male, digits = 2),
      format_ci(heart_rate_results$male, digits = 2),
      format_ci(ventilation_results$male, digits = 2),
      format_ci(oxygen_pulse_results$male, digits = 2),
      NA,
      format_ci(breathing_frequency_results$male, digits = 2)
    )
    ci_female <- c(
      format_ci(vo2_ml_min_results$female, digits = 2),
      format_ci(vo2_ml_kg_min_results$female, digits = 2),
      format_ci(heart_rate_results$female, digits = 2),
      format_ci(ventilation_results$female, digits = 2),
      format_ci(oxygen_pulse_results$female, digits = 2),
      NA,
      format_ci(breathing_frequency_results$female, digits = 2)
    )

    data.frame(
      "Metric" = metrics,
      "Value Boy" = value_male,
      "CI Boy" = ci_male,
      "Value Girl" = value_female,
      "CI Girl" = ci_female,
      check.names = FALSE
    )
  }, align = "lrcrc")
  output$confidence_intervals <- renderUI({
    tabsetPanel(
      tabPanel("VO2 ml/min",
        h4("By diagnostic group"), plotOutput("vo2_ml_min_plot_group"),
        h4("By height (cm)"), plotOutput("vo2_ml_min_plot_height"),
        h4("By BMI (kg/m²)"), plotOutput("vo2_ml_min_plot_bmi")
      ),
      tabPanel("VO₂/kg", plotOutput("vo2_ml_kg_min_plot")),
      tabPanel("HR", plotOutput("heart_rate_plot")),
      tabPanel("VE", plotOutput("ventilation_plot")),
      tabPanel("O₂ pulse", plotOutput("oxygen_pulse_plot")),
      tabPanel("VE/VCO₂", plotOutput("ve_vco2_slope_plot")),
      tabPanel("fR", plotOutput("breathing_frequency_plot")),
      type = "pills"
    )
  })

  output$vo2_ml_kg_min_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$vo2_ml_kg_min(group, person_male)
    results_female <- group$vo2_ml_kg_min(group, person_female)
    # Assume results_* is a matrix with 1 row, 3 columns: estimate, lower, upper
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "VO2 ml/kg/min", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = rev(strong_col)
    )
  })

  output$oxygen_pulse_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$oxygen_pulse(group, person_male)
    results_female <- group$oxygen_pulse(group, person_female)
    # Assume results_* is a matrix with 1 row, 3 columns: estimate, lower, upper
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "Oxygen pulse", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = rev(strong_col)
    )
  })

  output$heart_rate_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$heart_rate(group, person_male)
    results_female <- group$heart_rate(group, person_female)
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "Heart rate", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = rev(strong_col)
    )
  })

  output$ventilation_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$ventilation(group, person_male)
    results_female <- group$ventilation(group, person_female)
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "Ventilation", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = rev(strong_col)
    )
  })

  output$ve_vco2_slope_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$ve_vco2_slope(group, person_male)
    results_female <- group$ve_vco2_slope(group, person_female)
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates,
      xlab = "Sex", ylab = "VE/VCO2 slope", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
  })

  output$breathing_frequency_plot <- renderPlot({
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$breathing_frequency(group, person_male)
    results_female <- group$breathing_frequency(group, person_female)
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "Breathing frequency", xaxt = "n", pch = 16, col = rev(strong_col)
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = rev(strong_col)
    )
  })


  # By diagnostic group
  output$vo2_ml_min_plot_group <- renderPlot({
    groups <- c("simple", "moderate", "fontan")
    group_labels <- c("Simple", "Moderate", "Fontan")
    height <- input$height
    bmi <- input$bmi
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(groups))
    faded_col <- c(color_girl_faded, color_boy_faded) # [girl, boy]
    for (sex in 0:1) {
      for (i in seq_along(groups)) {
        g <- get(groups[i])
        p <- person(sex = sex, height = height, bmi = bmi)
        y_est[sex+1, i] <- g$vo2_ml_min(g, p)[1, 1]
        y_lower[sex+1, i] <- g$vo2_ml_min(g, p)[1, 2]
        y_upper[sex+1, i] <- g$vo2_ml_min(g, p)[1, 3]
      }
    }
    highlight <- match(input$group, groups)
    col_mat <- matrix(NA, nrow = 2, ncol = length(groups))
    col_mat[1, ] <- faded_col[1] # girls row
    col_mat[2, ] <- faded_col[2] # boys row
    if (!is.na(highlight)) {
      col_mat[1, highlight] <- strong_col[1]
      col_mat[2, highlight] <- strong_col[2]
    }
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    matplot(1:3, t(y_est), type = "p", pch = 16, lty = 1, col = strong_col,
            xlab = "Diagnostic group", ylab = "VO2 ml/min", xaxt = "n", ylim = range(y_lower, y_upper, na.rm = TRUE))
    axis(1, at = 1:3, labels = group_labels)
    for (sex in 0:1) {
      arrows(1:3, y_lower[sex+1,], 1:3, y_upper[sex+1,], angle = 90, code = 3, length = 0.07, col = col_mat[sex+1,], lwd = ifelse(1:3 == highlight, 2, 1))
      points(1:3, y_est[sex+1,], pch = 16, col = col_mat[sex+1,], cex = ifelse(1:3 == highlight, 1.3, 1))
    }
    legend("topright", legend = c("Boy", "Girl"), col = rev(strong_col), pch = 16, lty = 1)
  })

  # By height (cm)
  output$vo2_ml_min_plot_height <- renderPlot({
    group <- get(input$group)
    bmi <- input$bmi
    heights <- 100:210
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(heights))
    faded_col <- c(color_girl_faded, color_boy_faded) # [girl, boy]
    highlight <- which(heights == input$height)
    for (sex in 0:1) {
      for (i in seq_along(heights)) {
        p <- person(sex = sex, height = heights[i], bmi = bmi)
        res <- group$vo2_ml_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    col_mat <- matrix(NA, nrow = 2, ncol = length(heights))
    col_mat[1, ] <- faded_col[1]
    col_mat[2, ] <- faded_col[2]
    if (length(highlight) == 1) {
      col_mat[1, highlight] <- strong_col[1]
      col_mat[2, highlight] <- strong_col[2]
    }
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    matplot(heights, t(y_est), type = "c", lty = 1, col = strong_col,
            xlab = "Height (cm)", ylab = "VO2 ml/min", ylim = range(y_lower, y_upper, na.rm = TRUE))
    for (sex in 0:1) {
      for (i in seq_along(heights)) {
        arrows(heights[i], y_lower[sex+1, i], heights[i], y_upper[sex+1, i], angle = 90, code = 3, length = 0.07, col = col_mat[sex+1, i], lwd = ifelse(i == highlight, 2, 1))
      }
      points(heights, y_est[sex+1,], pch = 16, col = col_mat[sex+1,], cex = ifelse(1:length(heights) == highlight, 1.3, 1))
    }
    legend("topright", legend = c("Boy", "Girl"), col = rev(strong_col), pch = 16, lty = 1)
  })

  # By BMI (kg/m²)
  output$vo2_ml_min_plot_bmi <- renderPlot({
    group <- get(input$group)
    height <- input$height
    bmis <- seq(5, 35, by = 0.1)
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(bmis))
    faded_col <- c(color_girl_faded, color_boy_faded) # [girl, boy]
    highlight <- which(abs(bmis - input$bmi) < 1e-8)
    for (sex in 0:1) {
      for (i in seq_along(bmis)) {
        p <- person(sex = sex, height = height, bmi = bmis[i])
        res <- group$vo2_ml_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    col_mat <- matrix(NA, nrow = 2, ncol = length(bmis))
    col_mat[1, ] <- faded_col[1]
    col_mat[2, ] <- faded_col[2]
    if (length(highlight) == 1) {
      col_mat[1, highlight] <- strong_col[1]
      col_mat[2, highlight] <- strong_col[2]
    }
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    matplot(bmis, t(y_est), type = "c", lty = 1, col = strong_col,
            xlab = "BMI (kg/m²)", ylab = "VO2 ml/min", ylim = range(y_lower, y_upper, na.rm = TRUE))
    for (sex in 0:1) {
      for (i in seq_along(bmis)) {
        arrows(bmis[i], y_lower[sex+1, i], bmis[i], y_upper[sex+1, i], angle = 90, code = 3, length = 0.07, col = col_mat[sex+1, i], lwd = ifelse(i == highlight, 2, 1))
      }
      points(bmis, y_est[sex+1,], pch = 16, col = col_mat[sex+1,], cex = ifelse(1:length(bmis) == highlight, 1.3, 1))
    }
    legend("topright", legend = c("Boy", "Girl"), col = rev(strong_col), pch = 16, lty = 1)
  })
}
