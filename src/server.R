source("classes.R")
source("methods.R")
source("models.R")
source("plotting.R")

server <- function(input, output) {
  # Mapping of group IDs to display names (used in multiple plots)
  group_names <- c(simple = "Simple", moderate = "Moderate", fontan = "Fontan")

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
      "Absolute VO₂ (mL/min)", "VO₂ relative to body mass (mL/kg/min)",
      "Heart rate (BPM)", "Minute ventilation (L/min)",
      "Oxygen pulse (mL/beat)", "Ventilatory efficiency (VE/VCO₂ slope)",
      "Breathing frequency (breaths/min)"
    )

    # Format values: 1 decimal for HR and VO2 ml/min, 2 decimals for others
    value_male <- c(
      sprintf("%.1f", vo2_ml_min_results$male[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$male[1, 1]),
      sprintf("%.1f", heart_rate_results$male[1, 1]),
      sprintf("%.2f", ventilation_results$male[1, 1]),
      sprintf("%.2f", oxygen_pulse_results$male[1, 1]),
      sprintf("%.2f", ve_vco2_slope_results$male[1, 1]),
      sprintf("%.2f", breathing_frequency_results$male[1, 1])
    )
    value_female <- c(
      sprintf("%.1f", vo2_ml_min_results$female[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$female[1, 1]),
      sprintf("%.1f", heart_rate_results$female[1, 1]),
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
      format_ci(vo2_ml_min_results$male, digits = 1),
      format_ci(vo2_ml_kg_min_results$male, digits = 2),
      format_ci(heart_rate_results$male, digits = 1),
      format_ci(ventilation_results$male, digits = 2),
      format_ci(oxygen_pulse_results$male, digits = 2),
      NA,
      format_ci(breathing_frequency_results$male, digits = 2)
    )
    ci_female <- c(
      format_ci(vo2_ml_min_results$female, digits = 1),
      format_ci(vo2_ml_kg_min_results$female, digits = 2),
      format_ci(heart_rate_results$female, digits = 1),
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
  }, align = "lccrc")

  output$confidence_intervals <- renderUI({
    tabsetPanel(
      tabPanel("VO₂",
        plotOutput("vo2_ml_min_plot_group"),
        plotOutput("vo2_ml_min_plot_height"),
        plotOutput("vo2_ml_min_plot_bmi")
      ),
      tabPanel("VO₂/kg",
        plotOutput("vo2_ml_kg_min_plot_group"),
        plotOutput("vo2_ml_kg_min_plot_height"),
        plotOutput("vo2_ml_kg_min_plot_bmi")
      ),
      tabPanel("HR",
        plotOutput("heart_rate_plot_group")
      ),
      tabPanel("VE",
        plotOutput("ventilation_plot_group")
      ),
      tabPanel("O₂ pulse",
        plotOutput("oxygen_pulse_plot_group")
      ),
      tabPanel("VE/VCO₂",
        plotOutput("ve_vco2_slope_plot_group")
      ),
      tabPanel("BF",
        plotOutput("breathing_frequency_plot_group")
      ),
      type = "pills"
    )
  })

  # Plots by diagnostic group
  output$vo2_ml_min_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$vo2_ml_min(g, p),
      ylab = "VO2 ml/min",
      main = "VO2 by group",
      legend_pos = "topright"
    )
  })

  # VO2/kg by group
  output$vo2_ml_kg_min_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$vo2_ml_kg_min(g, p),
      ylab = "VO2 ml/kg/min",
      main = "VO2/kg by group",
      legend_pos = "topright"
    )
  })

  # Heart rate by group
  output$heart_rate_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$heart_rate(g, p),
      ylab = "Heart rate (BPM)",
      main = "Heart rate by group",
      legend_pos = "bottomleft"
    )
  })

  # Ventilation by group
  output$ventilation_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$ventilation(g, p),
      ylab = "Ventilation (L/min)",
      main = "Ventilation by group",
      legend_pos = "topright"
    )
  })

  # Oxygen pulse by group
  output$oxygen_pulse_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$oxygen_pulse(g, p),
      ylab = "Oxygen pulse (mL/beat)",
      main = "Oxygen pulse by group",
      legend_pos = "topright"
    )
  })

  # VE/VCO2 slope by group
  output$ve_vco2_slope_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$ve_vco2_slope(g, p),
      ylab = "VE/VCO2 slope",
      main = "VE/VCO2 slope by group",
      show_ci = FALSE,
      legend_pos = "topleft"
    )
  })

  # Breathing frequency by group
  output$breathing_frequency_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p) g$breathing_frequency(g, p),
      ylab = "Breathing frequency (breaths/min)",
      main = "Breathing frequency by group",
      legend_pos = "bottomleft"
    )
  })

  # By height (cm) (VO2/kg)
  output$vo2_ml_kg_min_plot_height <- renderPlot({
    group <- get(input$group)
    bmi <- input$bmi
    heights <- 100:210
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(heights))
    for (sex in 0:1) {
      for (i in seq_along(heights)) {
        p <- person(sex = sex, height = heights[i], bmi = bmi)
        res <- group$vo2_ml_kg_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    group_label <- group_names[input$group]
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    matplot(heights, t(y_est), type = "n", lty = 1, col = strong_col,
            xlab = "Height (cm)", ylab = "VO2 ml/kg/min", ylim = range(y_lower, y_upper, na.rm = TRUE),
            main = "VO2/kg by height")
    faded_col <- c(color_girl_faded, color_boy_faded)
    for (sex in 0:1) {
      polygon(c(heights, rev(heights)),
              c(y_lower[sex+1,], rev(y_upper[sex+1,])),
              col = faded_col[sex+1], border = NA)
    }
    matlines(heights, t(y_est), lty = 1, col = strong_col, lwd = 2)
    matlines(heights, t(y_lower), lty = 2, col = strong_col)
    matlines(heights, t(y_upper), lty = 2, col = strong_col)
    legend("bottomright",
           legend = c("Boy (estimate)", "Boy (CI)", "Girl (estimate)", "Girl (CI)", "",
                     sprintf("Group: %s", group_label),
                     sprintf("BMI: %.1f kg/m²", bmi)),
           col = c(strong_col[2], strong_col[2], strong_col[1], strong_col[1], NA, NA, NA),
           lty = c(1, 2, 1, 2, NA, NA, NA), lwd = 2, seg.len = 3)
  })

  # By BMI (kg/m²) (VO2/kg)
  output$vo2_ml_kg_min_plot_bmi <- renderPlot({
    group <- get(input$group)
    height <- input$height
    bmis <- seq(5, 35, by = 0.1)
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(bmis))
    for (sex in 0:1) {
      for (i in seq_along(bmis)) {
        p <- person(sex = sex, height = height, bmi = bmis[i])
        res <- group$vo2_ml_kg_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    group_label <- group_names[input$group]
    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
    matplot(bmis, t(y_est), type = "n", lty = 1, col = strong_col,
            xlab = "BMI (kg/m²)", ylab = "VO2 ml/kg/min", ylim = range(y_lower, y_upper, na.rm = TRUE),
            main = "VO2/kg by BMI")
    faded_col <- c(color_girl_faded, color_boy_faded)
    for (sex in 0:1) {
      polygon(c(bmis, rev(bmis)),
              c(y_lower[sex+1,], rev(y_upper[sex+1,])),
              col = faded_col[sex+1], border = NA)
    }
    matlines(bmis, t(y_est), lty = 1, col = strong_col, lwd = 2)
    matlines(bmis, t(y_lower), lty = 2, col = strong_col)
    matlines(bmis, t(y_upper), lty = 2, col = strong_col)
    legend("bottomright",
           legend = c("Boy (estimate)", "Boy (CI)", "Girl (estimate)", "Girl (CI)", "",
                     sprintf("Group: %s", group_label),
                     sprintf("Height: %.0f cm", height)),
           col = c(strong_col[2], strong_col[2], strong_col[1], strong_col[1], NA, NA, NA),
           lty = c(1, 2, 1, 2, NA, NA, NA), lwd = 2, seg.len = 3)
  })

  # By height (cm)
  output$vo2_ml_min_plot_height <- renderPlot({
    group <- get(input$group)
    bmi <- input$bmi
    heights <- 100:210
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(heights))
    for (sex in 0:1) {
      for (i in seq_along(heights)) {
        p <- person(sex = sex, height = heights[i], bmi = bmi)
        res <- group$vo2_ml_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    # Get group label for legend
    group_label <- group_names[input$group]

    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
          matplot(heights, t(y_est), type = "n", lty = 1, col = strong_col,
            xlab = "Height (cm)", ylab = "VO2 ml/min", ylim = range(y_lower, y_upper, na.rm = TRUE),
            main = "VO2 by height")
          faded_col <- c(color_girl_faded, color_boy_faded)
          for (sex in 0:1) {
            polygon(c(heights, rev(heights)),
              c(y_lower[sex+1,], rev(y_upper[sex+1,])),
              col = faded_col[sex+1], border = NA)
          }
          matlines(heights, t(y_est), lty = 1, col = strong_col, lwd = 2)
          matlines(heights, t(y_lower), lty = 2, col = strong_col)
          matlines(heights, t(y_upper), lty = 2, col = strong_col)
              legend("bottomright",
                legend = c("Boy (estimate)", "Boy (CI)", "Girl (estimate)", "Girl (CI)", "",
                          sprintf("Group: %s", group_label),
                          sprintf("BMI: %.1f kg/m²", bmi)),
                col = c(strong_col[2], strong_col[2], strong_col[1], strong_col[1], NA, NA, NA),
                lty = c(1, 2, 1, 2, NA, NA, NA), lwd = 2, seg.len = 3)
  })

  # By BMI (kg/m²)
  output$vo2_ml_min_plot_bmi <- renderPlot({
    group <- get(input$group)
    height <- input$height
    bmis <- seq(5, 35, by = 0.1)
    y_est <- y_lower <- y_upper <- matrix(NA, nrow = 2, ncol = length(bmis))
    for (sex in 0:1) {
      for (i in seq_along(bmis)) {
        p <- person(sex = sex, height = height, bmi = bmis[i])
        res <- group$vo2_ml_min(group, p)
        y_est[sex+1, i] <- res[1, 1]
        y_lower[sex+1, i] <- res[1, 2]
        y_upper[sex+1, i] <- res[1, 3]
      }
    }
    # Get group label for legend
    group_label <- group_names[input$group]

    par(bg = rgb(0.9607843, 0.9607843, 0.9607843))
          matplot(bmis, t(y_est), type = "n", lty = 1, col = strong_col,
            xlab = "BMI (kg/m²)", ylab = "VO2 ml/min", ylim = range(y_lower, y_upper, na.rm = TRUE),
            main = "VO2 by BMI")
          faded_col <- c(color_girl_faded, color_boy_faded)
          for (sex in 0:1) {
            polygon(c(bmis, rev(bmis)),
              c(y_lower[sex+1,], rev(y_upper[sex+1,])),
              col = faded_col[sex+1], border = NA)
          }
          matlines(bmis, t(y_est), lty = 1, col = strong_col, lwd = 2)
          matlines(bmis, t(y_lower), lty = 2, col = strong_col)
          matlines(bmis, t(y_upper), lty = 2, col = strong_col)
              legend("bottomright",
                legend = c("Boy (estimate)", "Boy (CI)", "Girl (estimate)", "Girl (CI)", "",
                          sprintf("Group: %s", group_label),
                          sprintf("Height: %.0f cm", height)),
                col = c(strong_col[2], strong_col[2], strong_col[1], strong_col[1], NA, NA, NA),
                lty = c(1, 2, 1, 2, NA, NA, NA), lwd = 2, seg.len = 3)
  })
}
