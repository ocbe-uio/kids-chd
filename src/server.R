source("classes.R")
source("methods.R")
source("models.R")

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

    if (input$group == "simple") {
      vo2_ml_min_results <- get_metric_ci(group$vo2_ml_min)
      vo2_ml_kg_min_results <- get_metric_ci(group$vo2_ml_kg_min)
      oxygen_pulse_results <- get_metric_ci(group$oxygen_pulse)
      ve_vco2_slope_results <- get_metric_ci(group$ve_vco2_slope)
    } else {
      vo2_ml_min_results <- list(
        male = matrix(c(group$vo2_ml_min(group, person_male), NA, NA), 1),
        female = matrix(c(group$vo2_ml_min(group, person_female), NA, NA), 1)
      )
      vo2_ml_kg_min_results <- list(
        male = matrix(c(group$vo2_ml_kg_min(group, person_male), NA, NA), 1),
        female = matrix(c(group$vo2_ml_kg_min(group, person_female), NA, NA), 1)
      )
      oxygen_pulse_results <- list(
        male = matrix(c(group$oxygen_pulse(group, person_male), NA, NA), 1),
        female = matrix(c(group$oxygen_pulse(group, person_female), NA, NA), 1)
      )
      ve_vco2_slope_results <- list(
        male = matrix(c(group$ve_vco2_slope(group, person_male), NA, NA), 1),
        female = matrix(c(group$ve_vco2_slope(group, person_female), NA, NA), 1)
      )
    }

    metrics <- c(
      "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
      "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
    )

    value_male <- c(
      vo2_ml_min_results$male[1, 1],
      vo2_ml_kg_min_results$male[1, 1],
      group$heart_rate(group, person_male),
      group$ventilation(group, person_male),
      oxygen_pulse_results$male[1, 1],
      ve_vco2_slope_results$male[1, 1],
      group$breathing_frequency(group, person_male)
    )
    value_female <- c(
      vo2_ml_min_results$female[1, 1],
      vo2_ml_kg_min_results$female[1, 1],
      group$heart_rate(group, person_female),
      group$ventilation(group, person_female),
      oxygen_pulse_results$female[1, 1],
      ve_vco2_slope_results$female[1, 1],
      group$breathing_frequency(group, person_female)
    )
    format_ci <- function(mat) {
      if (is.null(mat) || any(is.na(mat[1, 2:3]))) {
      return(NA)
      }
      paste(
      formatC(mat[1, 2], width = 6, format = "f", digits = 2, flag = "#"),
      "-",
      formatC(mat[1, 3], width = 6, format = "f", digits = 2, flag = "#")
      )
    }

    ci_male <- c(
      format_ci(vo2_ml_min_results$male),
      format_ci(vo2_ml_kg_min_results$male),
      NA,
      NA,
      format_ci(oxygen_pulse_results$male),
      NA,
      NA
    )
    ci_female <- c(
      format_ci(vo2_ml_min_results$female),
      format_ci(vo2_ml_kg_min_results$female),
      NA,
      NA,
      format_ci(oxygen_pulse_results$female),
      NA,
      NA
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
      tabPanel("VO2 ml/min", plotOutput("vo2_ml_min_plot")),
      tabPanel("VO2 ml/kg/min", plotOutput("vo2_ml_kg_min_plot")),
      tabPanel("Oxygen pulse", plotOutput("oxygen_pulse_plot"))
    )
  })

  output$vo2_ml_min_plot <- renderPlot({
    if (input$group != "simple") {
      return(NULL)
    }
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$vo2_ml_min(group, person_male)
    results_female <- group$vo2_ml_min(group, person_female)
    # Assume results_* is a matrix with 1 row, 3 columns: estimate, lower, upper
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])

    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "VO2 ml/min", xaxt = "n", pch = 16, col = c("blue", "red")
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = c("blue", "red")
    )
  })

  output$vo2_ml_kg_min_plot <- renderPlot({
    if (input$group != "simple") {
      return(NULL)
    }
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$vo2_ml_kg_min(group, person_male)
    results_female <- group$vo2_ml_kg_min(group, person_female)
    # Assume results_* is a matrix with 1 row, 3 columns: estimate, lower, upper
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])

    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "VO2 ml/kg/min", xaxt = "n", pch = 16, col = c("blue", "red")
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = c("blue", "red")
    )
  })

  output$oxygen_pulse_plot <- renderPlot({
    if (input$group != "simple") {
      return(NULL)
    }
    group <- get(input$group)
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)
    results_male <- group$oxygen_pulse(group, person_male)
    results_female <- group$oxygen_pulse(group, person_female)
    # Assume results_* is a matrix with 1 row, 3 columns: estimate, lower, upper
    point_estimates <- c(results_male[1, 1], results_female[1, 1])
    lower_limits <- c(results_male[1, 2], results_female[1, 2])
    upper_limits <- c(results_male[1, 3], results_female[1, 3])

    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Sex", ylab = "Oxygen pulse", xaxt = "n", pch = 16, col = c("blue", "red")
    )
    axis(1, at = 1:2, labels = c("Boy", "Girl"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = c("blue", "red")
    )
  })
}
