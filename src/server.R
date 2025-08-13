source("classes.R")
source("groups.R")


server <- function(input, output) {
  output$results_table <- renderTable({
    group <- get(input$group)
    person <- person(
      sex = as.numeric(input$sex), height = input$height, bmi = input$bmi
    )
    vo2_ml_min_results <- group$vo2_ml_min(group, person)
    data.frame(
      "Metric" = c(
        "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
        "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
      ),
      "Value" = c(
        vo2_ml_min_results[1, 1],
        group$vo2_ml_kg_min(group, person),
        group$heart_rate(group, person),
        group$ventilation(group, person),
        group$oxygen_pulse(group, person),
        group$ve_vco2_slope(group, person),
        group$breathing_frequency(group, person)
      ),
      "Confidence_Interval" = c(
        paste0(
          round(vo2_ml_min_results[1, 2], 2), " - ",
          round(vo2_ml_min_results[1, 3], 2)
        ),
        NA, NA, NA, NA, NA, NA
      )
    )
  })
  output$confidence_intervals <- renderPlot({
    if (input$group != "simple") {
      # Workaround while plots are not implemented for all groups
      return(NULL)
    }
    group <- get(input$group)
    person <- person(
      sex = as.numeric(input$sex), height = input$height, bmi = input$bmi
    )
    results <- group$vo2_ml_min(group, person)
    point_estimates <- results[, 1]
    lower_limits <- results[, 2]
    upper_limits <- results[, 3]

    plot(
      x = 1:2, y = point_estimates, ylim = range(lower_limits, upper_limits),
      xlab = "Group", ylab = "VO2 ml/min", xaxt = "n", pch = 16, col = "blue"
    )
    axis(1, at = 1:2, labels = c("Selected sex", "Other sex"))
    arrows(
      x0 = 1:2, y0 = lower_limits, x1 = 1:2, y1 = upper_limits,
      angle = 90, code = 3, length = 0.1, col = "blue"
    )
  })
}
