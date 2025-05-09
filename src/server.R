source("classes.R")
source("groups.R")


server <- function(input, output) {
  output$results_table <- renderTable({
    group <- get(input$group)
    person <- person(
      sex = as.numeric(input$sex), height = input$height, bmi = input$bmi
    )
    data.frame(
      "Metric" = c(
        "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
        "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
      ),
      "Value" = c(
        group$vo2_ml_min(group, person)["Y"],
        group$vo2_ml_kg_min(group, person),
        group$heart_rate(group, person),
        group$ventilation(group, person),
        group$oxygen_pulse(group, person),
        group$ve_vco2_slope(group, person),
        group$breathing_frequency(group, person)
      ),
      "95 pct CI, lower" = c(
        group$vo2_ml_min(group, person)[2],
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
      ),
      "95 pct CI, upper" = c(
        group$vo2_ml_min(group, person)[3],
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
      )
    )
  })
}
