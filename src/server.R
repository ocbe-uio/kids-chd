server <- function(input, output) {
  vo2_ml_min <- function() {
    NA
  }

  vo2_ml_kg_min <- function() {
    NA
  }

  heart_rate <- function(diagnosis, height, bmi, is_male) {
    is_vyntus <- 0.0469 # prop. of Vyntus on Fontan (only relevant case here)
    switch(diagnosis,
      "simple" = (9168804 * height + 5.13e9) ^ (1 / 4.3),
      "moderate" = (9.9e8 * height - 2.86e9 * bmi + 1.4e11) ^ (1 / 5),
      "fontan" = (-144400.5 * height - 3.81e7 * is_male + 2076971 * bmi * is_male + 1.24e7 * is_vyntus + 9.75e7) ^ (1 / 3.5)
    )
  }

  ventilation <- function() {
    NA
  }

  oxygen_pulse <- function() {
    NA
  }

  ve_vco2_slope <- function() {
    NA
  }

  breathing_frequency <- function() {
    NA
  }

  output$results_table <- renderTable({
    data.frame(
      "Metric" = c(
        "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
        "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
      ),
      "Value" = c(
        vo2_ml_min(),
        vo2_ml_kg_min(),
        heart_rate(input$diagnosis, input$height, input$bmi, as.numeric(input$sex)),
        ventilation(),
        oxygen_pulse(),
        ve_vco2_slope(),
        breathing_frequency()
      )
    )
  })
}
