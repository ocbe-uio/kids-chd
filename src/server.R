server <- function(input, output) {
  # Calculate endpoints
  output$vo2_ml_min <- renderText({
    vo2_ml_min()
  })
  output$vo2_ml_kg_min <- renderText({
    vo2_ml_kg_min()
  })
  output$heart_rate <- renderText({
    heart_rate(input$diagnosis, input$height, input$bmi, as.numeric(input$sex))
  })
  output$ventilation <- renderText({
    ventilation()
  })
  output$oxygen_pulse <- renderText({
    oxygen_pulse()
  })
  output$ve_vco2_slope <- renderText({
    ve_vco2_slope()
  })
  output$breathing_frequency <- renderText({
    breathing_frequency()
  })

  vo2_ml_min <- function() {
    NA
  }

  vo2_ml_kg_min <- function() {
    NA
  }

  heart_rate <- function(diagnosis, height, bmi, sex) {
    software <- 0.0469 # prop. of Vyntus on Fontan (only relevant case here)
    switch(diagnosis,
      simple = (9168804 * height + 5.13e9) ^ (1 / 4.3),
      moderate = (9.9e8 * height - 2.86e9 * bmi + 1.4e11) ^ (1 / 5),
      fontan = (-144400.5 * height - 3.81e7 * sex + 2076971 * bmi * sex + 1.24e7 * software + 9.75e7) ^ (1 / 3.5)
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
}
