server <- function(input, output) {
  # Calculate endpoints
  output$vo2_ml_min <- renderText({
    vo2_ml_min()
  })
  output$vo2_ml_kg_min <- renderText({
    vo2_ml_kg_min()
  })
  output$heart_rate <- renderText({
    heart_rate()
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
}

vo2_ml_min <- function() {
  1
}

vo2_ml_kg_min <- function() {
  2
}

heart_rate <- function() {
  3
}

ventilation <- function() {
  4
}

oxygen_pulse <- function() {
  5
}

ve_vco2_slope <- function() {
  6
}

breathing_frequency <- function() {
  7
}
