ui <- fluidPage(
  # Step 1: selecting diagnosis
  h1("Kids with congenital heart defects"),
  wellPanel(
    h2("Select diagnosis"),
    selectInput(
      inputId = "diagnosis",
      label   = "Diagnostic group:",
      choices = c(
        "Simple defects" = "simple_defects",
        "Moderate complex defects" = "moderate_complex_defects",
        "Fontan circulation" = "fontan_circulation"
      )
    ),
    # Add "Next" button
    actionButton("next", "Next")
  ),
  # Step 2: selecting covariates
  conditionalPanel(
    condition = "input.next > 0",
    wellPanel(
      h2("Select covariates"),
      radioButtons("sex", "Sex", c("Male", "Female")),
      numericInput(
        "height", "Height (cm)",
        value = 100L, min = 0L, step = 1L, max = 200L
      ),
      numericInput(
        "bmi", "BMI",
        value = 20.0, min = 0.1, max = 50.0, step = 0.1
      )
    ),
    actionButton("submit", "Calculate endpoints")
  ),
  # Step 3: displaying results
  conditionalPanel(
    condition = "input.submit > 0",
    wellPanel(
      h2("Results"),
      "VO2 ml/min: ", textOutput("vo2_ml_min"),
      "VO2 ml/kg/min: ", textOutput("vo2_ml_kg_min"),
      "Heart rate: ", textOutput("heart_rate"),
      "Ventilation: ", textOutput("ventilation"),
      "Oxygen pulse: ", textOutput("oxygen_pulse"),
      "VE/VCO2 slope: ", textOutput("ve_vco2_slope"),
      "Breathing frequency: ", textOutput("breathing_frequency")
    )
  )
)

server <- function(input, output, session) {
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

shinyApp(ui, server)
