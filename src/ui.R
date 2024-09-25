ui <- fluidPage(
  # Step 1: selecting diagnosis
  h1("Kids with congenital heart defects"),
  wellPanel(
    h2("Select diagnosis"),
    selectInput(
      inputId = "diagnosis",
      label   = "Diagnostic group:",
      choices = c(
        "Simple defects" = "simple",
        "Moderate complex defects" = "moderate",
        "Fontan circulation" = "fontan"
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
