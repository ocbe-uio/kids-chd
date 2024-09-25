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
      radioButtons(
        "sex", "Sex",
        selected = character(0),
        choiceNames = list("Male", "Female"), choiceValues = list(1L, 0L)
      ),
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
      tableOutput("results_table")
    )
  )
)
