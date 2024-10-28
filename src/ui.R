ui <- fluidPage(
  titlePanel("Kids with Congenital Heart Defects"),
  sidebarPanel(
    # Step 1: selecting diagnosis
    h2("Select diagnosis"),
    radioButtons(
      "diagnosis", "Diagnostic group:",
      choiceNames = c(
        "Simple defects",
        "Moderate complex defects",
        "Univentricular defects with Fontan circulation"
      ),
      choiceValues = c("simple", "moderate", "fontan")
    ),
    # Step 2: selecting covariates
    h2("Select covariates"),
    radioButtons(
      "sex", "Sex",
      choiceNames = list("Male", "Female"), choiceValues = list(1L, 0L)
    ),
    numericInput(
      "height", "Height (cm)",
      value = 100L, min = 0L, step = 1L, max = 200L
    ),
    numericInput(
      "bmi", "BMI",
      value = 20.0, min = 0.1, max = 50.0, step = 0.1
    ),
    actionButton("submit", "Calculate endpoints")
  ),
  mainPanel(
    # Step 3: displaying results
    conditionalPanel(
      condition = "input.submit > 0",
      wellPanel(
        h2("Results"),
        tableOutput("results_table")
      )
    )
  )
)
