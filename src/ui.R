ui <- fluidPage(
  titlePanel("Kids with Congenital Heart Defects - v0.0.0.9021"),
  sidebarPanel(
    # Step 1: selecting diagnostic group
    h2("Select group"),
    radioButtons(
      "group", "Diagnostic group",
      choiceNames = c(
        "Simple defects",
        "Moderate complex defects",
        "Univentricular defects with Fontan circulation"
      ),
      choiceValues = c("simple", "moderate", "fontan")
    ),

    # Step 2: selecting covariates
    h2("Select covariates"),

    # Sex selection removed; results will be shown for both genders
    numericInput(
      "height", "Height (cm)",
      value = 150L, min = 0L, step = 1L, max = 210L
    ),
    numericInput(
      "bmi", "BMI (kg/m²)",
      value = 20.0, min = 0.1, step = 0.1, max = 50.0
    ),
    actionButton("submit", "Calculate endpoints")
  ),

  mainPanel(
    h1("⚠️ THIS PAGE IS UNDER DEVELOPMENT ⚠️"),
    # Step 3: displaying results
    conditionalPanel(
      condition = "input.submit > 0",
      wellPanel(
        h2("Results"),
        HTML("The values below are calculated based on the selected diagnostic group
        and covariates."),
        tableOutput("results_table"),
        h2("Plots"),
        HTML("The selected diagnostic group and covariates are highlighted in the plots below."),
        uiOutput("confidence_intervals")
      )
    )
  )
)
