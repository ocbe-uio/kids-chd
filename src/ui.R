ui <- fluidPage(
  titlePanel("Kids with Congenital Heart Defects"),
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

    # Step 3: selecting confidence level
    h2("Select confidence level"),
    sliderInput(
      "conf_level", "Confidence level (%)",
      value = 95L, min = 80L, max = 99L, step = 1L,
      ticks = FALSE
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
        uiOutput("plots")
      )
    ),
    # Footer with source code link and version
    hr(),
    HTML('<p style="font-size: 0.85em; color: #666; text-align: center;">
      <a href="https://github.com/ocbe-uio/kids-chd" target="_blank" rel="noopener noreferrer" style="color: #337ab7; text-decoration: none;">
        View source code on GitHub
      </a>
      &nbsp;|&nbsp;
      version 0.0.0.9034
    </p>')
  )
)
