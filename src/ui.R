ui <- fluidPage(
  titlePanel("Kids with Congenital Heart Defects"),
  sidebarPanel(
      # Step 1: selecting diagnostic group and covariates
      h2("Select covariates"),
      radioButtons(
        "group", "Diagnostic group",
        choiceNames = list(
          tagList(
            "Simple defects",
            tags$span(
              style = "margin-left: 5px; cursor: pointer;",
              title = "Atrial and ventricular septal defect, Coarctation of the Aorta, Left Ventricular Outflow Tract Obstruction",
              "(?)"
            )
          ),
          tagList(
            "Moderate complex defects",
            tags$span(
              style = "margin-left: 5px; cursor: pointer;",
              title = "Tetralogy of Fallot, Transposition of the Great Arteries",
              "(?)"
            )
          ),
          "Univentricular defects with Fontan circulation"
        ),
          choiceValues = c("simple", "moderate", "fontan")
      ),
        # Removed explanatory box; tooltips are now in radioButtons
      numericInput(
        "height", "Height (cm)",
        value = 150L, min = 0L, step = 1L, max = 210L
      ),
      numericInput(
        "bmi", "BMI (kg/m²)",
        value = 20.0, min = 0.1, step = 0.1, max = 50.0
      ),
      actionButton("submit", "Calculate endpoints"),

      # Collapsible options section
      actionButton("toggle_options", label = "Additional Options ▼"),
      conditionalPanel(
        condition = "input.toggle_options % 2 == 1",
        div(style = "margin-top: 10px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: #f9f9f9;",
          h2("Options"),
          h3("Confidence level"),
          sliderInput(
            "conf_level", "Confidence level (%)",
            value = 95L, min = 80L, max = 99L, step = 1L,
            ticks = FALSE
          ),
          h3("Plot ranges"),
          sliderInput(
            "height_range", "Height range (cm)",
            value = c(150L, 180L), min = 100L, max = 210L, step = 1L
          ),
          sliderInput(
            "bmi_range", "BMI range (kg/m²)",
            value = c(18.5, 25), min = 5, max = 35, step = 0.5
          )
        )
      )
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
      version 0.0.0.9038
    </p>')
  )
)
