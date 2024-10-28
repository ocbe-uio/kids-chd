diagnosis <- setRefClass(
  "Diagnosis",
  fields = list(
    hosp_soft = "numeric", # cross-proportion of non-surgical and vyntus
    hosp = "numeric",  # proportion of non-surgical centres
    soft = "numeric",  # proportion of vyntus software
    heart_rate = "function"
  )
)

person <- setRefClass(
  "Person",
  fields = list(
    sex = "numeric",
    height = "numeric",
    bmi = "numeric"
  )
)

simple <- diagnosis(
  hosp_soft = c(0.3994, 0.0029, 0.5478, 0.0499),
  hosp = 0.5849,
  soft = 0.0528,
  heart_rate = function(.self, person) {
    (9168804 * person$height + 5.13e9) ^ (1 / 4.3)
  }
)

moderate <- diagnosis(
  heart_rate = function(.self, person) {
    (9.9e8 * person$height - 2.86e9 * person$bmi + 1.4e11) ^ (1 / 5)
  }
)

fontan <- diagnosis(
  soft = 0.0469,
  heart_rate = function(.self, person) {
    (-144400.5 * person$height - 3.81e7 * person$sex + 2076971 * person$bmi * person$sex + 1.24e7 * .self$soft + 9.75e7) ^ (1 / 3.5)
  }
)

server <- function(input, output) {
  vo2_ml_min <- function() {
    NA
  }

  vo2_ml_kg_min <- function() {
    NA
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
    diag <- get(input$diagnosis)
    person <- person(
      sex = as.numeric(input$sex), height = input$height, bmi = input$bmi
    )
    data.frame(
      "Metric" = c(
        "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
        "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
      ),
      "Value" = c(
        vo2_ml_min(),
        vo2_ml_kg_min(),
        diag$heart_rate(diag, person),
        ventilation(),
        oxygen_pulse(),
        ve_vco2_slope(),
        breathing_frequency()
      )
    )
  })
}
