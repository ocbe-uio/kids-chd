diagnosis <- setRefClass(
  "Diagnosis",
  fields = list(
    hosp_soft = "numeric", # cross-proportion of surgical and vyntus
    hosp = "numeric",  # proportion of surgical centres
    soft = "numeric",  # proportion of vyntus software
    heart_rate = "function",
    breathing_frequency = "function"
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
  },
  breathing_frequency = function(.self, person) {
   (- 0.0114363 * person$height + 0.0007431 * person$sex - 0.1421088 * .self$hosp + 6.693345) ^ (1 / 0.4)
  }
)

moderate <- diagnosis(
  hosp_soft = c(0.6439, 0.0060, 0.2938, 0.0563),
  hosp = 0.6499,
  soft = 0.0624,
  heart_rate = function(.self, person) {
    (9.9e8 * person$height - 2.86e9 * person$bmi + 1.4e11) ^ (1 / 5)
  },
  breathing_frequency = function(.self, person) {
    (-0.037375 * person$height - 1.778892 * person$sex + 0.0134113 * person$height * person$sex - 0.3806323 * .self$hosp + 16.65239) ^ (1 / 0.6)
  }
)

fontan <- diagnosis(
  hosp_soft = c(0.7834, 0.0072, 0.1697, 0.0397),
  hosp = 0.7906,
  soft = 0.0469,
  heart_rate = function(.self, person) {
    (-144400.5 * person$height - 3.81e7 * person$sex + 2076971 * person$bmi * person$sex + 1.24e7 * .self$soft + 9.75e7) ^ (1 / 3.5)
  },
  breathing_frequency = function(.self, person) {
    exp(-0.0044619 * person$height + 0.0225936 * log(person$bmi) * person$sex - 0.0820773 * .self$hosp + 4.609728)
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
        diag$breathing_frequency(diag, person)
      )
    )
  })
}
