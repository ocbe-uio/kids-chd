diagnosis <- setRefClass(
  "Diagnosis",
  fields = list(
    surg_vyntus = "numeric", # cross-proportion of surgical and vyntus
    surg = "numeric",  # proportion of surgical centres
    vyntus = "numeric",  # proportion of vyntus software
    grid = "data.frame",
    vo2_ml_min = "function",
    vo2_ml_kg_min = "function",
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
  surg_vyntus = c(0.3994, 0.0029, 0.5478, 0.0499),
  surg = 0.5849,
  vyntus = 0.0528,
  grid = expand.grid("vyntus" = 0:1, "surg" = 1:0),
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(0.0155584 * person$height + 0.4371531 * log(person$bmi) + 0.0009139 * person$height * person$sex - 0.1803019 * config["vyntus"] + 0.102317 * config["surg"] + 3.760053)
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      0.0983195 * person$height - 1.152879 * person$bmi + 0.0423992 * person$height * person$sex - 7.601633 * config["vyntus"] + 4.73933 * config["surg"] + 45.77055
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  heart_rate = function(.self, person) {
    (9168804 * person$height + 5.13e9) ^ (1 / 4.3)
  },
  breathing_frequency = function(.self, person) {
   (- 0.0114363 * person$height + 0.0007431 * person$sex - 0.1421088 * .self$surg + 6.693345) ^ (1 / 0.4)
  }
)

moderate <- diagnosis(
  surg_vyntus = c(0.6439, 0.0060, 0.2938, 0.0563),
  surg = 0.6499,
  vyntus = 0.0624,
  grid = expand.grid("vyntus" = 0:1, "surg" = 1:0),
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(0.0136715 * person$height + 0.3949761 * log(person$bmi) + 0.0010347 * person$height * person$sex - 0.0829085 * config["vyntus"] + 0.088169 * config["surg"] + 4.069768)
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      -21.99611 * log(person$bmi) + 0.0430605 * person$height * person$sex - 3.504218 * config["vyntus"] + 3.483406 * config["surg"] + 99.9302
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  heart_rate = function(.self, person) {
    (9.9e8 * person$height - 2.86e9 * person$bmi + 1.4e11) ^ (1 / 5)
  },
  breathing_frequency = function(.self, person) {
    (-0.037375 * person$height - 1.778892 * person$sex + 0.0134113 * person$height * person$sex - 0.3806323 * .self$surg + 16.65239) ^ (1 / 0.6)
  }
)

fontan <- diagnosis(
  surg_vyntus = c(0.7834, 0.0072, 0.1697, 0.0397),
  surg = 0.7906,
  vyntus = 0.0469,
  grid = expand.grid("vyntus" = 0:1, "surg" = 1:0),
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(0.0142453 * person$height + 0.3543394 * log(person$bmi) - 0.8410274 * person$sex + 0.3348171 * log(person$bmi) * person$sex - 0.1212021 * config["vyntus"] + 0.0948334 * config["surg"] + 3.929859)
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      -0.6682767 * person$bmi + 0.0329825 * person$height * person$sex - 4.52337 * config["vyntus"] + 3.745683 * config["surg"] + 42.38803
    })
    weighted.mean(results, .self$surg_vyntus)
  },
  heart_rate = function(.self, person) {
    (-144400.5 * person$height - 3.81e7 * person$sex + 2076971 * person$bmi * person$sex + 1.24e7 * .self$vyntus + 9.75e7) ^ (1 / 3.5)
  },
  breathing_frequency = function(.self, person) {
    exp(-0.0044619 * person$height + 0.0225936 * log(person$bmi) * person$sex - 0.0820773 * .self$surg + 4.609728)
  }
)

server <- function(input, output) {
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
        diag$vo2_ml_min(diag, person),
        diag$vo2_ml_kg_min(diag, person),
        diag$heart_rate(diag, person),
        ventilation(),
        oxygen_pulse(),
        ve_vco2_slope(),
        diag$breathing_frequency(diag, person)
      )
    )
  })
}
