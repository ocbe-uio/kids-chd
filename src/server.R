group <- setRefClass(
  "Diagnostic group",
  fields = list(
    grid = function() expand.grid("vyntus" = 0:1, "haukeland" = 1:0), # 01, 11, 00, 10
    haukeland_vyntus = "numeric", # proportion of vyntus and surgical (see grid)
    haukeland = "numeric",  # proportion of surgical centres
    vyntus = "numeric",  # proportion of vyntus software
    vo2_ml_min = "function",
    vo2_ml_kg_min = "function",
    heart_rate = "function",
    ventilation = "function",
    oxygen_pulse = "function",
    ve_vco2_slope = "function",
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

simple <- group(
  haukeland_vyntus = c(0.3652, 0.0499, 0.5820, 0.0029),
  haukeland = 0.4151,
  vyntus = 0.0528,
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0155584 * person$height
        + 0.4371531 * log(person$bmi)
        + 0.0009139 * person$height * person$sex
        - 0.1803019 * config["vyntus"]
        + 0.102317 * config["haukeland"]
        + 3.760053
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        + 0.0983195 * person$height
        - 1.152879 * person$bmi
        + 0.0423992 * person$height * person$sex
        - 7.601633 * config["vyntus"]
        + 4.73933 * config["haukeland"]
        + 45.77055
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  heart_rate = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        + 9168804 * person$height
        + 5.13e9
      ) ^ (1 / 4.3)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ventilation = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0109834 * person$height
        - 3642.842 * (person$bmi ^ -3.6)
        - 0.4839596 * person$sex
        + 0.0038111 * person$height * person$sex
        + 2.581131
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  oxygen_pulse = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0146831 * person$height
        - 42.58742 * (person$bmi ^ -1.7)
        + 0.0009018 * person$height * person$sex
        - 0.1793433 * config["vyntus"]
        + 0.1046776 * config["haukeland"]
        + 0.2350272
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ve_vco2_slope = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        + 0.0005124 * person$height
        + 0.0170638 * log(person$bmi)
        + 0.0294384 * person$sex
        - 0.0001712 * person$height * person$sex
        - 0.0143388 * config["vyntus"]
        + 0.0126451 * config["haukeland"]
        + 0.1285197
      ) ^ (1 / -0.4)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  breathing_frequency = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        - 0.0114363 * person$height
        + 0.0007431 * person$height * person$sex
        - 0.1421088 * config["haukeland"]
        + 6.693345
      ) ^ (1 / 0.4)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  }
)

moderate <- group(
  haukeland_vyntus = c(0.2938, 0.0563, 0.6439, 0.0060),
  haukeland = 0.3501,
  vyntus = 0.0623,
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0136715 * person$height
        + 0.3949761 * log(person$bmi)
        + 0.0010347 * person$height * person$sex
        - 0.0829085 * config["vyntus"]
        + 0.088169 * config["haukeland"]
        + 4.069768
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        - 21.99611 * log(person$bmi)
        + 0.0430605 * person$height * person$sex
        - 3.504218 * config["vyntus"]
        + 3.483406 * config["haukeland"]
        + 99.9302
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  heart_rate = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        + 9.9e8 * person$height
        - 2.86e9 * person$bmi
        + 1.4e11
      ) ^ (1 / 5)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ventilation = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0118031 * person$height
        + 0.3617417 * log(person$bmi)
        - 0.3458141 * person$sex
        + 0.003166 * person$height * person$sex
        + 1.202455
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  oxygen_pulse = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0125909 * person$height
        + 0.4460709 * log(person$bmi)
        + 0.0010929 * person$height * person$sex
        - 0.0851175 * config["vyntus"]
        + 0.0796701 * config["haukeland"]
        - 1.143238
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ve_vco2_slope = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        + 0.0003922 * person$height
        - 0.0152721 * config["vyntus"]
        + 0.0171314 * config["haukeland"]
        + 0.1956142
      ) ^ (1 / -0.4)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  breathing_frequency = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        - 0.037375 * person$height
        - 1.778892 * person$sex
        + 0.0134113 * person$height * person$sex
        - 0.3806323 * config["haukeland"]
        + 16.65239
        ) ^ (1 / 0.6)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  }
)

fontan <- group(
  haukeland_vyntus = c(0.1697, 0.0397, 0.7834, 0.0072),
  haukeland = 0.2094, # TODO: check why coding here is different (config 2 is Haukeland)
  vyntus = 0.0469,
  vo2_ml_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0142453 * person$height
        + 0.3543394 * log(person$bmi)
        - 0.8410274 * person$sex
        + 0.3348171 * log(person$bmi) * person$sex
        - 0.1212021 * config["vyntus"]
        + 0.0948334 * config["haukeland"]
        + 3.929859
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  vo2_ml_kg_min = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        - 0.6682767 * person$bmi
        + 0.0329825 * person$height * person$sex
        - 4.52337 * config["vyntus"]
        + 3.745683 * config["haukeland"]
        + 42.38803
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  heart_rate = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      (
        - 144400.5 * person$height
        - 3.81e7 * person$sex
        + 2076971 * person$bmi * person$sex
        + 1.24e7 * config["vyntus"]
        + 9.75e7
      ) ^ (1 / 3.5)
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ventilation = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0131873 * person$height
        + 0.3613543 * log(person$bmi)
        + 0.0007153 * person$height * person$sex
        + 0.9744558
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  oxygen_pulse = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        + 0.0152299 * person$height
        - 0.0158716 * person$bmi
        + 0.0071081 * person$bmi * person$sex
        - 0.1669066 * config["vyntus"]
        + 0.0745283 * config["haukeland"]
        - 0.6453765
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  ve_vco2_slope = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        - 0.0058539 * person$height
        - 0.6899319 * person$sex
        + 0.0041379 * person$height * person$sex
        + 0.1335418 * config["vyntus"]
        - 0.1643959 * config["haukeland"]
        + 4.505025
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  },
  breathing_frequency = function(.self, person) {
    results = apply(.self$grid, 1, function(config) {
      exp(
        - 0.0044619 * person$height
        + 0.0225936 * log(person$bmi) * person$sex
        - 0.0820773 * config["haukeland"]
        + 4.609728
      )
    })
    weighted.mean(results, .self$haukeland_vyntus)
  }
)

server <- function(input, output) {
  output$results_table <- renderTable({
    group <- get(input$group)
    person <- person(
      sex = as.numeric(input$sex), height = input$height, bmi = input$bmi
    )
    data.frame(
      "Metric" = c(
        "VO2 ml/min", "VO2 ml/kg/min", "Heart rate", "Ventilation",
        "Oxygen pulse", "VE/VCO2 slope", "Breathing frequency"
      ),
      "Value" = c(
        group$vo2_ml_min(group, person),
        group$vo2_ml_kg_min(group, person),
        group$heart_rate(group, person),
        group$ventilation(group, person),
        group$oxygen_pulse(group, person),
        group$ve_vco2_slope(group, person),
        group$breathing_frequency(group, person)
      )
    )
  })
}
