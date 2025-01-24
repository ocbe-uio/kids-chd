group <- setRefClass(
  "Diagnostic group",
  fields = list(
    grid = function() expand.grid("vyntus" = 0:1, "haukeland" = 1:0), # 01, 11, 00, 10
    haukeland_vyntus = "numeric", # proportion of vyntus and surgical (see grid)
    haukeland = "numeric",  # proportion of surgical centres
    vyntus = "numeric",  # proportion of vyntus software
    beta_hat = "list",
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
