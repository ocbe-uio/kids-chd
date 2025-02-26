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

y <- function(x, beta_hat, weights, grid, trans) {
  UseMethod("y")
}

y.numeric <- function(x, beta_hat, weights, grid, trans) {
  # Calculate the estimated endpoints
  # x starts as a vector of covariates and is eventually transformed into a
  # matrix with copies of the covariates on the rows and the different grid
  # configurations on the columns
  n_configs <- nrow(grid)
  x <- matrix(rep(x, n_configs), ncol = n_configs) # FIXME: doesn't work well for k > 1
  x <- as.matrix(cbind(t(x), grid, 1)) # 1 for the intercept
  weights %*% trans(x %*% beta_hat)
}
