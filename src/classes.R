group <- setRefClass(
  "Diagnostic group",
  fields = list(
    grid = function() expand.grid("vyntus" = 0:1, "haukeland" = 1:0), # 01, 11, 00, 10
    haukeland_vyntus = "numeric", # proportion of vyntus and surgical (see grid)
    haukeland = "numeric",  # proportion of surgical centres
    vyntus = "numeric",  # proportion of vyntus software
    beta_hat = "list",
    sigma_beta_hat = "list",
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

y <- function(x, beta_hat, weights, grid, transf) {
  UseMethod("y")
}

y.data.frame <- function(x, beta_hat, weights, grid, transf) {
  # Calculate the estimated endpoints
  # x starts as a vector of covariates and is eventually transformed into a
  # matrix with copies of the covariates on the rows and the different grid
  # configurations on the columns
  y_weightless <- transf(as.matrix(x) %*% beta_hat)
  if (all(c("vyntus", "haukeland") %in% colnames(x))) {
    # x is already expanded, no need to expand here
  } else {
    y_weightless <- expand_matrix(as.matrix(y_weightless), nrow(grid))
  }
  weights %*% y_weightless
}

expand_matrix <- function(mx, times) {
  # Expand a matrix by repeating each row `times` times
  kronecker(rep(1, times), as.matrix(mx))
}
