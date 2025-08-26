y <- function(x, beta_hat, weights, grid, transf) {
  UseMethod("y")
}

y.data.frame <- function(x, beta_hat, weights, grid, transf) {
  # Calculate the estimated endpoints
  # x starts as a vector of covariates and is eventually transformed into a
  # matrix with copies of the covariates on the rows and the different grid
  # configurations on the columns
  y_transformed <- as.matrix(x) %*% beta_hat
  y_detransformed <- transf(y_transformed)
  if (all(c("vyntus", "haukeland") %in% colnames(x))) {
    # x is already expanded, no need to expand here
  } else {
    y_detransformed <- expand_matrix(as.matrix(y_detransformed), nrow(grid))
  }
  list(
    "transformed" = y_transformed,
    "detransformed_avg" = weights %*% y_detransformed
  )
}

ci <- function(y_hat, X, sigma_beta_hat, weights, transf) {
  # Calculate the confidence intervals
  sigma_y_hat <- as.matrix(X) %*% sigma_beta_hat %*% t(as.matrix(X))
  ci_transformed <- as.numeric(y_hat$transformed) + cbind(
    -1.96 * sqrt(diag(sigma_y_hat)),
    1.96 * sqrt(diag(sigma_y_hat))
  )
  ci_detransformed <- transf(ci_transformed)
  weights %*% ci_detransformed
}

expand_matrix <- function(mx, times) {
  # Expand a matrix by repeating each row `times` times
  kronecker(rep(1, times), as.matrix(mx))
}

y_hat_ci <- function(x, metric, metric_data, transf) {
  beta_hat <- metric_data$beta_hat[[metric]]
  sigma_beta_hat <- metric_data$sigma_beta_hat[[metric]]
  weights <- metric_data$haukeland_vyntus
  grid <- metric_data$grid
  mat <- apply(x, 2,
    function (x) {
      # Expand x
      x_across_configs <- as.data.frame(cbind(t(x), grid, "intercept" = 1))
      y_hat_list <- y(x_across_configs, beta_hat, weights, grid, transf)
      ci <- ci(y_hat_list, x_across_configs, sigma_beta_hat, weights, transf)

      # Return vector
      cbind(y_hat_list$detransformed_avg, ci)
    }
  )
  t(mat)
}

create_x <- function(person, sex, x_expr) {
  # Capture the expression
  expr <- substitute(x_expr)
  # Build a function of person and sex with expr as the body
  x_function <- eval(bquote(function(person, sex) .(expr)))
  vapply(
    X = c(person$sex, 1 - person$sex),
    FUN = function(sex) x_function(person, sex),
    FUN.VALUE = numeric(length(x_function(person, sex)))
  )
}
