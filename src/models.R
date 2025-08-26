simple <- group(
  haukeland_vyntus = c(0.3652, 0.0499, 0.5820, 0.0029),
  haukeland = c("0" = 0.5849, "1" = 0.4151),
  vyntus = c("0" = 0.9472, "1" = 0.0528),
  beta_hat = list(
    vo2_ml_min = c(
      0.0155584, 0.4371531, 0.0009139, -0.1803019, 0.102317, 3.760053
    ),
    vo2_ml_kg_min = c(
      0.0983195, -1.152879, 0.0423992, -7.601633, 4.73933, 45.77055
    ),
    heart_rate = c(9168804, 5.13e9),
    ventilation = c(0.0109834, -3642.842, -0.4839596, 0.0038111, 2.581131),
    oxygen_pulse = c(0.0146831, -42.58742, 0.0009018, -0.1793433, 0.1046776, 0.2350272),
    ve_vco2_slope = c(0.0005124, 0.0170638, 0.0294384, -0.0001712, -0.0143388, 0.0126451, 0.1285197),
    breathing_frequency = c(-0.0114363, 0.0007431, -0.1421088, 6.693345)
  ),
  sigma_beta_hat = list( # from Supplement 4
    vo2_ml_min = matrix(
      c(
        2.11e-07, -0.00001263, -1.46e-08, -9.02e-07, -1.49e-07, 5.55e-06,
        -0.00001263, 0.0018624, 6.41e-07, -0.00001219, 0.00004482, -0.00358706,
        -1.46e-08, 6.41e-07, 8.52e-09, 1.56e-07, -1.58e-08, -4.38e-07,
        -9.02e-07, -0.00001219, 1.56e-07, 7.23e-04, -9.26e-05, 1.58e-04,
        -1.49e-07, 0.00004482, -1.58e-08, -9.26e-05, 2.05e-04, -1.84e-04,
        5.55e-06, -0.00358706, -4.38e-07, 1.58e-04, -1.84e-04, 0.00986512
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    vo2_ml_kg_min = matrix(
      c(
        0.00035024, -0.00098016, -0.00002489, -0.00171601, -0.00015618, -0.03344335,
        -0.00098016, 0.00757501, 0.0000504, -0.00036311, 0.00335704, 0.0006148,
        -0.00002489, 0.0000504, 0.00001519, 0.00028271, -0.00003328, 0.00142169,
        -0.00171601, -0.00036311, 0.00028271, 1.2854802, -0.1644319, 0.24078036,
        -0.00015618, 0.00335704, -0.00003328, -0.1644319, 0.36629488, -0.17410341,
        -0.03344335, 0.0006148, 0.00142169, 0.24078036, -0.17410341, 5.2525623
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    heart_rate = matrix(
      c(
        1.07e13, -1.69e15,
        -1.69e15, 2.70e17
      ),
      nrow = 2, ncol = 2, byrow = TRUE
    ),
    ventilation = matrix(
      c(
        9.18e-07, 0.21897971, 0.00011749, -7.69e-07, -0.00014793,
        0.21897971, 271986.39, 3.8461203, -0.03392181, -41.647763,
        0.00011749, 3.8461203, 0.02404299, -0.00015287, -0.01841087,
        -7.69e-07, -0.03392181, -0.00015287, 9.84e-07, 0.00011959,
        -0.00014793, -41.647763, -0.01841087, 0.00011959, 0.02419922
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    ),
    oxygen_pulse = matrix(
      c(
        2.03e-07, 0.00110813, -1.36e-08, -7.20e-07, -2.09e-07, -3.82e-05,
        0.00110813, 13.620085, -0.00005427, 0.0019093, -0.00427112, -0.26224647,
        -1.36e-08, -0.00005427, 7.59e-09, 1.36e-07, -1.10e-08, 1.76e-06,
        -7.20e-07, 0.0019093, 1.36e-07, 0.00064898, -0.00008335, 0.00008247,
        -2.09e-07, -0.00427112, -1.10e-08, -0.00008335, 1.83e-04, -4.39e-06,
        -3.82e-05, -0.26224647, 1.76e-06, 0.00008247, -4.39e-06, 0.00770947
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    ve_vco2_slope = matrix(NA, nrow = 7, ncol = 7), # CIs impossible due to the use of multiple imputation.
    breathing_frequency = matrix(
      c(
        6.20e-07, -5.14e-08, -4.42e-07, -9.21e-05,
        -5.14e-08, 4.38e-08, 5.01e-08, 3.66e-06,
        -4.42e-07, 5.01e-08, 0.00099503, -0.00035021,
        -9.21e-05, 3.66e-06, -0.00035021, 0.01448764
      ),
      nrow = 4, ncol = 4, byrow = TRUE
    )
  ),
  vo2_ml_min = function(.self, person) {
    x <- create_x(person, sex, c(person$height, log(person$bmi), person$height * person$sex))
    y_hat_ci(x, "vo2_ml_min", .self, exp)
  },
  vo2_ml_kg_min = function(.self, person) {
    x <- create_x(person, sex, c(person$height, person$bmi, person$height * person$sex))
    y_hat_ci(x, "vo2_ml_kg_min", .self, identity)
  },
  heart_rate = function(.self, person) {
    x <- data.frame("height" = person$height)
    x <- cbind(t(x), 1) # 1 for the intercept
    transf <- function(x) x ^ (1 / 4.3)
    y(as.data.frame(x), .self$beta_hat$heart_rate, .self$haukeland_vyntus, .self$grid, transf)$detransformed_avg
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
    x <- create_x(person, sex, c(person$height, person$bmi ^ -1.7, person$height * person$sex))
    y_hat_ci(x, "oxygen_pulse", .self, exp)
  },
  ve_vco2_slope = function(.self, person) {
    x <- create_x(person, sex, c(person$height, log(person$bmi), person$sex, person$height * person$sex))
    y_hat_ci(x, "ve_vco2_slope", .self, function(x) x ^ (1 / -0.4))
  },
  breathing_frequency = function(.self, person) {
    results = apply(expand.grid(0:1), 1, function(config) {
      (
        - 0.0114363 * person$height
        + 0.0007431 * person$height * person$sex
        - 0.1421088 * config
        + 6.693345
      ) ^ (1 / 0.4)
    })
    weighted.mean(results, .self$haukeland)
  }
)

moderate <- group(
  haukeland_vyntus = c(0.2938, 0.0563, 0.6439, 0.0060),
  haukeland = 0.3501,
  vyntus = 0.0623,
  beta_hat = list(
    vo2_ml_min = c(0.0136715, 0.3949761, 0.0010347, -0.0829085, 0.088169, 4.069768),
    vo2_ml_kg_min = c(-21.99611, 0.0430605, -3.504218, 3.483406, 99.9302),
    heart_rate = c(9.9e8, -2.86e9, 1.4e11),
    ventilation = c(0.0118031, 0.3617417, -0.3458141, 0.003166, 1.202455),
    oxygen_pulse = c(0.0125909, 0.4460709, 0.0010929, -0.0851175, 0.0796701, -1.143238),
    ve_vco2_slope = c(0.0003922, -0.0152721, 0.0171314, 0.1956142),
    breathing_frequency = c(-0.037375, -1.778892, 0.0134113, -0.3806323, 16.65239)
  ),
  sigma_beta_hat = list(
    vo2_ml_min = matrix(
      c(
        3.93e-07, -0.00002769, -2.20e-08, -1.67e-06, 3.41e-07, 2.19e-05,
        -0.00002769, 0.00405631, 7.83e-07, -0.00004597, 0.00017076, -0.00769375,
        -2.20e-08, 7.83e-07, 1.82e-08, -1.90e-08, 9.34e-08, -5.73e-07,
        -1.67e-06, -0.00004597, -1.90e-08, 0.00105683, -0.00018295, 0.00038466,
        3.41e-07, 0.00017076, 9.34e-08, -0.00018295, 4.85e-04, -7.09e-04,
        2.19e-05, -0.00769375, -5.73e-07, 0.00038466, -7.09e-04, 0.01949065
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    vo2_ml_kg_min = matrix(
      c(
        3.0291717, -0.00101912, -0.21071336, 0.27370221, -8.8546093,
        -0.00101912, 0.0000226, -0.00015802, 0.0001536, 0.00085928,
        -0.21071336, -0.00015802, 1.5427521, -0.26612529, 0.61523721,
        0.27370221, 0.0001536, -0.26612529, 0.65831658, -1.0159748,
        -8.8546093, 0.00085928, 0.61523721, -1.0159748, 26.29247
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    ),
    heart_rate = matrix(
      c(
        4.80e+16, -1.68e+17, -4.28e+18,
        -1.68e+17, 1.31e+18, 1.14e+18,
        -4.28e+18, 1.14e+18, 6.59e+20
      ),
      nrow = 3, ncol = 3, byrow = TRUE
    ),
    ventilation = matrix(
      c(
        8.78e-07, -0.00003363, 0.00010017, -6.61e-07, -0.00003577,
        -0.00003363, 0.00451375, -0.00057525, 4.48e-06, -0.00806025,
        0.00010017, -0.00057525, 0.02432148, -0.0001544, -0.01388307,
        -6.61e-07, 4.48e-06, -0.0001544, 9.99e-07, 0.00008785,
        -0.00003577, -0.00806025, -0.01388307, 0.00008785, 0.02929694
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    ),
    oxygen_pulse = matrix(
      c(
        3.55e-07, -0.00002384, -1.92e-08, -1.37e-06, 3.54e-07, 1.63e-05,
        -0.00002384, 0.00349863, 6.64e-07, -0.00003482, 0.00014633, -0.00664163,
        -1.92e-08, 6.64e-07, 1.49e-08, -2.44e-08, 7.54e-08, -3.54e-07,
        -1.37e-06, -0.00003482, -2.44e-08, 0.00097854, -0.00016897, 0.0003073,
        3.54e-07, 0.00014633, 7.54e-08, -0.00016897, 4.05e-04, -6.14e-04,
        1.63e-05, -0.00664163, -3.54e-07, 0.0003073, -6.14e-04, 0.01721801
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    ve_vco2_slope = matrix(NA, nrow = 4, ncol = 4),   # CIs impossible due to the use of multiple imputation.
    breathing_frequency = matrix(
      c(
        0.00002034, 0.00308449, -0.00002022, 0.00004208, -0.0031175,
        0.00308449, 0.79718032, -0.00504493, -0.00113243, -0.47991971,
        -0.00002022, -0.00504493, 0.00003263, 0.00001023, 0.00307983,
        0.00004208, -0.00113243, 0.00001023, 0.01760227, -0.0126732,
        -0.0031175, -0.47991971, 0.00307983, -0.0126732, 0.48985943
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    )
  ),
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
  beta_hat = list(
    vo2_ml_min = c(0.0142453, 0.3543394, -0.8410274, 0.3348171, -0.1212021, 0.0948334, 3.929859),
    vo2_ml_kg_min = c(-0.6682767, 0.0329825, -4.52337, 3.745683, 42.38803),
    heart_rate = c(-144400.5, -3.81e7, 2076971, 1.24e7, 9.75e7),
    ventilation = c(0.0131873, 0.3613543, 0.0007153, 0.9744558),
    oxygen_pulse = c(0.0152299, -0.0158716, 0.0071081, -0.1669066, 0.0745283, -0.6453765),
    ve_vco2_slope = c(-0.0058539, -0.6899319, 0.0041379, 0.1335418, -0.1643959, 4.505025),
    breathing_frequency = c(-0.0044619, 0.0225936, -0.0820773, 4.609728)
  ),
  sigma_beta_hat = list(
    vo2_ml_min = matrix(
      c(
        6.93e-07, -0.00004553, 0.0000618, -0.00002303, -8.39e-06, 2.53e-06, 0.00002892,
        -0.00004553, 0.01851821, 0.04064398, -0.01371626, 0.00094428, 0.00035114, -0.0475786,
        0.0000618, 0.04064398, 0.22647517, -0.07718575, 0.00191534, -0.00098601, -0.12919257,
        -0.00002303, -0.01371626, -0.07718575, 0.02645103, -0.00059656, 0.00034461, 0.04368487,
        -8.39e-06, 0.00094428, 0.00191534, -0.00059656, 0.00375332, -0.00058942, -0.00165901,
        2.53e-06, 0.00035114, -0.00098601, 0.00034461, -0.00058942, 0.00152426, -0.00167278,
        0.00002892, -0.0475786, -0.12919257, 0.04368487, -0.00165901, -0.00167278, 0.13634202
      ),
      nrow = 7, ncol = 7, byrow = TRUE
    ),
    vo2_ml_kg_min = matrix(
      c(
        0.0182387, -0.00007529, -0.00583977, 0.0383486, -0.34244845,
        -0.00007529, 0.00004507, -0.00002117, 0.00053872, -0.00285108,
        -0.00583977, -0.00002117, 3.741276, -0.57201401, 0.0161844,
        0.0383486, 0.00053872, -0.57201401, 1.5468858, -1.0226426,
        -0.34244845, -0.00285108, 0.0161844, -1.0226426, 7.1756135
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    ),
    heart_rate = matrix(
      c(
        6.02e+09, 5.37e+11, -3.09e+10, -5.22e+10, -9.05e+11,
        5.37e+11, 1.68e+14, -8.47e+12, -2.27e+12, -8.87e+13,
        -3.09e+10, -8.47e+12, 4.65e+11, 2.07e+11, 4.65e+12,
        -5.22e+10, -2.27e+12, 2.07e+11, 3.81e+13, 5.10e+12,
        -9.05e+11, -8.87e+13, 4.65e+12, 5.10e+12, 1.44e+14
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    ),
    ventilation = matrix(
      c(
        8.79e-07, -0.00006495, -7.64e-08, 0.00006114,
        -0.00006495, 0.0118739, 4.46e-06, -0.02505763,
        -7.64e-08, 4.46e-06, 4.77e-08, -5.65e-06,
        0.00006114, -0.02505763, -5.65e-06, 0.0645076
      ),
      nrow = 4, ncol = 4, byrow = TRUE
    ),
    oxygen_pulse = matrix(
      c(
        5.94e-07, -2.35e-06, -2.91e-07, -8.00e-06, 2.91e-06, -4.45e-05,
        -2.35e-06, 0.00002616, -1.88e-07, 0.00002484, 0.00002244, -0.00013148,
        -2.91e-07, -1.88e-07, 3.13e-06, 6.95e-06, 1.48e-06, 1.35e-05,
        -8.00e-06, 0.00002484, 6.95e-06, 0.00346725, -0.00054121, 0.00060656,
        2.91e-06, 0.00002244, 1.48e-06, -0.00054121, 1.41e-03, -1.12e-03,
        -4.45e-05, -0.00013148, 1.35e-05, 0.00060656, -1.12e-03, 0.00963567
      ),
      nrow = 6, ncol = 6, byrow = TRUE
    ),
    ve_vco2_slope = matrix(NA, nrow = 6, ncol = 6),   # CIs impossible due to the use of multiple imputation.
    breathing_frequency = matrix(
      c(
        3.25e-07, -9.17e-07, 3.64e-06, -4.93e-05,
        -9.17e-07, 0.00009564, 7.00e-06, -0.0000241,
        3.64e-06, 7.00e-06, 0.00106632, -0.00077028,
        -4.93e-05, -0.0000241, -0.00077028, 0.00798718
      ),
      nrow = 4, ncol = 4, byrow = TRUE
    )
  ),
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
