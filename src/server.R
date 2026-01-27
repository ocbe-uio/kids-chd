source("classes.R")
source("methods.R")
source("models.R")
source("plotting.R")

server <- function(input, output) {
  # Mapping of group IDs to display names (used in multiple plots)
  group_names <- c(simple = "Simple", moderate = "Moderate", fontan = "Fontan")

  output$results_table <- renderTable({
    group <- get(input$group)
    conf_level <- input$conf_level / 100  # Convert from percentage to decimal
    # Calculate for both sexes: 1 = Boy, 0 = Girl
    person_male <- person(sex = 1, height = input$height, bmi = input$bmi)
    person_female <- person(sex = 0, height = input$height, bmi = input$bmi)

    # Helper to get results for both sexes
    get_metric <- function(fun) {
      c(male = fun(group, person_male, conf_level), female = fun(group, person_female, conf_level))
    }

    # For metrics with confidence intervals (assume matrix with 3 columns)
    get_metric_ci <- function(fun) {
      male <- fun(group, person_male, conf_level)
      female <- fun(group, person_female, conf_level)
      list(male = male, female = female)
    }

    vo2_ml_min_results <- get_metric_ci(group$vo2_ml_min)
    vo2_ml_kg_min_results <- get_metric_ci(group$vo2_ml_kg_min)
    heart_rate_results <- get_metric_ci(group$heart_rate)
    ventilation_results <- get_metric_ci(group$ventilation)
    oxygen_pulse_results <- get_metric_ci(group$oxygen_pulse)
    ve_vco2_slope_results <- get_metric_ci(group$ve_vco2_slope)
    breathing_frequency_results <- get_metric_ci(group$breathing_frequency)

    metrics <- c(
      "Absolute VO₂ (mL/min)", "VO₂ relative to body mass (mL/kg/min)",
      "Heart rate (BPM)", "Minute ventilation (L/min)",
      "Oxygen pulse (mL/beat)", "Ventilatory efficiency (VE/VCO₂ slope)",
      "Breathing frequency (breaths/min)"
    )

    # Format values: 1 decimal for HR and VO2 ml/min, 2 decimals for others
    value_male <- c(
      sprintf("%.1f", vo2_ml_min_results$male[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$male[1, 1]),
      sprintf("%.1f", heart_rate_results$male[1, 1]),
      sprintf("%.2f", ventilation_results$male[1, 1]),
      sprintf("%.2f", oxygen_pulse_results$male[1, 1]),
      sprintf("%.2f", ve_vco2_slope_results$male[1, 1]),
      sprintf("%.2f", breathing_frequency_results$male[1, 1])
    )
    value_female <- c(
      sprintf("%.1f", vo2_ml_min_results$female[1, 1]),
      sprintf("%.2f", vo2_ml_kg_min_results$female[1, 1]),
      sprintf("%.1f", heart_rate_results$female[1, 1]),
      sprintf("%.2f", ventilation_results$female[1, 1]),
      sprintf("%.2f", oxygen_pulse_results$female[1, 1]),
      sprintf("%.2f", ve_vco2_slope_results$female[1, 1]),
      sprintf("%.2f", breathing_frequency_results$female[1, 1])
    )
    # Format confidence intervals with appropriate precision
    format_ci <- function(mat, digits = 2) {
      if (is.null(mat) || any(is.na(mat[1, 2:3]))) {
      return(NA)
      }
      paste(
      formatC(mat[1, 2], width = 6, format = "f", digits = digits, flag = "#"),
      "-",
      formatC(mat[1, 3], width = 6, format = "f", digits = digits, flag = "#")
      )
    }

    ci_male <- c(
      format_ci(vo2_ml_min_results$male, digits = 1),
      format_ci(vo2_ml_kg_min_results$male, digits = 2),
      format_ci(heart_rate_results$male, digits = 1),
      format_ci(ventilation_results$male, digits = 2),
      format_ci(oxygen_pulse_results$male, digits = 2),
      NA,
      format_ci(breathing_frequency_results$male, digits = 2)
    )
    ci_female <- c(
      format_ci(vo2_ml_min_results$female, digits = 1),
      format_ci(vo2_ml_kg_min_results$female, digits = 2),
      format_ci(heart_rate_results$female, digits = 1),
      format_ci(ventilation_results$female, digits = 2),
      format_ci(oxygen_pulse_results$female, digits = 2),
      NA,
      format_ci(breathing_frequency_results$female, digits = 2)
    )

      # Create the data frame with fixed columns
      df <- data.frame(
        "Metric" = metrics,
        "Value Boy" = value_male,
        "Value Girl" = value_female,
        check.names = FALSE
      )
      # Add CI columns with dynamic names
      ci_boy_col <- sprintf("%d%% CI Boy", input$conf_level)
      ci_girl_col <- sprintf("%d%% CI Girl", input$conf_level)
      df[[ci_boy_col]] <- ci_male
      df[[ci_girl_col]] <- ci_female
      # Reorder columns to match original intent
      df <- df[, c("Metric", "Value Boy", ci_boy_col, "Value Girl", ci_girl_col)]
      df
  }, align = "lrcrc")

  output$plots <- renderUI({
    tabsetPanel(
      tabPanel("VO₂",
        plotOutput("vo2_ml_min_plot_group"),
        plotOutput("vo2_ml_min_plot_height"),
        plotOutput("vo2_ml_min_plot_bmi")
      ),
      tabPanel("VO₂/kg",
        plotOutput("vo2_ml_kg_min_plot_group"),
        plotOutput("vo2_ml_kg_min_plot_height"),
        plotOutput("vo2_ml_kg_min_plot_bmi")
      ),
      tabPanel("HR",
        plotOutput("heart_rate_plot_group"),
        plotOutput("heart_rate_plot_height"),
        plotOutput("heart_rate_plot_bmi")
      ),
      tabPanel("VE",
        plotOutput("ventilation_plot_group"),
        plotOutput("ventilation_plot_height"),
        plotOutput("ventilation_plot_bmi")
      ),
      tabPanel("O₂ pulse",
        plotOutput("oxygen_pulse_plot_group"),
        plotOutput("oxygen_pulse_plot_height"),
        plotOutput("oxygen_pulse_plot_bmi")
      ),
      tabPanel("VE/VCO₂",
        plotOutput("ve_vco2_slope_plot_group"),
        plotOutput("ve_vco2_slope_plot_height"),
        plotOutput("ve_vco2_slope_plot_bmi")
      ),
      tabPanel("BF",
        plotOutput("breathing_frequency_plot_group"),
        plotOutput("breathing_frequency_plot_height"),
        plotOutput("breathing_frequency_plot_bmi")
      ),
      type = "pills"
    )
  })

  # VO2 plots
  output$vo2_ml_min_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$vo2_ml_min(g, p, conf_level),
      ylab = "VO2 ml/min",
      main = "VO2 by group",
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })
  output$vo2_ml_min_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$vo2_ml_min(g, p, conf_level),
      ylab = "VO2 ml/min",
      main = "VO2 by height",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })
  output$vo2_ml_min_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$vo2_ml_min(g, p, conf_level),
      ylab = "VO2 ml/min",
      main = "VO2 by BMI",
      group_label = group_names[input$group],
      legend_pos = "topleft",
      conf_level = input$conf_level / 100
    )
  })

  # VO2/kg plots
  output$vo2_ml_kg_min_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$vo2_ml_kg_min(g, p, conf_level),
      ylab = "VO2 ml/kg/min",
      main = "VO2/kg by group",
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })
  output$vo2_ml_kg_min_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$vo2_ml_kg_min(g, p, conf_level),
      ylab = "VO2 ml/kg/min",
      main = "VO2/kg by height",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })
  output$vo2_ml_kg_min_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$vo2_ml_kg_min(g, p, conf_level),
      ylab = "VO2 ml/kg/min",
      main = "VO2/kg by BMI",
      group_label = group_names[input$group],
      legend_pos = "bottomleft",
      conf_level = input$conf_level / 100
    )
  })

  # Heart rate plots
  output$heart_rate_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$heart_rate(g, p, conf_level),
      ylab = "Heart rate (BPM)",
      main = "Heart rate by group",
      legend_pos = "bottomleft",
      conf_level = input$conf_level / 100
    )
  })
  output$heart_rate_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$heart_rate(g, p, conf_level),
      ylab = "Heart rate (BPM)",
      main = "Heart rate by height",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })
  output$heart_rate_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$heart_rate(g, p, conf_level),
      ylab = "Heart rate (BPM)",
      main = "Heart rate by BMI",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })

  # Ventilation plots
  output$ventilation_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$ventilation(g, p, conf_level),
      ylab = "Ventilation (L/min)",
      main = "Ventilation by group",
      legend_pos = "bottomleft",
      conf_level = input$conf_level / 100
    )
  })
  output$ventilation_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$ventilation(g, p, conf_level),
      ylab = "Ventilation (L/min)",
      main = "Ventilation by height",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })
  output$ventilation_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$ventilation(g, p, conf_level),
      ylab = "Ventilation (L/min)",
      main = "Ventilation by BMI",
      group_label = group_names[input$group],
      legend_pos = "bottomright",
      conf_level = input$conf_level / 100
    )
  })

  # Oxygen pulse plots
  output$oxygen_pulse_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$oxygen_pulse(g, p, conf_level),
      ylab = "Oxygen pulse (mL/beat)",
      main = "Oxygen pulse by group",
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })
  output$oxygen_pulse_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$oxygen_pulse(g, p, conf_level),
      ylab = "Oxygen pulse (mL/beat)",
      main = "Oxygen pulse by height",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })
  output$oxygen_pulse_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$oxygen_pulse(g, p, conf_level),
      ylab = "Oxygen pulse (mL/beat)",
      main = "Oxygen pulse by BMI",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })

  # VE/VCO2 slope plots
  output$ve_vco2_slope_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$ve_vco2_slope(g, p, conf_level),
      ylab = "VE/VCO2 slope",
      main = "VE/VCO2 slope by group",
      show_ci = FALSE,
      legend_pos = "topleft",
      conf_level = input$conf_level / 100
    )
  })
  output$ve_vco2_slope_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$ve_vco2_slope(g, p, conf_level),
      ylab = "VE/VCO2 slope",
      main = "VE/VCO2 slope by height",
      group_label = group_names[input$group],
      show_ci = FALSE,
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })
  output$ve_vco2_slope_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$ve_vco2_slope(g, p, conf_level),
      ylab = "VE/VCO2 slope",
      main = "VE/VCO2 slope by BMI",
      group_label = group_names[input$group],
      show_ci = FALSE,
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })

  # Breathing frequency plots
  output$breathing_frequency_plot_group <- renderPlot({
    plot_metric_by_group(
      height = input$height,
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$breathing_frequency(g, p, conf_level),
      ylab = "Breathing frequency (breaths/min)",
      main = "Breathing frequency by group",
      legend_pos = "bottomleft",
      conf_level = input$conf_level / 100
    )
  })
  output$breathing_frequency_plot_height <- renderPlot({
    plot_metric_by_height(
      group = get(input$group),
      bmi = input$bmi,
      metric_fun = function(g, p, conf_level) g$breathing_frequency(g, p, conf_level),
      ylab = "Breathing frequency (breaths/min)",
      main = "Breathing frequency by height",
      group_label = group_names[input$group],
      legend_pos = "topright",
      conf_level = input$conf_level / 100
    )
  })
  output$breathing_frequency_plot_bmi <- renderPlot({
    plot_metric_by_bmi(
      group = get(input$group),
      height = input$height,
      metric_fun = function(g, p, conf_level) g$breathing_frequency(g, p, conf_level),
      ylab = "Breathing frequency (breaths/min)",
      main = "Breathing frequency by BMI",
      group_label = group_names[input$group],
      conf_level = input$conf_level / 100
    )
  })

}
