library(shinybusy)

ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle", timeout = 1000, color = "#337ab7", position = "full-page"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$div(
    class = "app-header",
    tags$div(
      class = "app-header-title",
      tags$h1("Kids with Congenital Heart Defects"),
      tags$p("Oslo University Hospital, Department of Paediatric Cardiology")
    ),
    tags$div(
      class = "app-header-logos",
      tags$a(
        href = "https://oslo-universitetssykehus.no",
        target = "_blank",
        rel = "noopener noreferrer",
        tags$img(
          src = "https://www.helse-sorost.no/49c8ae/contentassets/3e88e25ed18c497a925a0497575b2c78/ous/bokmal---oslo-universitetssykehus---rgb.png",
          alt = "Oslo University Hospital logo",
          title = "Oslo University Hospital",
          style = "width: 40%; float: right;"
        )
      ),
      tags$a(
        href = "https://www.uio.no",
        target = "_blank",
        rel = "noopener noreferrer",
        tags$img(
          src = "https://www.uio.no/om/designmanual/profilelementer/logo/formell-logo/03_uio_full_logo_no_pos.png",
          alt = "University of Oslo logo",
          title = "University of Oslo"
        )
      )
    )
  ),
  sidebarPanel(
      # Step 1: selecting diagnostic group and covariates
      h2("Select covariates"),
      radioButtons(
        "group", "Diagnostic group",
        choiceNames = list(
          tagList(
            "Simple defects",
            tags$span(
              style = "margin-left: 5px; cursor: pointer;",
              title = "Atrial and ventricular septal defect, Coarctation of the Aorta, Left Ventricular Outflow Tract Obstruction",
              "(?)"
            )
          ),
          tagList(
            "Moderate complex defects",
            tags$span(
              style = "margin-left: 5px; cursor: pointer;",
              title = "Tetralogy of Fallot, Transposition of the Great Arteries",
              "(?)"
            )
          ),
          "Univentricular defects with Fontan circulation"
        ),
          choiceValues = c("simple", "moderate", "fontan")
      ),
        # Removed explanatory box; tooltips are now in radioButtons
      tags$details(
        style = "margin-bottom: 12px;",
        tags$summary(
          style = "cursor: pointer; color: #337ab7;",
          "Inclusion criteria"
        ),
        tags$div(
          style = "margin-top: 8px;",
          tags$p(tags$strong("Simple")),
          tags$p("ASD (closed): Atrial septum defects of secundum type or sinus venosus type, closed by surgical or interventional procedure."),
          tags$p("VSD (closed + small open): Isolated ventricular septal defect without other major congenital cardiac malformations. The VSD should have been closed by surgical or interventional procedures. In the case of a minor open VSD considered below the closure threshold, there should be no sign of volume overload or pulmonary hypertension (Eisenmenger physiology). Concomitant ASD or right ventricular outflow tract obstruction is no exclusion criterion if the ASD has been closed and the residual RVOTO is of not more than mild degree."),
          tags$p("Aortic stenosis: Left ventricular outflow tract obstruction at the subvalvular, valvular, or supravalvular level without other major congenital cardiac malformations and aortic arch pathology."),
          tags$p("Coarctation of the Aorta: Stenosis of the aortic arch either as a hypoplastic arch or distinct coarctation. In any case, the obstruction should have been addressed by surgical or interventional procedure, or the lesion should be considered below the intervention threshold."),
          tags$p(tags$strong("Moderate")),
          tags$p("Tetralogy of Fallot: Typical anatomy surgically corrected."),
          tags$p("Transposition of the Great Arteries: Transposition of the great arteries surgically corrected by the arterial switch procedure (Jatene) and not corrected by a Mustard/Senning or Rastelli-type procedure."),
          tags$p(tags$strong("Univentricular")),
          tags$p("Fontan/TCPC: Functional univentricular circulation of any ventricular morphology without a pumping chamber supporting the pulmonary circulation.")
        )
      ),
      numericInput(
        "height", "Height (cm)",
        value = 150L, min = 0L, step = 1L, max = 210L
      ),
      numericInput(
        "weight", "Weight (kg)",
        value = 45, min = 1, step = 0.1, max = 100
      ),
      tags$div(
        role = "status",
        "aria-live" = "polite",
        textOutput("calculated_bmi")
      ),
      tags$br(),
      actionButton("submit", "Calculate endpoints"),

      # Collapsible options section
      div(
        style = "display: flex; gap: 8px; align-items: center;",
        actionButton("toggle_options", label = "Additional Options ▼", style = "background-color: #e0e0e0; color: #555; border-color: #ccc;"),
        actionButton("toggle_observed", label = "Observed data ▼", style = "background-color: #e0e0e0; color: #555; border-color: #ccc;")
      ),
      conditionalPanel(
        condition = "input.toggle_options % 2 == 1",
        div(style = "margin-top: 10px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: #f9f9f9;",
          h2("Options"),
          h3("Confidence level"),
          sliderInput(
            "conf_level", "Confidence level (%)",
            value = 95L, min = 80L, max = 99L, step = 1L,
            ticks = FALSE
          ),
          h3("Plot ranges"),
          sliderInput(
            "height_range", "Height range (cm)",
            value = c(150L, 180L), min = 100L, max = 210L, step = 1L
          ),
          sliderInput(
            "bmi_range", "BMI range (kg/m²)",
            value = c(18.5, 25), min = 5, max = 35, step = 0.5
          )
        )
      ),
      conditionalPanel(
        condition = "input.toggle_observed % 2 == 1",
        div(style = "margin-top: 10px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: #f9f9f9;",
          h2("Observed data"),
          radioButtons(
            "observed_sex", "Sex",
            choices = c("Boy" = "boy", "Girl" = "girl"),
            selected = "boy",
            inline = TRUE
          ),
          numericInput("obs_vo2_ml_min", "Absolute VO₂ (ml/min)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_vo2_ml_kg_min", "VO₂ relative to body mass (ml/kg/min)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_heart_rate", "Heart rate (BPM)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_ventilation", "Ventilation (l/min)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_oxygen_pulse", "Oxygen pulse (ml/beat)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_ve_vco2_slope", "Ventilatory efficiency (VE/VCO₂ slope)", value = NA_real_, min = 0, step = 0.1),
          numericInput("obs_breathing_frequency", "Breathing frequency (breaths/min)", value = NA_real_, min = 0, step = 0.1),
          actionButton("clear_observed", "Clear observed data")
        )
      )
  ),

  mainPanel(
    h1("⚠️ THIS PAGE IS UNDER DEVELOPMENT ⚠️"),
    # Step 3: displaying results
    conditionalPanel(
      condition = "input.submit > 0",
      wellPanel(
        h2("Results"),
        HTML("The values below are calculated based on the selected diagnostic group
        and covariates."),
        tableOutput("results_table"),
        h2("Plots"),
        HTML("The selected diagnostic group and covariates are highlighted in the plots below."),
        uiOutput("plots"),
        downloadButton("download_pdf", "Download PDF Report")
      )
    ),
    # Footer with source code link, version, and reference
    hr(),
    HTML('<p style="font-size: 0.85em; color: #666; text-align: left;">
      <a href="https://pubmed.ncbi.nlm.nih.gov/40569467/" target="_blank" rel="noopener noreferrer" style="color: #337ab7; text-decoration: none;">
        Reference:
      </a>
      Klungerbo V, Hirth A, Fredriksen PM, Holst R, Edvardsen E, Holmstrøm H, Möller T.
      Reference models for individualized assessment of cardiorespiratory fitness
      in children and adolescents with congenital heart disease:
      a retrospective multicentre study. Eur J Pediatr. 2025 Jun 26;184(7):450.
      doi: <a href="https://doi.org/10.1007/s00431-025-06270-x" target="_blank" rel="noopener noreferrer" style="color: #337ab7; text-decoration: none;">
        10.1007/s00431-025-06270-x
      </a>. PMID: 40569467; PMCID: PMC12202686.
    </p>'),
    HTML('<p style="font-size: 0.85em; color: #666; text-align: center;">
      <a href="https://github.com/ocbe-uio/kids-chd" target="_blank" rel="noopener noreferrer" style="color: #337ab7; text-decoration: none;">
        View source code on GitHub
      </a>
      &nbsp;|&nbsp;
      version 0.0.0.9048
    </p>')
  )
)
