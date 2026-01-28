# Kids with Congenital Heart Defects (kids-chd)

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)

A Shiny web application for calculating cardiopulmonary exercise test (CPET) endpoints for children with congenital heart defects.

## ⚠️ Development Status

**THIS APPLICATION IS UNDER DEVELOPMENT** - Results and calculations are subject to change and should be validated before clinical use.

## Overview

This application provides predicted cardiopulmonary endpoints for children with congenital heart defects based on their diagnostic group, height, and BMI. The tool calculates several key metrics including:

- **VO₂** (oxygen consumption) in ml/min and ml/kg/min
- **Heart rate** (bpm)
- **Ventilation** (l/min)
- **Oxygen pulse** (ml/beat)
- **VE/VCO₂ slope** (ventilatory efficiency)
- **Breathing frequency** (breaths/min)

Results are automatically calculated for both male and female patients, with confidence intervals for all measurements.

## Features

- Interactive web interface for selecting patient parameters
- Support for three diagnostic groups:
  - Simple defects
  - Moderate complex defects
  - Univentricular defects with Fontan circulation
- Automatic calculation for both sexes
- Confidence interval visualization
- Statistical models based on clinical data from Haukeland and Vyntus sources

## Prerequisites

- R (version 3.5 or higher recommended)
- Required R packages:
  - `shiny`
  - `rsconnect` (for deployment)

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ocbe-uio/kids-chd.git
   cd kids-chd
   ```

2. Install required R packages:
   ```R
   install.packages(c("shiny", "rsconnect"))
   ```

## Usage

### Testing

To test the application locally on your own machine:

```bash
make test
```

Or directly with R:

```R
shiny::shinyAppDir('src', options=list(launch.browser=TRUE))
```

This will launch the Shiny app in your default browser for local use and testing.

### Deployment

To deploy the application to a Shiny server:

```bash
make deploy
```

Or using R:

```R
rsconnect::deployApp('src', appName='kids-chd')
```

## Project Structure

```
.
├── LICENSE              # GNU GPL v3 license
├── Makefile            # Build automation (test and deploy commands)
├── NEWS.md             # Version history and changelog
├── README.md           # This file
├── src/                # Shiny application source code
│   ├── classes.R       # Class definitions (group, person)
│   ├── methods.R       # Calculation methods
│   ├── models.R        # Statistical models for each diagnostic group
│   ├── server.R        # Shiny server logic
│   └── ui.R            # User interface definition
└── tests/              # Test files
```

## How It Works

The application uses statistical models with pre-calculated coefficients (`beta_hat`) and covariance matrices (`sigma_beta_hat`) for each diagnostic group. When a user inputs patient parameters (height, BMI, diagnostic group), the application:

1. Creates person objects for both male and female patients
2. Applies the appropriate statistical model based on the diagnostic group
3. Calculates predicted endpoints with confidence intervals
4. Displays results with visualization

## Development

See [NEWS.md](NEWS.md) for detailed version history and recent changes.

## Contributing

This project was developed as part of OCBE's [advisory services](https://www.med.uio.no/imb/english/research/centres/ocbe/advising/about/index.html).

This is an academic/research project maintained by OCBE-UIO (Oslo Centre for Biostatistics and Epidemiology, University of Oslo).

For questions, issues, or contributions, please open an issue on the [GitHub repository](https://github.com/ocbe-uio/kids-chd).
