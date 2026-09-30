# Human-AI Collaboration in Health Economic Evaluations
This repository contains the source code and supporting scripts for the manuscript on
Incorporating Human-AI Collaboration in Health Economic Evaluations of AI-based Prediction Models: A Modeling Framework and Simulation Study

There are eight scripts in total:
-	"f_gen_psa.R" function to generate probabilistic (sensitivity) analysis input parameters
-	"f_collab_prediction.R" helper function to calculate (con/dis)cordance and reliance for human-AI joint prediction performance
-	"f_economic_model.R" function to calculate results with a decision tree economic model
-	"f_wrapper.R" function to combine the functions of f_gen_psa and f_economic_model 
- "f_sens_spec_corr.R" helper function to correlate sensitivity and specificity
-	"f_packages.R" helper function to install and update R packages
-	"Analyses.R" to show the results of various (cost-)effectiveness analyses within the simulation study 
-	"Model validation.R" to check the technical implementation of the model

---

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Model conceptualization](#model-conceptualization)
- [Model inputs](#model-inputs)
- [Model analyses](#model-analyses)
- [Model validation](#model-validation)
- [Contact](#contact)

---

## Installation

### Prerequisites

- **R** (version 4.0.0 or later, install from https://cran.r-project.org/bin/windows/base/)
- **R Studio** (install from https://www.rstudio.com/products/rstudio/download/)
- **R packages**:
  - dampack
  - docstring
  - BCEA
  - testthat
  - mc2d
  - truncnorm
  - ggplot2
  - ggrepel
  - tidyverse
  - gridExtra
  - dplyr
  - tibble

### Steps

1.  **Clone the Repository (Terminal tab next to the Console tab in R Studio):**
    ```bash
    git clone https://github.com/mabelwieman/human-AI-collaboration.git
    ```
2.  **Install Required R Packages:**
    In R, run:
    ```r
    required_packages <- c("docstring", "dampack", "BCEA","testthat", "mc2d", "truncnorm",
              "ggplot2", "ggrepel", "tidyverse", "gridExtra", "dplyr", "tibble")
    install.packages(required_packages)
    ```
3.  **Set the Working Directory:**
    Ensure your R working directory is set to the repository folder.

## Usage

1.  **Selecting the strategy: human-AI collaboration explicitly or implicitly modeled**
    Set the human_ai_collab flags in the scripts `f_gen_psa.R`, `f_economic_model.R` and `f_wrapper.R` to FALSE (implicitly modeled) or TRUE (explicitly modeled).
    
1.  **Main results:**
    Run the script `Analyses.R` to generate the results of our simulation study.

2.  **Changing model input parameters:**
    Adjust and run the script `f_gen_psa.R` for your own health economic evaluation of an AI-based prediction model.

## Model conceptualization

We performed a simulation study to illustrate our proposed health economic modeling framework explicitly considering human-AI collaboration within model-based health economic evaluations to support informed decision-making for further research and implementation of AI-based prediction models when only AI stand-alone evidence is available. A hypothetical cohort of patients were diagnosed either via implicit modeling of human-AI collaboration (.e. assuming AI stand-alone performance evidence reflects human-AI joint performance), explicit modeling of human-AI collaboration (i.e. including concordance, discordance, and reliance parameters), or human assessment (i.e. standard of care). The three strategies were compared in terms of performance outcomes and (cost-)effectiveness.

## Model inputs

The input parameters are example inputs and several assumptions were made.
The input parameters generated through the f_gen_psa() function with deterministic values can be copied to the clipboard by: 

```r
write.table(inputs_overview, "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
```

## Model analyses

Uncertainty is examined via Monte Carlo simulations (10,000 iterations), along with various scenario analyses. Outputs include among others cost-effectiveness planes and cost-effectiveness acceptability curves.

## Model validation

The technical implementation of the model has been checked with the help of the testthat package in `Model validation.R` to ensure that the model functions logically and produces expected outcomes.

## Contact

For further information regarding this repository or the associated deliverable, please contact:

**Mabel Wieman**
- GitHub: @mabelwieman

**Bram Ramaekers**
- GitHub: @Bram-R

**Ties Bos**
- GitHub: @TiesBos