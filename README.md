# Human-AI Collaboration in Health Economic Evaluations
This repository contains the source code and supporting scripts for the manuscript on
Incorporating Human-AI Collaboration in Health Economic Evaluations of AI-based Prediction Models: A Modelling Framework and Simulation Study

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

### Steps

1.  **Clone the Repository (Terminal tab next to the Console tab in R Studio):**
    ```bash
    git clone https://github.com/mabelwieman/human-AI-collaboration.git
    ```
2.  **Install Required R Packages:**

    In R, run:
    ```r
    required_packages <- c("docstring", "dampack", "BCEA","testthat", "mc2d", "truncnorm",
              "ggplot2", "ggrepel", "tidyverse", "gridExtra")
    install.packages(required_packages)
    ```
3.  **Set the Working Directory:**
    Ensure your R working directory is set to the repository folder.

## Usage

1.  **Selecting AI as stand-alone intervention or human-AI collaboration**
    Set the human_ai_collab flags in the scripts `f_gen_psa.R`, `f_economic_model.R` and `f_wrapper.R` to FALSE or TRUE, respectively.
    
1.  **Main results:**
    Run the script `Analyses.R` to generate the results of our simulation study.

2.  **Changing model input parameters:**
    Adjust and run the script `f_gen_psa.R` for your own health economic evaluation of an AI-based prediction model.

## Model conceptualization

We performed a simulation study to illustrate our proposed health economic modeling framework for human-AI collaboration. A hypothetical cohort of patients were diagnosed either via an AI-based prediction model stand-alone intervention (i.e. ignoring human-AI collaboration), human-AI collaboration (i.e. an AI-based prediction model with human oversight, including discordance and reliance parameters), or humans alone (SoC). The three care pathways were compared in terms of performance outcomes and (cost-)effectiveness.

## Model inputs

The input parameters are example inputs and several assumptions were made.
The input parameters generated through the f_gen_psa() function with deterministic values can be copied to the clipboard by: 

```r
write.table(inputs_overview, "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
```

## Model analyses

Uncertainty is examined via Monte Carlo simulations (10,000 iterations), along with deterministic sensitivity analyses, various scenario analyses, value-of-information analysis and factor analyses. Outputs include among others cost-effectiveness planes, cost-effectiveness acceptability curves, optimal strategy plots, tornado diagrams, info-rank plots, and expected loss curves.

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