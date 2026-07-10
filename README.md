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
  - DiagrammeR
  - BCEA
  - matrixStats
  - summarytools
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
    required_packages <- c("docstring", "DiagrammeR", "dampack", "BCEA", "matrixStats", "summarytools", "matrixStats", "mc2d", "truncnorm",
              "ggplot2", "ggrepel", "tidyverse", "gridExtra")
    install.packages(required_packages)
    ```
3.  **Set the Working Directory:**
    Ensure your R working directory is set to the repository folder.

## Usage

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
write.table(f_gen_psa(n_sim = 1), "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)
```
Parameter name	Value (SE)	Distribution 
p_prev 	0.50 (0.10)	Beta
p_human_sens	0.70 (0.14)	Beta 
p_human_spec	0.70 (0.14)	Beta 
p_AI_sens 	0.80 (0.16)	Beta
p_AI_spec 
	0.80 (0.16)	Beta
rho 	-0.30 	Fixed
ρpos	0.50 (0.10)	Truncated normal
ρneg	0.50 (0.10)	Truncated normal
α	0.00 (0.20)	Normal 
β1	0.00 (0.00)	Normal
β2	0.00 (0.00)	Normal
c_scan = c_tn	0.00 (0)	Gamma
c_AI	200 (40)	Gamma
c_tp	5,000 (1,000)	Gamma 
c_fn	10,000 (2,000)	Gamma 
c_fp	1,000 (200)	Gamma
u_tp	0.80 (0.16)	Beta
u_tn	0.95 (0.19)	Beta
u_fn	0.40 (0.08)	Beta
u_fp	0.65 (0.13)	Beta 


## Model analyses

Uncertainty is examined via Monte Carlo simulations (10,000 iterations), along with deterministic sensitivity analyses, various scenario analyses, value-of-information analysis and factor analyses. Outputs include among others cost-effectiveness planes, cost-effectiveness acceptability curves, optimal strategy plots, tornado diagrams, info-rank plots, and expected loss curves.

## Model validation

The technical implementation of the model has been reviewed by a second health economist and an econometrist. Stress tests (black box testing) as well as sensitivity and scenario analyses were performed to ensure the model functions logically and produces expected outcomes.

## Contact

For further information regarding this repository or the associated deliverable, please contact:

**Mabel Wieman**
- GitHub: @mabelwieman

**Bram Ramaekers**
- GitHub: @Bram-R

**Ties Bos**
- GitHub: @TiesBos