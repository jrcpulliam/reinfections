# SARS-CoV-2 reinfection trends in South Africa: analysis of routine surveillance data 

This repository provides code and data for the analyses presented in:

> Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) [_SARS-CoV-2 reinfection trends in South Africa: analysis of routine surveillance data_](https://www.medrxiv.org/content/10.1101/2021.11.11.21266068). _medRxiv_ DOI: 10.1101/2021.11.11.21266068

**The materials in this repository are made available under a CC-BY-NC 4.0 International License. See the [LICENSE](./LICENSE) file for additional information.**

Note: This repository contains a large simulation output file (>50 MB). It may take a long time to download / clone if you have a slow internet connection. 

If you have questions or comments, please contact the repository maintainer, Juliet Pulliam, at <pulliam@sun.ac.za>.

## Software requirements

- R - a statistical programming language (download links for [Windows](http://cran.r-project.org/bin/windows/base/), [Linux](http://cran.r-project.org/bin/linux/), and [MacOS](http://cran.r-project.org/bin/macosx/))
- R Studio - a user interface for R that will be needed for computer exercises ([download link](http://www.rstudio.com/products/rstudio/download/))

The following R packages are required to run the code in this repository (version numbers indicate the versions used for manuscript preparation):

- data.table (1.14.0)
- coda (0.19-4)
- ggplot2 (3.3.4)
- patchwork (1.1.1)
- hexbin (1.28.2)
- lme4 (1.1-27.1)
- Matrix (1.3-4)
- MORE TO BE ADDED?

## Pipeline files

The files listed below are located in the main directory:

- `Makefile` - full pipeline via GNU Make (requires use of Unix-like command line); see <https://www.gnu.org/software/make/> for more information
- `pub.json` - configuration file used for manuscript preparation
- `test.json` - test configuration file (useful to see how the code works without requiring intensive computation)
- `reinfections_pub.Rproj` - R project file, which can be used for easy file navigation in interactive mode
- `LICENSE` - license information
- `README.md` - this file
- `.gitignore` - specifies files and file types for version control to ignore

## Data files

The files listed below are located in the `data` subdirectory:

- `ts_data.csv` - national daily time series of newly detected putative primary infections (`inc_1`) and suspected reinfections (`inc_2`) by specimen receipt date (`date`)
- `demog_data.csv` - counts of individuals eligible for reinfection (`total`), who have 0 suspected reinfections (`no_reinf`) or >0 suspected reinfections (`reinf`) by province (`province`), age group (5-year bands, `agegrp5`), and sex (M = Male, F = Female, U = Unknown, `sex`)

Derived data files created by certain scripts will also be placed in this subdirectory.

## Code files

The files listed below are located in the `code` subdirectory:

#### Data preparation scripts

- `prep_ts_data.R` - creates an RDS file with time series data (used in analysis / plotting scripts)
- `prep_demog_data.R` - creates an RData file with counts by province and counts by age group / sex combination (used in analysis / plotting scripts)

Files generated by these scripts will be placed in the `data` subdirectory and will be ignored by the version control system.

#### Utility scripts

- `install.R` - TO BE ADDED
- `empirical_hazard_fxn.R` - utility functions for empirical hazard estimation (approach 2)
- `fit_fxn_null.R` - utility functions for likelihood calculations (approach 1)
- `plotting_fxns.R` - utility functions for formatting plots
- `wave_defs.R` - utility functions for defining wave periods

Utility functions generated by these scripts will be placed in the `utils` subdirectory and will be ignored by the version control system.

#### Analysis and visualization scripts

_Descriptive_:

- `ts_plot.R` - creates time series plot (Figure 1)
- `demog_plot.R` - creates descriptive analysis plot (Figure 2, panels B and C)

_Approach 1_:

- `mcmc_fit.R` - TO BE ADDED
- `sim_null.R` - TO BE ADDED
- `sim_null_dyn.R` - TO BE ADDED (Note: Not used; see file for additional information.)
- `sim_plot.R` - creates plot of observed data with model fits and projections using approach 1 (Figure 3)
- `convergence_plot.R` - creates plot of convergence diagnostics using output of the MCMC fitting procedure (Figure S4)

_Approach 2_:

- `emp_haz_plot.R` - creates empirical hazards plot using approach 2 (Figure 4) from the manuscript
- `reconstruct_data_for_reg.R` - creates reconstructed data set using model for approach 2, to be used in regression analysis
- `reg_out.R` - TO BE ADDED
- `sens_an.R` - TO BE ADDED
- `sens_an_plot.R` - TO BE ADDED

## Output files

The files listed below are located in the `output` subdirectory:

- `posterior_90_null.RData` - posterior samples from the MCMC fitting procedure (as used in the manuscript)
- `sim_90_null.RDS` - simulation results (as used in the manuscript)

Files generated by the code in this repository will also generally be placed in this subdirectory (with exceptions as noted above).

## Other information

The manuscript was prepared using the following configuration:

- R version: 4.0.5 (2021-03-31)
- Platform: x86_64-apple-darwin17.0 (64-bit)
- Running under: macOS Catalina 10.15.7