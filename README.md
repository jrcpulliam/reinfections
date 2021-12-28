# SARS-CoV-2 reinfection trends in South Africa: analysis of routine surveillance data 

This repository provides code and data for the analyses presented in:

> Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) [_Increased risk of SARS-CoV-2 reinfection associated with emergence of the Omicron variant in South Africa_](https://www.medrxiv.org/content/10.1101/2021.11.11.21266068v2). _medRxiv_ DOI: 10.1101/2021.11.11.21266068

**The materials in this repository are made available under a CC-BY-NC 4.0 International License. See the [LICENSE](./LICENSE) file for additional information.**

Note: This repository previously contained a large simulation output file (>50 MB), which caused it to take a long time to download / clone when using a slow internet connection. This and another moderately large file have been removed from the repository but can be downloaded from elsewhere (see the [**Output files**](#output) section for more details) or recreated using the code provided.

If you have questions or comments, please contact the repository maintainer, Juliet Pulliam, at <pulliam@sun.ac.za>.

## Software requirements

- R - a statistical programming language (download links for [Windows](http://cran.r-project.org/bin/windows/base/), [Linux](http://cran.r-project.org/bin/linux/), and [MacOS](http://cran.r-project.org/bin/macosx/))
- R Studio (recommended)- a user interface for R ([download link](http://www.rstudio.com/products/rstudio/download/))

The following R packages are required to run the code in this repository (version numbers indicate the versions used for manuscript preparation):

- coda (0.19-4)
- colorspace (2.0-1)
- data.table (1.14.0)
- ggplot2 (3.3.4)
- hexbin (1.28.2)
- jsonlite (1.7.2)
- lme4 (1.1-27.1)
- Matrix (1.3-4)
- patchwork (1.1.1)

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

- `install.R` - checks for required packages and installs if not present
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

- `mcmc_fit.R` - implements MCMC fitting for approach 1
- `sim_null.R` - simulates projections from the null model for approach 1 using a simplified simulation approach
- `sim_null_dyn.R` - simulates projections from the null model for approach 1 using an approach that includes dynamical noise (Note: Not used; see file for additional information.)
- `sim_plot.R` - creates plot of observed data with model fits and projections using approach 1 (Figure 3)
- `convergence_plot.R` - creates plot of convergence diagnostics using output of the MCMC fitting procedure (Figure S4)

_Approach 2_:

- `emp_haz_plot.R` - creates empirical hazards plot using approach 2 (Figure 4)
- `reconstruct_data_for_reg.R` - creates reconstructed data set using model for approach 2, to be used in regression analysis
- `reg_out.R` - conducts Poisson regression analysis and outputs estimates of coefficients with 95% confidence intervals
- `sens_an.R` - conducts sensitivity analysis to assumptions about observation probabilities for approach 2
- `sens_an_plot.R` - creates sensitivity analysis plot (Figure S5)

## <a name="output"></a>Output files

Files listed below are **not** included in the repository but can be downloaded and placed in the `output` subdirectory:

- `posterior_90_null.RData` - posterior samples from the MCMC fitting procedure (as used in the manuscript)
    - download v1.0 - link to be added    
    - [download v2.0](https://zenodo.org/record/5745339/files/posterior_90_null_pub.RData?download=1)
- `sim_90_null.RDS` - simulation results (as used in the manuscript)
    - download v1.0 - link to be added    
    - [download v2.0](https://zenodo.org/record/5745339/files/sim_90_null_pub.RDS?download=1)

Files generated by the code in this repository will also generally be placed in this subdirectory (with exceptions as noted above).

## Releases

#### Code releases

- [v1.0](https://github.com/jrcpulliam/reinfections/releases/tag/v1.0) (associated with [version 1 of the preprint]( https://www.medrxiv.org/content/10.1101/2021.11.11.21266068v1))
- [v2.0](https://github.com/jrcpulliam/reinfections/releases/tag/v2.0) (associated with [version 2 of the preprint]( https://www.medrxiv.org/content/10.1101/2021.11.11.21266068v2))

#### Data releases

The most up-to-date data (only) are available on Zenodo at DOI: [10.5281/zenodo.5745338](https://zenodo.org/record/5745338).

- [v1.0](https://github.com/jrcpulliam/reinfections/releases/tag/v1.0) (associated with [version 1 of the preprint]( https://www.medrxiv.org/content/10.1101/2021.11.11.21266068v1))
- v2.0 DOI: [10.5281/zenodo.5745339](https://zenodo.org/record/5745339) (associated with [version 2 of the preprint]( https://www.medrxiv.org/content/10.1101/2021.11.11.21266068v2))

## Other information

The manuscript was prepared using the following configuration:

- R version: 4.0.5 (2021-03-31)
- Platform: x86_64-apple-darwin17.0 (64-bit)
- Running under: macOS Catalina 10.15.7
