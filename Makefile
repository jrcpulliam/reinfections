# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen,
# MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) _SARS-CoV-2 reinfection
# trends in South Africa: analysis of routine surveillance data_. _medRxiv_
# <https://www.medrxiv.org/content/10.1101/2021.11.11.21266068>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

R = Rscript $^ $@
Rstar = Rscript $^ $* $@

### DATA PREPARATION (create RDS / RData files for analysis / plotting
### from CSV files provided)

data/ts_data_for_analysis.RDS: code/prep_ts_data.R data/ts_data.csv pub.json
	${R}

data/demog_data_for_display.RData: code/prep_demog_data.R data/demog_data.csv
	${R}

all_data: data/ts_data_for_analysis.RDS data/demog_data_for_display.RData

### UTILITY FUNCTIONS

utils/emp_haz_fxn.RDS: code/empirical_hazard_fxn.R
	${R}

utils/fit_fxn_null.RData: code/fit_fxn_null.R
	${R}

utils/plotting_fxns.RData: code/plotting_fxns.R
	${R}

utils/wave_defs.RDS: code/wave_defs.R data/ts_data_for_analysis.RDS pub.json
	${R}

all_utils: utils/plotting_fxns.RData utils/emp_haz_fxn.RDS \
utils/fit_fxn_null.RData utils/plotting_fxns.RData utils/wave_defs.RDS

### DESCRIPTIVE ANALYSIS

# Figure 1
output/ts_plot.RDS output/ts_plot.png: code/ts_plot.R data/ts_data_for_analysis.RDS \
utils/wave_defs.RDS utils/plotting_fxns.RData
	${R}

# Figure 2 (panels B and C)
output/demog_plot.RDS output/demog_plot.png: code/demog_plot.R \
data/demog_data_for_display.RData
	${R}

# MORE TO BE ADDED

### APPROACH 1

# ANALYSIS TO BE ADDED

# Figure 3
output/sim_plot.RDS output/sim_plot.png: code/sim_plot.R output/sim_90_null.RDS \
data/ts_data_for_analysis.RDS pub.json utils/plotting_fxns.RData
	${R}

# Figure S4
output/convergence_plot.RDS output/convergence_plot.png: code/convergence_plot.R \
output/posterior_90_null.RData pub.json
	${R}

### APPROACH 2

# ANALYSIS TO BE ADDED

# Figure 4
output/emp_haz_plot.RDS output/emp_haz_plot.png: code/emp_haz_plot.R \
data/ts_data_for_analysis.RDS utils/emp_haz_fxn.RDS pub.json utils/wave_defs.RDS \
utils/plotting_fxns.RData
		Rscript $^ 90 $@

all_plots: output/ts_plot.png output/demog_plot.png output/sim_plot.png \
output/emp_haz_plot.png output/convergence_plot.png