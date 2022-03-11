# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

suppressPackageStartupMessages({
  library(data.table)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'emp_haz_fxn.RDS'),
  file.path('pub.json'),
  file.path('utils', 'wave_defs.RDS'),
  file.path('output', 'reconstructed_dat_for_reg.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

ts <- readRDS(.args[1])

emp_haz <- readRDS(.args[2]) # Empirical hazard function

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

waves <- readRDS(.args[4])

p_obs_u <- eh$p_obs * eh$p_obs_u_scale
ehdt <- emp_haz(ts, eh$p_obs, eh$p_obs_2, p_obs_u)

dat <- ehdt[, .(date, inc_g1 = as.numeric(tot_inc_g1)
                , inc_g2 = as.numeric(obs_2 / eh$p_obs_2)
                , sus_g1 = sus_1, sus_g2 = sus_2)]

dat <- melt(dat[!is.na(sus_g2)], id.vars = 'date', variable.factor = FALSE)[order(date)]

dat[between(date, waves[wave == 'W1', min_date], waves[wave == 'W1', max_date]), wave := 'W1']
dat[between(date, waves[wave == 'W2', min_date], waves[wave == 'W2', max_date]), wave := 'W2']
dat[between(date, waves[wave == 'W3', min_date], waves[wave == 'W3', max_date]), wave := 'W3']
dat[between(date, as.Date(use_omicron_date) + 1, waves[wave == 'W4', min_date]), wave := 'X4']
dat[between(date, waves[wave == 'W4', min_date], waves[wave == 'W4', max_date]), wave := 'W4']

out <- dat[!is.na(wave), .(date
                           , class = substr(variable, 1, 3)
                           , group = substr(variable, 5, 6)
                           , value, wave)]

saveRDS(out, target)
