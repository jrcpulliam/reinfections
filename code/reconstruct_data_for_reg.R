# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen,
# MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) _Increased risk of
# SARS-CoV-2 reinfection associated with emergence of the Omicron variant in
# South Africa_. _medRxiv_
# <https://www.medrxiv.org/content/10.1101/2021.11.11.21266068>
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

ts <- readRDS(.args[1])[date < max(date) - 2]

emp_haz <- readRDS(.args[2]) # Empirical hazard function

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

waves <- readRDS(.args[4])

ehdt <- emp_haz(ts, eh$p_obs, eh$p_obs_2)

dat <- data.table(date = ts$date, inc_g1 = as.numeric(ts$cnt), inc_g2 =  as.numeric(ts$reinf)
                  , sus_g1 = ehdt$sus_1 + ehdt$sus_2u, sus_g2 = ehdt$sus_2)
dat[is.na(sus_g2), sus_g2 := 0]

dat <- melt(dat, id.vars = 'date', variable.factor = FALSE)[order(date)]

dat[between(date, waves[wave == 'W1', min_date], waves[wave == 'W1', max_date]), wave := 'W1']
dat[between(date, waves[wave == 'W2', min_date], waves[wave == 'W2', max_date]), wave := 'W2']
dat[between(date, waves[wave == 'W3', min_date], waves[wave == 'W3', max_date]), wave := 'W3']
dat[date > use_omicron_date, wave := 'X4']

out <- dat[!is.na(wave), .(date
                           , class = substr(variable, 1, 3)
                           , group = substr(variable, 5, 6)
                           , value, wave)]

saveRDS(out, target)
