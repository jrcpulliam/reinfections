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
  file.path('output', 'emp_haz_sens_an.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

ts <- readRDS(.args[1])

emp_haz <- readRDS(.args[2]) # Empirical hazard function

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

waves <- readRDS(.args[4])

dt <- data.table(expand.grid(p_obs = seq(.01,1,.01), p_obs_2 = seq(.01,1,.01)))

dt[, c("mean_rh", "mean_rh_W1", "mean_rh_W2", "mean_rh_W3", "mean_rh_X4", "mean_rh_W4") := rbindlist(mapply(function(a, b) {
  e_h <- emp_haz(ts, a, b, a * eh$p_obs_u_scale)
  if(any(e_h$eh_1 < 0, na.rm = TRUE) | any(e_h$eh_2 < 0, na.rm = TRUE)){
    .(NA, NA, NA, NA, NA, NA)
  }else{
    .(e_h[, mean(eh_2 / eh_1, na.rm = T)]
      , e_h[between(date, waves[wave == 'W1', min_date], waves[wave == 'W1', max_date]), mean(eh_2 / eh_1, na.rm = T)]
      , e_h[between(date, waves[wave == 'W2', min_date], waves[wave == 'W2', max_date]), mean(eh_2 / eh_1, na.rm = T)]
      , e_h[between(date, waves[wave == 'W3', min_date], waves[wave == 'W3', max_date]), mean(eh_2 / eh_1, na.rm = T)]
      , e_h[between(date, as.Date(use_omicron_date) + 1, max(date)), mean(eh_2 / eh_1, na.rm = T)]
      , e_h[between(date, waves[wave == 'W4', min_date], waves[wave == 'W4', max_date]), mean(eh_2 / eh_1, na.rm = T)]
    )
  }}, p_obs, p_obs_2, SIMPLIFY = FALSE))]

saveRDS(dt, file = target)
