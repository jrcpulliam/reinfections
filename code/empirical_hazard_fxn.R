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

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'emp_haz_fxn.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

emp_haz <- function(obs_ts, obs_prop, obs_prop_2, obs_prop_u, pop = sa_pop, delay = cutoff){
  out <- obs_ts[, .(date
                    , obs_1u = cnt
                    , obs_2 = reinf
                    , inc_1u = ma_cnt
                    , inc_2 = ma_reinf
  )]
  out[, row := 1:.N]
  out[1:(delay), sus_2 := shift(cumsum(obs_1u), delay-1) - shift(cumsum(obs_2), 1, fill = 0) / obs_prop_2]
  out[1:(delay-1), sus_2 := 0]
  out[1:(delay), eh_2 := fifelse(sus_2 == 0, 0, inc_2 / obs_prop_2 / sus_2)] # empirical reinfection hazard
  out[!is.na(eh_2), false_primary := 0]
  out[!is.na(eh_2), false_primary_obs := 0]
  out[!is.na(eh_2), false_primary_missed := 0]
  out[false_primary == 0, true_primary_obs := as.double(obs_1u)]
  out[, true_primary_missed := as.double(true_primary_obs / obs_prop - true_primary_obs)]
  for(ii in (delay+1):nrow(out)){
    tmp <- copy(out[1:(ii-1)])
    tmp_sus_2 <- (shift(cumsum(tmp$true_primary_obs), delay-1) - shift(cumsum(tmp$obs_2), 1, fill = 0) / obs_prop_2)[ii-1]
    out[ii, sus_2 := tmp_sus_2]
    tmp_eh_2 <- out[ii, ifelse(sus_2 == 0, 0, inc_2 / obs_prop_2 / sus_2)]
    out[ii, eh_2 := tmp_eh_2]
    out[ii, false_primary := {
      # "false primary / fp" really means "infections in individuals whose first infections were missed"
      tmp[, fp := tmp_eh_2 * (sum(shift(true_primary_missed, delay-1), na.rm = T) - sum(false_primary))]
      tmp[.N, fp]
    }]
    out[ii, false_primary_obs := false_primary * obs_prop_u]
    out[ii, true_primary_obs := obs_1u - false_primary_obs]
    out[ii, true_primary_missed := true_primary_obs / obs_prop - true_primary_obs]
  }
  out[, false_primary_missed := (1 - obs_prop_u) * false_primary] # Unnecessary
  out[, sus_1 := pop - shift(cumsum(true_primary_obs), 1, fill = 0) - shift(cumsum(true_primary_missed), 1, fill = 0)]
  out[, sus_2u := shift(cumsum(true_primary_missed), 1, fill = 0) - shift(cumsum(false_primary), 1, fill = 0)]
  out[, sus_2 := shift(cumsum(true_primary_obs), delay-1) - shift(cumsum(obs_2), 1, fill = 0) / obs_prop_2]
  out[, eh_1 := frollmean(true_primary_obs + true_primary_missed, 7) / sus_1]
  
  out[, tot_inc_g1 := true_primary_obs + true_primary_missed]
  out[, tot_inc_gu := false_primary]
  out[, tot_inc_g2 := inc_2 / obs_prop_2]
  out[, tot_inc := tot_inc_g1 + tot_inc_gu + tot_inc_g2]
  out[, ma_tot_inc := frollmean(tot_inc, 7)]
  return(out)
}

saveRDS(emp_haz, file = target)
