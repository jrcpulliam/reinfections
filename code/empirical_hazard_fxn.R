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

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'emp_haz_fxn.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

emp_haz <- function(obs_ts, obs_prop, obs_prop_2, pop = sa_pop, delay = cutoff){
  out <- obs_ts[, .(date
                    , ma_tot
                    , inc_1 = ma_cnt
                    , inc_2 = ma_reinf
                    , sus_1 = pop - cumsum(cnt) / obs_prop
                    , sus_2u = (1-obs_prop)*cumsum(cnt) / obs_prop # unobserved first infections
                    , sus_2 = shift(cumsum(cnt), delay) - cumsum(reinf) / obs_prop_2
  )]
  out[, eh_2 := inc_2 / obs_prop_2 / sus_2]
  out[, eh_1 := (inc_1 / obs_prop - eh_2 * sus_2u) / (sus_1)]
  return(out)
}

saveRDS(emp_haz, file = target)
