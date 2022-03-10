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
})

.debug <- '.'
.args <- if (interactive()) sprintf(c(
  "%s/fit_fxn_null.RData" # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

## Fitting functions
expected <- function(parms=disease_params(), cts = ts, use_ma = TRUE, delta = cutoff) with(parms, {
  if(use_ma){
    hz <- lambda * cts$ma_tot
  }else{
    hz <- lambda * cts$tot
  }
  out <- data.frame(date = cts$date, exp_reinf = rep(0, nrow(cts)))
  for(ii in 1:(nrow(cts) - delta)){
    tmp <- cts$cnt[ii] * (1-exp(-cumsum(hz[(ii + delta):nrow(cts)])))
    out$exp_reinf[(ii + delta):nrow(cts)] <- out$exp_reinf[(ii + delta):nrow(cts)] + tmp
  }
  return(out)
}
)
# Negative log-likelihood
nllikelihood <- function(parms = disease_params(), obsDat) with(parms, {
  tmp <- expected(parms, obsDat)
  log_p <- dnbinom(obsDat$reinf, size = 1/kappa, mu = c(0, diff(tmp$exp_reinf)), log = TRUE)
  return(-sum(log_p))
})

save(expected, nllikelihood, file = target)