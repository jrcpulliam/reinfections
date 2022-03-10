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
  file.path('output', 'posterior_90_null.RData'), # input
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'fit_fxn_null.RData'),
  file.path('test.json'), # NOTE: change this to do full run!
  file.path('output', 'sim_90_null.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

load(.args[1])
ts <- readRDS(.args[2])
load(.args[3])

configpth <- .args[4]
attach(jsonlite::read_json(configpth))

if(grepl('test', .args[4]) & !grepl('test', tail(.args, 1))){
  target <- file.path('output', 'sim_90_null_test.RDS')
  warning(paste0('Changing output file to ', target, '.'))
}else{
  target <- tail(.args, 1)
}

set.seed(2021)

sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
  ex <- expected(tmp)$exp_reinf # Calculate expected reinfections using posterior
  return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
}

sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)
saveRDS(sims, file = target)
