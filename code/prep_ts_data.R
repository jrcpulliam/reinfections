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
  require(data.table)
})

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'ts_data.csv'), # input
  file.path('pub.json'),
  file.path('%s', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

ts <- data.table(read.csv(.args[1], comment.char = '#', stringsAsFactors = FALSE)) # Use to set wave dates as >15% of wave peak

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

ts[, date := as.Date(date)]
ts[, ma_cnt := frollmean(cnt, window_days)]
ts[, ma_reinf := frollmean(reinf, window_days)]
ts[, tot := cnt + reinf + third + fourth]
ts[, ma_tot := frollmean(tot, window_days)]
ts[, elig := shift(cumsum(cnt), cutoff-1) - shift(cumsum(reinf), 1, fill = 0)] # eligible for reinfection

saveRDS(ts, file = target)
