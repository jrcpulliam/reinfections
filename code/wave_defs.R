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

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('pub.json'),
  file.path('utils', 'wave_defs.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

ts <- readRDS(.args[1]) # Use to set wave dates as >=15% of wave peak

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

peak1 <- ts[date <= '2020-09-15', max(ma_tot, na.rm = TRUE)]
peak2 <- ts[date <= '2021-02-15', max(ma_tot, na.rm = TRUE)]
peak3 <- ts[date > '2021-02-15' & date <= '2021-10-15', max(ma_tot, na.rm = TRUE)]
peak4 <- ts[date > '2021-10-15' & date <= '2022-01-31', max(ma_tot, na.rm = TRUE)]

ts[date <= '2020-09-15' & ma_cnt >= wave_thresh * peak1, wave := 'W1']
ts[date > '2020-09-15' & date <= '2021-02-15' & ma_cnt >= wave_thresh * peak2, wave := 'W2']
ts[date > '2021-02-15' & date <= '2021-10-15' & ma_cnt >= wave_thresh * peak3, wave := 'W3']
ts[date > '2021-10-15' & date <= '2022-01-31' & ma_cnt >= wave_thresh * peak4, wave := 'W4']

waves <- ts[!is.na(wave), .(min_date = min(date), max_date = max(date)), keyby = wave]
waves[, col := c('#785EF0', '#DC267F', '#FE6100', '#07D8B4')]

saveRDS(waves, file = target)