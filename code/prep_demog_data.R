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
  require(data.table)
})

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'demog_data.csv'), # input
  file.path('%s', 'demog_data_for_display.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

dat <- data.table(read.csv(.args[1], comment.char = '#', stringsAsFactors = FALSE)) # Use to set wave dates as >15% of wave peak

target <- tail(.args, 1)

tab_prov <- dat[, .(no_reinf = sum(no_reinf), reinf = sum(reinf), total = sum(total)), by = province]
tab_age_sex <- dat[, .(no_reinf = sum(no_reinf), reinf = sum(reinf), total = sum(total)), by = .(agegrp5, sex)]

tmp <- cut(seq(2.5, 85, 5), c(seq(0, 80, 5), Inf))
tab_age_sex[, agegrp5 := factor(agegrp5, levels = levels(tmp))]

save(tab_prov, tab_age_sex, file = target)