# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen,
# MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) _SARS-CoV-2 reinfection
# trends in South Africa: analysis of routine surveillance data_. _medRxiv_
# <https://www.medrxiv.org/content/10.1101/2021.11.11.21266068>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

suppressPackageStartupMessages({
  library(lme4)
  library(data.table)
})

.debug <- './reinf'
.args <- if (interactive()) sprintf(c(
  file.path('output', 'reconstructed_dat_for_reg.RDS'), # input
  file.path('output', 'reg_out.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

dat <- readRDS(.args[1])

target <- tail(.args, 1)

# Make the analysis frame
af <- dcast(dat, date + group + wave ~ class, value.var = 'value')[sus > 0]

# Poisson model
poism <- glmer(inc ~ group * wave + (1 | date) + offset(log(sus))
               , data = af, family = poisson(link = 'log')
)
poisc <- confint(poism)

tab1 <- data.table(effect = rownames(coef(summary(poism))), coef(summary(poism)))
tab2 <- data.table(effect = rownames(poisc), poisc)

out <- merge(tab1, tab2, by = 'effect')

saveRDS(out, target)
