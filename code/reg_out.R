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
poism <- glmer(round(inc) ~ group * wave + (1 | date) + offset(log(sus))
               , data = af, family = poisson(link = 'log')
               , control = glmerControl(optimizer ="bobyqa")
)
poisc <- confint(poism)

tab1 <- data.table(effect = rownames(coef(summary(poism))), coef(summary(poism)))
tab2 <- data.table(effect = rownames(poisc), poisc)

out <- merge(tab1, tab2, by = 'effect')

saveRDS(out, target)
