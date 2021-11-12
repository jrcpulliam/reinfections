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
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'emp_haz_fxn.RDS'),
  file.path('pub.json'),
  file.path('utils', 'wave_defs.RDS'),
  file.path('utils', 'plotting_fxns.RData'),
  '90', 
  file.path('output', 'emp_haz_plot.png') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

ts <- readRDS(.args[1])[date < max(date) - 2]

emp_haz <- readRDS(.args[2]) # Empirical hazard function

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

waves <- readRDS(.args[4])

load(.args[5])

cutoff <- as.numeric(.args[6])

ehdt <- emp_haz(ts, eh$p_obs, eh$p_obs_2)

geom_wave <- function(ww, dt = waves){
  geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf), fill = dt[wave == ww, col], alpha = .7, data = dt[wave == ww], inherit.aes = FALSE)
}

fig4a <- (ggplot(ehdt[!(is.na(eh_2) | is.na(eh_1))])
          + aes(x = date)
          + geom_wave('W1')
          + geom_wave('W2')
          + geom_wave('W3')
          + geom_line(aes(y = eh_1 / (inc_1 / eh$p_obs + inc_2 / eh$p_obs_2)), size = 1, color = 'black')
          + geom_line(aes(y = eh_2 / (inc_1 / eh$p_obs + inc_2 / eh$p_obs_2)), size = 1, color = '3')
          + xlab('Specimen receipt date')
          + ylab('Hazard coefficient')
)

fig4b <- (ggplot(ehdt[!(is.na(eh_2) | is.na(eh_1))])
          + aes(x = date, y = eh_2 / eh_1)
          + geom_line(size = 1, color = '1')
          + xlab('Specimen receipt date')
          + ylab('Hazard ratio')
          + ylim(0, NA)
)

fig4 <- (fig4a / fig4b 
         + plot_annotation(tag_levels = 'A')
         & theme_minimal()
         & theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                 , panel.grid.minor = element_blank())
         & scale_x_Ms()
         & theme(panel.grid.major.x = element_blank()
                 , axis.ticks = element_blank()
                 , panel.grid.minor.x = element_line(color = 'lightgrey', size = .5)
                 , panel.grid.minor.y = element_blank()
                 , panel.grid.major.y = element_line(color = 'lightgrey', size = .5)
         ) 
         & geom_vline(xintercept = as.Date('2021-01-01')
                      , linetype = 2, size = .5, color = '#111111') 
         & geom_text(aes(label = year, y = 0)
                     , data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year]
                     , vjust = -11, hjust = 'left', nudge_x = c(0, 14)
         )
)

if(grepl('RDS', target)){
  saveRDS(fig4, file = target)
}else{
  ggsave(fig4, filename = target, width = 5.5, height = 4)
}
