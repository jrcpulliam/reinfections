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
  file.path('output', 'sim_90_null.RDS'), # input
  file.path('data', 'ts_data_for_analysis.RDS'),
  file.path('pub.json'),
  file.path('utils', 'plotting_fxns.RData'),
  file.path('output', 'sim_plot.png') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

sims <- readRDS(.args[1])
ts <- readRDS(.args[2])

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

load(.args[4])

target <- tail(.args, 1)

## Note that we calculated reinfections, not cumulative reinfections
eri <- data.table(date=ts$date,
                  exp_reinf=apply(sims, 1, mean),
                  low_reinf=apply(sims, 1, function(x) quantile(x, 0.025)),
                  upp_reinf=apply(sims, 1, function(x) quantile(x, 0.975)))

plot_sim <- function(dat, sim) (ggplot(dat) 
             + aes(x = date) 
             + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .2)
             + geom_line(aes(y = ma_reinf), color = '4')
             + geom_point(aes(y = reinf), size = .2, color = '4', alpha = .5)
             + geom_line(data = sim, aes(y = exp_reinf), color = 'darkgrey')
             + ylab('Reinfections')
             + xlab('Specimen receipt date')
             + geom_vline(aes(xintercept = 1 + as.Date(fit_through)), linetype = 3, color = 'red')
             # + ggtitle('Full course of epidemic')
             + theme_minimal()
             + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                     , panel.grid.minor = element_blank()
                     # , plot.title = element_text(margin = margin(t = 20, b = -20))
             )
             # + scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL)
             + scale_x_Ms(name = 'Specimen receipt date')
             + theme(panel.grid.major.x = element_blank()
                     , axis.ticks = element_blank()
                     , panel.grid.minor.x = element_line(color = 'lightgrey', size = .5)
                     , panel.grid.major.y = element_line(color = 'lightgrey', size = .5)
                     , panel.grid.minor.y = element_blank()
                     , panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
             )
             + geom_vline(xintercept = as.Date('2021-01-01')
                          , linetype = 2, size = .5, color = '#111111')
)

inc_reinf <- (plot_sim(ts, eri) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 12)
)
inc_reinf_fit <- (plot_sim(ts[between(date, '2020-06-01', fit_through)], eri[between(date, '2020-06-01', fit_through)])
                  + ggtitle('Fitting period')
                  + scale_y_continuous(breaks = seq(0,15,5), minor_breaks = seq(0,15,1), limits = c(0, 10))
                  + geom_text(aes(label = year, y = 0), data = ts[between(date, '2020-06-01', fit_through), .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -27, hjust = 'left', nudge_x = 12)
)
inc_reinf_proj <- (plot_sim(ts[date > as.Date(fit_through)], eri[date > as.Date(fit_through)])
                   + ggtitle('Projection period')
                   + geom_text(aes(label = year, y = 0), data = ts[date > '2020-12-31', .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -27, hjust = 'left', nudge_x = 12)
)

fig3 <- inc_reinf_fit + inc_reinf_proj + 
  plot_layout(widths = c(1.5, 3))

if(grepl('.RDS', target)){
  saveRDS(fig3, file = target)
}else{
  ggsave(fig3, filename = target, width = 6, height = 3)
}
