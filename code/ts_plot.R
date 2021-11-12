suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'wave_defs.RDS'),
  file.path('utils', 'plotting_fxns.RData'),
  file.path('output', 'ts_plot.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

ts <- readRDS(.args[1])
waves <- readRDS(.args[2])

load(.args[3])

target <- tail(.args, 1)

geom_wave <- function(ww, dt = waves){
  geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf), fill = dt[wave == ww, col], alpha = .7, data = dt[wave == ww], inherit.aes = FALSE)
}

fig1a <- (ggplot(ts) 
          + aes(x = date) 
          + geom_wave('W1')
          + geom_wave('W2')
          + geom_wave('W3')
          + geom_point(aes(y = cnt / 10^3), size = .2, alpha = .5)
          + geom_line(aes(y = ma_cnt / 10^3), size = 1)
          + ylab('Primary infections')
          + ggtitle(expression(paste('x', 10^3)))
          + scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL, limits = ts[, range(date)])
)

fig1b <- (ggplot(ts)
          + aes(x = date, y = elig / 10^5)
          + geom_line(col = '7', size = 1)
          + ylab('Population at risk\nfor reinfection')
          + ggtitle(expression(paste('x', 10^5)))
          + xlab('Specimen receipt date')
          + scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL)
)

tmp <- copy(ts)[date < min(date) + 90, reinf := NA][date < min(date) + 90, ma_reinf := NA]

fig1c <- (ggplot(tmp) 
          + aes(x = date) 
          + geom_point(aes(y = reinf), size = .2, alpha = .5, color = '4')
          + geom_line(aes(y = ma_reinf), size = 1, color = '4')
          + ylab('Reinfections')
          + xlab('Specimen receipt date')
          # + ggtitle(paste0('Positive tests of previously positive individuals (≥', cutoff ,' days later)'))
          + scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL, name = 'Specimen receipt date')
)

fig1 <- (fig1a / fig1b / fig1c 
         + plot_annotation(tag_levels = 'A')
         & theme_minimal()
         & theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
               , panel.grid.minor = element_blank())
         & scale_x_Ms() 
         & theme(panel.grid.major.x = element_blank()
                 , axis.ticks = element_blank()
                 , panel.grid.minor.x = element_line(color = 'lightgrey', size = .5)
                 , panel.grid.minor.y = element_blank()
         ) 
         & geom_vline(xintercept = as.Date('2021-01-01')
                      , linetype = 2, size = .5, color = '#111111') 
         & geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -12, hjust = 'left', nudge_x = c(0, 14))
)

if(grepl('RDS', target)){
  saveRDS(fig1, file = target)
}else{
  if(grepl('elig_plot', target)){
    fig1b <- (fig1b
              & theme_minimal()
              & theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              & scale_x_Ms() 
              & theme(panel.grid.major.x = element_blank()
                      , axis.ticks = element_blank()
                      , panel.grid.minor.x = element_line(color = 'lightgrey', size = .5)
                      , panel.grid.minor.y = element_blank()
              ) 
              & geom_vline(xintercept = as.Date('2021-01-01')
                           , linetype = 2, size = .5, color = '#111111') 
              & geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = c(0, 14))
    )
    ggsave(fig1b, filename = target, width = 15, height = 9, units = 'cm')
  }else{
    ggsave(fig1, filename = target, width = 7, height = 7)
  }
}