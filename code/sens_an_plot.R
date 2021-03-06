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
  library(ggplot2)
  library(patchwork)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('output', 'emp_haz_sens_an.RDS'), # input
  file.path('utils', 'emp_haz_fxn.RDS'),
  file.path('pub.json'),
  file.path('utils', 'wave_defs.RDS'),
  file.path('output', 'emp_haz_sens_an_plot.png') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

dt <- readRDS(.args[1])

emp_haz <- readRDS(.args[2]) # Empirical hazard function

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

waves <- readRDS(.args[4])

# MEAN RELATIVE HAZARD BY WAVE
figS8a <- (ggplot(dt[between(mean_rh, 0, 1)])
           + aes(x = p_obs, y = p_obs_2, fill = mean_rh_W1)
           + geom_tile()
           + ggtitle('Wave 1')
           + theme_bw()
)

figS8b <- (ggplot(dt[between(mean_rh, 0, 1)])
           + aes(x = p_obs, y = p_obs_2, fill = mean_rh_W2)
           + geom_tile()
           + ggtitle('Wave 2')
           + theme_bw()
)

figS8c <- (ggplot(dt[between(mean_rh, 0, 1)])
           + aes(x = p_obs, y = p_obs_2, fill = mean_rh_W3)
           + geom_tile()
           + ggtitle('Wave 3')
           + theme_bw()
)

figS8_top <- ((figS8a + figS8b + figS8c) 
              + plot_layout(guides = "collect") 
              & scale_fill_continuous(limits = c(0,1), high = '5')
              & labs(fill = expression(paste(mean~lambda[2]*'/'*lambda[1])))
)

# PERCENT CHANGE RELATIVE TO WAVE 1
figS8d <- (ggplot(dt[between(mean_rh, 0, 1)])
           + aes(x = p_obs, y = p_obs_2, fill = 100*(mean_rh_W2 - mean_rh_W1)/mean_rh_W1)
           + geom_tile()
           + theme_bw()
           + ggtitle('Wave 2 v. Wave 1')
)

figS8e <- (ggplot(dt[between(mean_rh, 0, 1)])
           + aes(x = p_obs, y = p_obs_2, fill = 100*(mean_rh_W3 - mean_rh_W1)/mean_rh_W1)
           + geom_tile()
           + theme_bw()
           + ggtitle('Wave 3 v. Wave 1')
)

figS8_bottom <- ((plot_spacer() + figS8d + figS8e) 
                 + plot_layout(guides = "collect") 
                 & scale_fill_viridis_c(option = "G", limits = c(-70, 0))
                 & labs(fill='Percent\nchange')
)

min_p_obs <- 1/26
max_p_obs <- 1/6.3
min_rh <- 0.13
max_rh <- 0.19

poly_tmp <- dt[
  round(p_obs, 2) >= round(min_p_obs, 2)][
    round(mean_rh_W1,2) >= min_rh,][
      round(p_obs, 2) <= round(max_p_obs, 2)][
        round(mean_rh_W1,2) <= max_rh
      ]

poly <- rbind(
  poly_tmp[p_obs == min(p_obs)][mean_rh_W1 == min(mean_rh_W1), .(unique(p_obs), max(p_obs_2))]
  , poly_tmp[p_obs == min(p_obs)][mean_rh_W1 == max(mean_rh_W1), .(unique(p_obs), min(p_obs_2))]
  , poly_tmp[p_obs == max(p_obs)][mean_rh_W1 == max(mean_rh_W1), .(unique(p_obs), min(p_obs_2))]
  , poly_tmp[p_obs == max(p_obs)][mean_rh_W1 == min(mean_rh_W1), .(unique(p_obs), max(p_obs_2))]
  , poly_tmp[p_obs == min(p_obs)][mean_rh_W1 == min(mean_rh_W1), .(unique(p_obs), max(p_obs_2))]
)
names(poly) <- c('p_obs', 'p_obs_2')

figS8 <- (figS8_top / figS8_bottom 
          + plot_annotation(tag_levels = 'A') 
          & scale_y_continuous(minor_breaks = seq(0, 1, .05)) 
          & scale_x_continuous(minor_breaks = seq(0, 1, .05))
          & labs(x = expression(p[obs]), y = expression(p[obs[2]]))
          & geom_point(aes(x = eh$p_obs, y = eh$p_obs_2), color = 'white', shape = 4)
          & geom_abline(slope = 1, intercept = 0)
          & geom_path(aes(x = p_obs, y = p_obs_2), data = poly, color = 'white', inherit.aes = FALSE)
          & theme_minimal()
          & theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                  , panel.grid.minor = element_blank())
)

if(grepl('RDS', target)){
  saveRDS(figS8, file = target)
}else{
  ggsave(figS8, filename = target, width = 8.5, height = 5)
}
