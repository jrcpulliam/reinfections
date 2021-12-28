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
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'demog_data_for_display.RData'), # input
  file.path('output', 'demog_plot.png') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

load(.args[1])

target <- tail(.args, 1)

fig2b <- (ggplot(tab_prov[province != 'UNKNOWN']) 
          + geom_bar(aes(x = province, y = (100 * reinf / total)), stat = 'identity', fill = '4a4a4a', width = .8)
          + theme_minimal()
          + ylim(0, NA)
          + geom_hline(yintercept = tab_prov[, sum(reinf) / sum(total) * 100], linetype = 2)
          + theme_minimal()
          + theme(axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                  axis.text.y = element_text(size = 10),
                  legend.title = element_text(size = 12),
                  panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
                  panel.grid.minor = element_blank()
          )
          + xlab('Province')
          + ylab('Percent')
)

tab_age_sex[, prop_noreinf := no_reinf / sum(no_reinf), by = .(sex)]
tab_age_sex[, prop_reinf := reinf / sum(reinf), by = .(sex)]

colors <- c("No reinfection" = "7", "Reinfection" = "4")

fig2c <- (ggplot(tab_age_sex[!(sex == 'U' | agegrp5 == 'UNKNOWN')][order(agegrp5)])
          + aes(x = agegrp5, group = sex, linetype = sex)
          + geom_line(aes(y = prop_noreinf, color = "No reinfection"))
          + geom_line(aes(y = prop_reinf, color = "Reinfection"))
          + theme_minimal()
          + theme(axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                  axis.text.y = element_text(size = 10),
                  legend.title = element_text(size = 12),
                  legend.position = 'bottom', legend.box = "vertical",
                  panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
                  panel.grid.minor = element_blank()
          )
          + labs(color = 'Type', linetype = 'Sex')
          + scale_color_manual(values = colors)
          + xlab('Age group (years)')
          + ylab('Proportion')
)

fig2bc <- (fig2b | fig2c 
         + plot_layout(guides = 'keep')
)

if(grepl('RDS', target)){
  saveRDS(fig2bc, file = target)
}else{
  ggsave(fig2bc, filename = target, width = 7, height = 4.5)
}