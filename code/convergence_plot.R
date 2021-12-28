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
  library(coda)
  library(ggplot2)
  library(patchwork)
  # NOTE: Requires installation of package 'hexbin' (to use geom_hex())
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('output', 'posterior_90_null.RData'), # input
  file.path('pub.json'),
  file.path('output', 'convergence_plot.png') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

load(.args[1])

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

tmp <- data.table(rbind(
  cbind(chain = 1, iter = (mcmc$burnin + 1):mcmc$n_iter, mcmc.run$chains[[1]])
  , cbind(chain = 2, iter = (mcmc$burnin + 1):mcmc$n_iter, mcmc.run$chains[[2]])
  , cbind(chain = 3, iter = (mcmc$burnin + 1):mcmc$n_iter, mcmc.run$chains[[3]])
  , cbind(chain = 4, iter = (mcmc$burnin + 1):mcmc$n_iter, mcmc.run$chains[[4]])
))
tmp[, 'log(kappa)' := log10(kappa)]

figS4a_top <- (ggplot(tmp) 
  + aes(x = iter, y = lambda, color = as.character(chain))
  + geom_line(alpha = 0.7)
  + scale_colour_brewer(palette = "Set2")
  + labs(color = 'Chain')
  + ylab(expression(lambda))
  + theme_minimal()
  + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
          , panel.grid.minor = element_blank())
  + theme(axis.title.x=element_blank())
)

figS4a_bottom <- (ggplot(tmp) 
               + aes(x = iter, y = `log(kappa)`, color = as.character(chain))
               + geom_line(alpha = 0.7)
               + scale_colour_brewer(palette = "Set2")
               + labs(color = 'Chain')
               + xlab('iteration')
               + ylab(expression(log(kappa)))
               + theme_minimal()
               + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                       , panel.grid.minor = element_blank())
)

figS4a <- figS4a_top / figS4a_bottom + plot_layout(guides = "collect") 

gd <- gelman.diag(mcmc.run$chains)
gd$psrf <- gd$psrf[ -3,]

figS4b <- (ggplot(data.table(cbind(parameter = rownames(gd$psrf), gd$psrf)))
           + aes(x = as.numeric(`Point est.`), y = parameter)
           + geom_point()
           + xlim(min(as.numeric(gd$psrf[,1])),max(c(1.5, as.numeric(gd$psrf[,1]))))
           + xlab(expression(hat(R)))
           + ylab('parameter')
           + scale_y_discrete(labels = parse(text = rev(rownames(gd$psrf))))
           + geom_vline(aes(xintercept = 1.1), linetype = 3)
           + theme_minimal()
           + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                   , panel.grid.minor = element_blank()
                   , axis.text.y=element_text(size=14))
)

figS4_top <- figS4a | figS4b

figS4c_11 <- (ggplot(tmp) 
              + geom_density(aes(x = lambda), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda')))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)
figS4c_21 <- (ggplot(tmp) 
              + geom_hex(aes(x = lambda, y = `log(kappa)`), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda'))) 
              + ylab(parse(text = expression('log(kappa)'))) 
              + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)

figS4c_22 <- (ggplot(tmp) 
              + geom_density(aes(x = `log(kappa)`), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('log(kappa)')))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)

figS4c_31 <- (ggplot(tmp) 
              + geom_hex(aes(x = lambda, y = ll), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda'))) 
              + ylab('log likelihood') + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
)

figS4c_32 <- (ggplot(tmp) 
              + geom_hex(aes(x = `log(kappa)`, y = ll), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('log(kappa)'))) 
              + ylab('log likelihood') + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
              + theme(axis.title.y=element_blank())
)

figS4c_33 <- (ggplot(tmp) 
              + geom_density(aes(x = ll), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab('log likelihood')
)

figS4_bottom <- (figS4c_11 / figS4c_21 / figS4c_31) |
  (plot_spacer() / figS4c_22 / figS4c_32) |
  (plot_spacer() / plot_spacer() / figS4c_33) 

figS4 <- (figS4_top / figS4_bottom) + plot_annotation(tag_levels = 'A')

if(grepl('RDS', target)){
  saveRDS(figS4, file = target)
}else{
  ggsave(figS4, filename = target, width = 9, height = 7)
}
