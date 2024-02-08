###################################################################################################
# This script plots the snow-covered area fraction (SCAF) as simulated by a DMBSim run.           #
# Author: Enrico Mattea (University of Fribourg)                                                  #
###################################################################################################

library(ggplot2)
library(cowplot)


path_output <- "../output/abramov"
years       <- 2012:2022

# Load all SCAFs, plot them with overlapping years.
d_all <- NULL
for (year in years) {
  
  d_cur <- read.csv(file.path(path_output, "/annual_results/mb_daily_series_glacier_", year, ".csv"))
  d_cur$year_sim <- as.character(year)
  d_cur$ts <- as.Date(d_cur$date, format = "%Y-%m-%d")
  d_all <- rbind(d_all, d_cur)
  
}

pl_all <- list()
for (year in years) {
  
  ids_plot <- which(d_all$year_sim %in% as.character(year+(-1:1)))
  d_cur <- d_all[ids_plot,]
  
  pl_all[[year - years[1] - 1]] <- ggplot(d_cur) +
    geom_line(aes(x = ts, y = gl_scaf, color = year_sim)) +
    scale_color_discrete(drop = TRUE) +
    scale_x_date(limits = c(as.Date(paste0(year-1, "/07/01")),
                            as.Date(paste0(year, "/11/01"))),
                 date_breaks = "1 month") +
    ylim(0,100) +
    theme_bw()
}

pl_out <- plot_grid(plotlist = pl_all,
                    ncol = 1, nrow = length(pl_all))
ggsave("scaf_all.pdf",
       pl_out,
       width = 20, height = 20)

