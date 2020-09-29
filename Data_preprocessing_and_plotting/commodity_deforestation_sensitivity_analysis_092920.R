library(dplyr)
library(ggplot2)

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Commodity deforestation/Commodity_deforestation_analysis_GitHub/Commodity_deforestation_analysis/")

deforestation_area_harvested <- read.csv("Data_preprocessing_and_plotting/deforestation_area_harvested.csv")

efficiency_gains <- seq(0.1,0.9,0.1)

fractions_deforestation <- vector()

for (i in 1:length(efficiency_gains)) {
  
  deforestation_fraction_reduced <- deforestation_area_harvested %>%
    mutate(spared_area=area_harvested*efficiency_gains[i],
           fraction_deforestation=spared_area/cumulative_deforestation,
           fraction_deforestation_under_one=ifelse(fraction_deforestation>1,1,fraction_deforestation),
           fraction_deforestation_global=(fraction_deforestation_under_one*cumulative_deforestation)/cumulative_deforestation_global) %>%
    select(-cumulative_deforestation_global)
  
  fractions_deforestation[i] <- sum(deforestation_fraction_reduced$fraction_deforestation_global)
  
}

efficiency_deforestation <- data.frame(efficeincy_gain=efficiency_gains,fraction_deforestation=fractions_deforestation)

efficiency_deforestation$fraction_deforestation <- round(efficiency_deforestation$fraction_deforestation,2)

ggplot(data=efficiency_deforestation,aes(efficeincy_gain,fraction_deforestation)) +
  geom_point() +
  xlab("Percent increase in agricultural efficiency") +
  ylab("Percent reduction in global deforestation") +
  scale_x_continuous(breaks = seq(0.1,0.9,0.2),
                     minor_breaks = seq(0.1,0.9,0.1),
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  theme_bw()

png("Figures/Annual_deforestation_022120.png",width=8,height=8,units="in",res=100)

print(annual_plot)

dev.off()