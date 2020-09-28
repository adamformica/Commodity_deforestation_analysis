library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Commodity deforestation/Commodity_deforestation_analysis_GitHub/Commodity_deforestation_analysis/")

proportions <- read.csv("Data_preprocessing_and_plotting/country_deforestation_proportion.csv")

percent <- proportions %>%
  transmute(ADMIN,percent_loss_area=proportions$proportion_loss_area * 100)

break_row <- 20

percent_top <- percent[1:break_row,]

percent_bottom <- percent[(break_row+1):nrow(percent),]

percent_bottom_sum <- sum(percent_bottom$percent_loss_area)

other_row <- data.frame(ADMIN="Other",percent_loss_area=percent_bottom_sum)

percent_top_other <- bind_rows(percent_top,other_row)

percent_top_other$ADMIN <- factor(percent_top_other$ADMIN,
                                      levels=rev(percent_top_other$ADMIN))

percent_plot <- ggplot(data=percent_top_other,
                           aes(ADMIN,percent_loss_area)) +
  geom_col() +
  geom_text(aes(label = paste0(signif(percent_loss_area,2),"%")),
            hjust=0,vjust=0.5,size=3) +
  ylab("Percent global commodity-driven deforestation (2001-2015)") +
  xlab("Country") +
  coord_flip(clip = "off")

png("Figures/Country_percent_deforestation_022120.png",width=6,height=6,units="in",res=100)

print(percent_plot)

dev.off()