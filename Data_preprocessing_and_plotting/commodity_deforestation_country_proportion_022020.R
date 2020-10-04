library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

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



# show that the log-log plot is a straight line to justify power law

percent_top$rank <- seq(1,nrow(percent_top))

percent_top_log_lm <- lm(log(percent_loss_area) ~ log(rank), data = percent_top)

log_lm_tidy <- tidy(percent_top_log_lm)

label <- paste0(signif(log_lm_tidy$estimate[2],2),
                 "x + ",signif(log_lm_tidy$estimate[1],2),
                "\np < 0.01")

ggplot(data=percent_top,
       aes(log(rank),log(percent_loss_area))) +
  geom_point() +
  geom_smooth(method="lm", col = "black", size=0.5, se=FALSE) +
  ylab("Log(percent global deforestation)") +
  xlab("Log(country rank)") +
  annotate("text",
           x = Inf, y = Inf, label = label,
           hjust   = 1,
           vjust   = 1) +
  theme_bw() 

ggsave("Figures/Country_percent_deforestation_log_log_plot_092920.png", width = 4, height = 3.5)
