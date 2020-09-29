# prepare data and plot commodity loss over time

library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Commodity deforestation/Commodity_deforestation_analysis_GitHub/Commodity_deforestation_analysis/")

zonal <- read.csv("Data_preprocessing_and_plotting/hansen_mask_zonal.csv")

# subset and rename columns

zonal_sub <- select(zonal,ADMIN,HISTO_1:HISTO_15)

colnames(zonal_sub)[2:16] <- 2001:2015



# reorder country factor levels by summed commodity loss to show countries in desired order on plot

levels(zonal_sub$ADMIN) <- c(levels(zonal_sub$ADMIN), "DRC")

zonal_sub[zonal_sub$ADMIN=="Democratic Republic of the Congo",]$ADMIN <- "DRC"

levels(zonal_sub$ADMIN) <- c(levels(zonal_sub$ADMIN), "USA")

zonal_sub[zonal_sub$ADMIN=="United States of America",]$ADMIN <- "USA"



# gather annual commodity loss data for plotting

zonal_gather <- gather(zonal_sub,"year","cells",2:16)

# convert annual commodity loss data to numeric

zonal_gather$year <- as.numeric(zonal_gather$year)

# convert cells of annual commodity loss to sq km

zonal_gather$cells <- zonal_gather$cells * 934.7356 / 1000

# Convert sq km to Mha

zonal_gather$cells <- zonal_gather$cells * 10^2/10^6

# Rename cells to loss area

zonal_gather_rename <- zonal_gather %>%
  dplyr::rename(loss_area=cells)



# Load country order by cumulative deforestation

country_order <- readRDS(file="Data_preprocessing_and_plotting/country_order.rds")

countries_top <- country_order[1:20]

# Select top 20 countries for plotting

zonal_gather_top <- zonal_gather_rename %>%
  filter(ADMIN %in% countries_top)

zonal_gather_top$ADMIN <- factor(zonal_gather_top$ADMIN, levels = countries_top)



# plot commodity loss over time

# export resolution 850 x 850

# dev.new(width=8,height=8,noRStudioGD = TRUE)

ggplot(zonal_gather_top,aes(year,loss_area)) + 
  geom_point() +
  facet_wrap(~ADMIN, scales = "free", ncol=4) + 
  labs(x = "Year", y = "Cumulative commodity deforestation (Mha)") +
  scale_x_continuous(breaks = seq(2001,2015,4)) +
  theme_bw()



zonal_gather_top_years_corrected <- zonal_gather_top %>% 
  mutate(year=year-2000)

deforestation_year_lm <- zonal_gather_top_years_corrected %>% group_by(ADMIN) %>%
  do(fit_country = lm(loss_area ~ year, data = .))

coef_lm <- tidy(deforestation_year_lm, fit_country)

coef_spread <- function(x) {
  
  coef_spread <- x %>% gather(variable,value,-(ADMIN:term)) %>%
    unite(temp,term,variable) %>%
    spread(temp, value)
  
  colnames(coef_spread) <- gsub("\\(|\\)","",colnames(coef_spread))
  
  colnames(coef_spread) <- gsub("\\.","_",colnames(coef_spread))
  
  return(coef_spread)
  
}

# create text labels for plots

coef_lm_spread <- coef_spread(coef_lm)

coef_lm_spread_labels <- paste0(signif(coef_lm_spread$year_estimate,1),
                                "x + ",signif(coef_lm_spread$Intercept_estimate,1),
                                ",\np < 0.01")

zonal_gather_top_p_value <- coef_lm_spread %>%
  select(ADMIN,year_p_value) %>%
  left_join(zonal_gather_top,by="ADMIN")

coef_lm_spread_low_p <- coef_lm_spread %>% filter(year_p_value<0.01)
coef_lm_spread_high_p <- coef_lm_spread %>% filter(year_p_value>=0.01)

coef_lm_spread_low_p$label <- paste0(signif(coef_lm_spread_low_p$year_estimate,1),
                                     "x + ",signif(coef_lm_spread_low_p$Intercept_estimate,1),
                                     ",\np < 0.01")

coef_lm_spread_high_p$label <- paste0(signif(coef_lm_spread_high_p$year_estimate,1),
                                      "x + ",signif(coef_lm_spread_high_p$Intercept_estimate,1),
                                      ",\np = ",signif(coef_lm_spread_high_p$year_p_value,1))

coef_lm_spread_labels <- bind_rows(coef_lm_spread_low_p,coef_lm_spread_high_p) %>%
  select(ADMIN,label,year_p_value) %>%
  arrange(ADMIN)

decreasing_countries <- c("Brazil")

coef_lm_spread_labels$x_value <- -Inf 

coef_lm_spread_labels$y_value <- Inf

coef_lm_spread_labels[coef_lm_spread_labels$ADMIN %in% decreasing_countries,]$y_value <- -Inf
  
annual_plot <- ggplot(zonal_gather_top_p_value,aes(year,loss_area)) + 
  geom_point() +
  geom_smooth(data = subset(zonal_gather_top_p_value, year_p_value < 0.05), method="lm", col = "black") +
  facet_wrap(~ADMIN, scales = "free", ncol=4) + 
  labs(x = "Year", y = "Annual commodity-driven deforestation (Mha)") +
  geom_text(
    data    = subset(coef_lm_spread_labels, year_p_value < 0.05),
    mapping = aes(x = x_value, y = y_value, label = label),
    hjust   = "inward",
    vjust   = "inward"
  ) +
  theme_bw()

png("Figures/Annual_deforestation_022120.png",width=8,height=8,units="in",res=100)

print(annual_plot)

dev.off()
