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



# Calculate cumulative deforestation

zonal_gather_cumulative <- zonal_gather_rename %>%
  arrange(ADMIN,year) %>%
  group_by(ADMIN) %>%
  mutate(loss_area_cumulative=cumsum(loss_area)) %>%
  select(-loss_area)



# Calculate fraction of deforestation by country

zonal_country_loss_area_cumulative <- zonal_gather_cumulative %>%
  group_by(ADMIN) %>%
  filter(year==2015) %>%
  arrange(desc(loss_area_cumulative)) %>%
  select(-year) %>%
  ungroup()

# Write data for the table of fraction of deforestation possible to reduce

write.csv(zonal_country_loss_area_cumulative, "Data_preprocessing_and_plotting/zonal_country_loss_area_cumulative.csv", row.names = FALSE)



zonal_total_loss_area_cumulative <- zonal_country_loss_area_cumulative %>%
  summarize(total_loss_area=sum(loss_area_cumulative))

zonal_country_loss_area_cumulative$total_loss_area <- zonal_total_loss_area_cumulative$total_loss_area

zonal_country_loss_area_cumulative_proportion <- zonal_country_loss_area_cumulative %>%
  mutate(proportion_loss_area=loss_area_cumulative/total_loss_area) %>%
  select(-(loss_area_cumulative:total_loss_area))

write.csv(zonal_country_loss_area_cumulative_proportion,"Data_preprocessing_and_plotting/country_deforestation_proportion.csv",row.names = FALSE)



# Save country order by cumulative deforestation

country_order <- as.vector(zonal_country_loss_area_cumulative_proportion$ADMIN)

saveRDS(country_order,file="Data_preprocessing_and_plotting/country_order.rds")

zonal_gather_cumulative$ADMIN <- factor(zonal_gather_cumulative$ADMIN, levels = country_order)

countries_top <- country_order[1:20]

# Select top 20 countries for plotting

zonal_gather_top <- zonal_gather_cumulative %>%
  filter(ADMIN %in% countries_top)

zonal_gather_top$ADMIN <- factor(zonal_gather_top$ADMIN, levels = countries_top)



# plot commodity loss over time

# export resolution 850 x 850

# dev.new(width=8,height=8,noRStudioGD = TRUE)

ggplot(zonal_gather_top,aes(year,loss_area_cumulative)) + 
  geom_point() +
  facet_wrap(~ADMIN, scales = "free", ncol=4) + 
  labs(x = "Year", y = "Cumulative commodity deforestation (Mha)") +
  theme_bw()



# See the following
# https://stats.stackexchange.com/questions/11947/fitting-an-exponential-model-to-data
# https://stats.stackexchange.com/questions/183653/getting-the-right-starting-values-for-an-nls-model-in-r

# take logs of both sides and fit a linear model to estimates of log(a) and b

zonal_gather_top_years_corrected <- zonal_gather_top %>% 
  mutate(year=year-2000)

deforestation_year_nls <- zonal_gather_top_years_corrected %>% group_by(ADMIN) %>%
  do(fit_country = nls(loss_area_cumulative ~ a*b^year, data = .,))





deforestation_year_log_lm <- zonal_gather_top_years_corrected %>% group_by(ADMIN) %>%
  do(fit_country = lm(log(loss_area_cumulative) ~ log(year), data = .))

coef_log_lm <- tidy(deforestation_year_log_lm, fit_country)

coef_spread <- function(x) {
  
  coef_spread <- x %>% gather(variable,value,-(ADMIN:term)) %>%
    unite(temp,term,variable) %>%
    spread(temp, value)
  
  colnames(coef_spread) <- gsub("\\(|\\)","",colnames(coef_spread))
  
  colnames(coef_spread) <- gsub("logyear","log_year",colnames(coef_spread))
  
  colnames(coef_spread) <- gsub("\\.","_",colnames(coef_spread))
  
  return(coef_spread)
  
}

coef_log_lm_spread <- coef_spread(coef_log_lm)

zonal_gather_coef <- coef_log_lm_spread %>%
  select(ADMIN,log_year_estimate,Intercept_estimate) %>%
  left_join(zonal_gather_top_years_corrected,by="ADMIN")

deforestation_year_nls <- zonal_gather_coef %>% group_by(ADMIN) %>%
  do(fit_country = nls(loss_area_cumulative ~ a*year^b, data = ., 
                       start = list(a = exp(first(.$Intercept_estimate)), 
                                    b = first(.$log_year_estimate))))

predictions <- lapply(deforestation_year_nls$fit_country,predict)

predictions_df <- data.frame(matrix(unlist(predictions), nrow=lengths(predictions)[1]))

colnames(predictions_df) <- deforestation_year_nls$ADMIN

predictions_gather <- predictions_df %>% gather(ADMIN,prediction)

predictions_gather$ADMIN <- factor(predictions_gather$ADMIN, levels = countries_top)

years <- rep(seq(1,lengths(predictions)[1])+2000,length(predictions))

predictions_years <- predictions_gather %>% mutate(year=years)

zonal_gather_predictions <- zonal_gather_top %>% left_join(predictions_years,by=c("ADMIN","year"))

# create text labels for plots

coef_nls <- tidy(deforestation_year_nls, fit_country)

coef_nls_spread <- coef_spread(coef_nls)

labels <- paste0(signif(coef_nls_spread$a_estimate,2),"x^",signif(coef_nls_spread$b_estimate,2))

countries <- factor(countries_top, levels = countries_top)

labels_df <- data.frame(ADMIN=countries,label=labels)



cumulative_plot <- ggplot(zonal_gather_predictions,aes(year,loss_area_cumulative)) +
  facet_wrap(vars(ADMIN), scales = "free", ncol=4) + 
  labs(x = "Year", y = "Cumulative commodity-driven deforestation (Mha)") +
  geom_point() +
  geom_line(aes(year,prediction)) +
  geom_text(
    data    = labels_df,
    mapping = aes(x = -Inf, y = Inf, label = label),
    hjust   = 0,
    vjust   = 1
  ) +
  theme_bw()

png("Figures/Cumulative_deforestation_022020.png",width=8,height=8,units="in",res=100)

print(cumulative_plot)

dev.off()



# show that the log-log plot is a straight line to justify power law

coef_log_lm_spread_neg_int <- coef_log_lm_spread %>% filter(Intercept_estimate<0)
coef_log_lm_spread_pos_int <- coef_log_lm_spread %>% filter(Intercept_estimate>=0)

coef_log_lm_spread_neg_int$label <- paste0(signif(coef_log_lm_spread_neg_int$log_year_estimate,2),
                                           "x - ",abs(signif(coef_log_lm_spread_neg_int$Intercept_estimate,2)))

coef_log_lm_spread_pos_int$label <- paste0(signif(coef_log_lm_spread_pos_int$log_year_estimate,2),
                                           "x + ",signif(coef_log_lm_spread_pos_int$Intercept_estimate,2))

coef_log_lm_spread_labels <- bind_rows(coef_log_lm_spread_neg_int,coef_log_lm_spread_pos_int) %>%
  select(ADMIN,label) %>%
  arrange(ADMIN)

coef_log_lm_spread_labels$x_value <- -Inf 

coef_log_lm_spread_labels$y_value <- Inf

cumulative_log_log_plot <- ggplot(zonal_gather_top_years_corrected,aes(log(year),log(loss_area_cumulative))) +
  geom_point() +
  geom_smooth(method="lm", col = "black", size=0.5, se=FALSE) +
  facet_wrap(~ADMIN, scales = "free", ncol=4) +
  labs(x = "Log(years after 2000)", y = "Log(cumulative commodity deforestation) (Mha)") +
  geom_text(
    data    = coef_log_lm_spread_labels,
    mapping = aes(x = x_value, y = y_value, label = label),
    hjust   = "inward",
    vjust   = "inward"
  ) +
  theme_bw()

png("Figures/Cumulative_deforestation_log_log_092820.png",width=8,height=8,units="in",res=100)

print(cumulative_log_log_plot)

dev.off()
