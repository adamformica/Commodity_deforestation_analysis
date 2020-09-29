library(dplyr)
library(tools)

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Commodity deforestation/Commodity_deforestation_analysis_GitHub/Commodity_deforestation_analysis/")

# load country-level and global total cumulative deforestation

zonal_country_loss_area_cumulative <- read.csv("Data_preprocessing_and_plotting/zonal_country_loss_area_cumulative.csv")

loss_area_cumulative_global <- zonal_country_loss_area_cumulative %>%
  summarize(loss_area_cumulative_sum=sum(loss_area_cumulative))

cumulative_deforestation <- zonal_country_loss_area_cumulative %>%
  slice(1:20) %>%
  mutate(loss_area_cumulative_global=loss_area_cumulative_global$loss_area_cumulative_sum)

# load area harvested

area_deforestation_countries_top_other <- read.csv("Data_preprocessing_and_plotting/area_deforestation_countries_top_other.csv")

# select top crops from 2015 and sum their area

area_harvested <- area_deforestation_countries_top_other %>%
  filter(Item!="Other",
         year==2015) %>%
  group_by(Area) %>%
  summarize(top_crop_area=sum(sum))

# join deforestation and area harvested and rename columns

deforestation_area_harvested <- left_join(cumulative_deforestation,area_harvested,by=c("ADMIN"="Area")) %>%
  rename(country=ADMIN,
         cumulative_deforestation=loss_area_cumulative,
         cumulative_deforestation_global=loss_area_cumulative_global,
         area_harvested=top_crop_area)

write.csv(deforestation_area_harvested,"Data_preprocessing_and_plotting/deforestation_area_harvested.csv",row.names = FALSE)

#' add columns for spared area and fraction of country and global deforestation
#' if fraction deforestation reduced > 1, set it to 1
#' because it's impossible to reduce more deforestation than there is
#' then drop global deforestation column

deforestation_fraction_reduced <- deforestation_area_harvested %>%
  mutate(spared_area=area_harvested*0.2,
         fraction_deforestation=spared_area/cumulative_deforestation,
         fraction_deforestation_under_one=ifelse(fraction_deforestation>1,1,fraction_deforestation),
         fraction_deforestation_global=(fraction_deforestation_under_one*cumulative_deforestation)/cumulative_deforestation_global) %>%
  select(-cumulative_deforestation_global)

# convert fractions to percent and drop fraction columns

deforestation_percent_reduced <- deforestation_fraction_reduced %>%
  mutate(percent_deforestation_under_one=fraction_deforestation_under_one*100,
         percent_deforestation_global=fraction_deforestation_global*100) %>%
  select(-contains("fraction"))

# finalize column order and headers

deforestation_percent_reduced_final <- deforestation_percent_reduced %>%
  select(country,area_harvested,spared_area,cumulative_deforestation,
         percent_deforestation_under_one,percent_deforestation_global) %>%
  mutate_at(vars(-country),funs(signif(.,2))) %>%
  mutate_at(vars(contains("percent")),funs(paste0(.,"%")))

colnames_no_underscore <- gsub("_"," ",colnames(deforestation_percent_reduced_final))

colnames_title_case <- toTitleCase(colnames_no_underscore)

#' Use =TRIM(A1), =MID(B1,4,LEN(B1)), and =C1&"," in Excel

colnames(deforestation_percent_reduced_final) <- c("Country",
                                                   "Area Harvested Widely Grown Crops (Mha)",
                                                   "Spared Area (Mha)",
                                                   "Cumulative Deforestation (Mha)",
                                                   "Percent Country-level Deforestation",
                                                   "Percent Global Deforestation")

write.csv(deforestation_percent_reduced_final,"Data_preprocessing_and_plotting/deforestation_percent_reduced_final.csv",row.names = FALSE)
