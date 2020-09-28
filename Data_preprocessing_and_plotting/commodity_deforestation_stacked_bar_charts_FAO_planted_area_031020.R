# load plyr before dplyr to avoid masking of dplyr functions, e.g. summarize()
library(plyr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Commodity deforestation/Commodity_deforestation_analysis_GitHub/Commodity_deforestation_analysis/")



#' 1. Load original data from FAOSTAT
#' Available from http://www.fao.org/faostat/en/#data/QC
#' Bulk download for all countries and crops available from
#' http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data.zip
#' Note: data were not uploaded to GitHub because of their large file size 

area_all_areas <- read.csv("C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Production_Crops_E_All_Data/Production_Crops_E_All_Data.csv")



#' 2. Preprocess data so it is in a format easy
#' to do dplyr analysis on.

# remove regions and special groups (below alaphabetical list of countries)

last_row <- which(area_all_areas$Area=="World")[1]-1

area_countries <- area_all_areas[1:last_row,]

# remove aggregated items (below alphabetical list of crops by country)

start_row <- which(area_countries$Item=="Cereals (Rice Milled Eqv)")[1]

end_row <- which(area_countries$Item=="Vegetables Primary")[1]

aggregated_items <- unique(area_countries[start_row:end_row,]$Item)

area_unique_crops <- area_countries[!(area_countries$Item %in% aggregated_items), ]

# select only area harvested element

area_area_harvested <- area_unique_crops[area_unique_crops$Element=="Area harvested",]

# remove year flags

flag_columns <- grep("F$",names(area_area_harvested),value = TRUE)

area_unflagged <- area_area_harvested[,!(names(area_unique_crops) %in% flag_columns)]

# remove years before 2001 and after 2015

area_recent <- area_unflagged[,-c(seq(8,ncol(area_unflagged)-16),ncol(area_unflagged))]

# remove unecessary description columns

area_necessary <- area_recent %>%
  select(Area:Item,Y2001:Y2015)

# remove text from year names

colnames(area_necessary) <- gsub("Y","",colnames(area_necessary))

# gather all years, i.e. convert year columns to rows

area_gather <- area_necessary %>% 
  gather(year,area_harvested,-(Area:Item))

# convert years to numeric

area_gather$year <- as.numeric(area_gather$year)



#' 3. Subset top 20 countries with highest deforestation.

country_order <- readRDS(file="country_order.rds")

country_order <- country_order[1:20]

included_countries <- unique(area_gather[area_gather$Area %in% country_order,]$Area)

excluded_countries <- country_order[!(country_order %in% included_countries)]

long_names <- c("Bolivia (Plurinational State of)",
                "Viet Nam",
                "Lao People's Democratic Republic",
                "United States of America",
                "Democratic Republic of the Congo",
                "Venezuela (Bolivarian Republic of)")

area_gather$Area <- mapvalues(area_gather$Area,long_names,excluded_countries)

area_deforestation_countries <- area_gather[area_gather$Area %in% country_order,]



#' 4. Sum each item area by country to later calculate each item's
#' proportion of the country's total area across all items.

area_country_sum <- area_deforestation_countries %>%
  group_by(Area, Item.Code) %>%
  dplyr::summarize(sum = sum(area_harvested,na.rm = TRUE)) %>%
  arrange(Area,desc(sum))

item_labels <- area_deforestation_countries %>%
  ungroup() %>%
  select(Item.Code,Item) %>%
  distinct()

area_country_labels <- area_country_sum %>%
  left_join(item_labels,by="Item.Code")



#' 5. Sum all item areas by country to later calculate each item's
#' proportion of the country's total area across all items.

area_country_total <- area_country_labels %>%
  group_by(Area) %>%
  dplyr::summarize(country_total=sum(sum,na.rm = TRUE))



#' 6. Calculate each item's proportion of the 
#' country's total area across all items.
#' Select items with >10% of the country's total area

area_country_proportion <- area_country_labels %>%
  left_join(area_country_total,by=c("Area")) %>%
  mutate(country_proportion=sum/country_total)

area_country_top <- area_country_proportion %>%
  filter(country_proportion>0.1) %>%
  select(-Item)


#' 7. Join the reclassified item areas by country and year
#' with each item's proportion of the country's total area of all items.
#' Reclassify all items with <10% of the country's total area as "Other."
#' Sum item areas across countries and years again to calculate the
#' area of items in the "Other" category.

area_deforestation_countries_top <- left_join(area_deforestation_countries,area_country_top,
                                               by=c("Area","Item.Code"))

levels(area_deforestation_countries_top$Item) <- c(levels(area_deforestation_countries_top$Item), "Other")

area_deforestation_countries_top[is.na(area_deforestation_countries_top$sum),]$Item <- "Other"

area_deforestation_countries_top_other <- area_deforestation_countries_top %>%
  group_by(Area, Item, year) %>%
  dplyr::summarize(sum = sum(area_harvested,na.rm = TRUE)) %>%
  arrange(Area,Item,year,desc(sum))



#' 8. Reformat and plot data.

area_deforestation_countries_top_other$Area <- factor(area_deforestation_countries_top_other$Area, levels = country_order)

area_deforestation_countries_top_other$sum <- area_deforestation_countries_top_other$sum / 10^6

# Write data for the table of fraction of deforestation possible to reduce

write.csv(area_deforestation_countries_top_other, "Data_preprocessing_and_plotting/area_deforestation_countries_top_other.csv", row.names = FALSE)



export_plot <- ggplot(area_deforestation_countries_top_other, aes(year, sum)) +
  facet_wrap(~Area, scales = "free", ncol=4) +
  geom_area(aes(fill=Item),col="lightgray") +
  geom_text(data = filter(area_deforestation_countries_top_other,year==2009),
            aes(label = Item, group=Item, hjust=0.5),
            position = position_stack(vjust = 0.5),
            size=3,
            lineheight = .75) +
  labs(x = "Year", y = "Area harvested (Mha)") +
  coord_cartesian(clip = "off") +
  guides(fill=FALSE)

png("Figures/FAO_plot_total_area_031020.png",width=8,height=8,units="in",res=100)

print(export_plot)

dev.off()


