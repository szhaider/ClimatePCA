## code to prepare `my_dataset` dataset goes here

library(tidyverse)
library(rio)
library(usethis)
# theme_set(theme_light())

#Data Prep
#Preparing the meta data in excel and then loading for simplicity
#Loading the metadata as in the same sheet as in PTI for consistency
data <- import_list("data-raw/pak_metadata_climate.xlsx")
usethis::use_data(data, overwrite = TRUE)

#District Names 131 to put omitted districts as NA
districts <- data[[2]] %>%
  select(district = admin1Name)

usethis::use_data(districts, overwrite = TRUE)

#To be used as legend for further changes
legend <- data[[3]]
usethis::use_data(legend, overwrite = TRUE)


#Main data in Wide format
data_pca <- data$admin1_District %>%
  select(-(1:2), -year, district=admin1Name, -ADM2_pop)
# %>%   #Year is Na, Kep 1 district Population
#   na.omit()    #Postponing na removal for later in PCA - to have maximum districts available

data_pca <- data_pca %>%
  mutate(Access_to_improved_toilet_facilities_PSLM_2014 =
           (Access_to_improved_toilet_facilities_PSLM_2014/100),
         Access_to_improved_toilet_facilities_PSLM_2019 =
           (Access_to_improved_toilet_facilities_PSLM_2019 /100),
         Access_to_improved_toilet_facilities_PSLM_2014_2019 =
           (Access_to_improved_toilet_facilities_PSLM_2014_2019/100))


#To join the clean feature names from legend in case of climate and remove _ from var names
data_pca <- data_pca %>%
  pivot_longer(-district,  names_to = "indicator", values_to = "value") %>%
  left_join(legend %>% select(var_code, var_name), by= c("indicator"="var_code")) %>%
  select(-indicator, indicator=var_name) %>%
  pivot_wider(names_from = indicator, values_from = value)


#Exporting data for Shiny
usethis::use_data(data_pca, overwrite = TRUE)
usethis::use_data(data_pca, overwrite = TRUE, internal = TRUE)

library(sf)
#Reading in shapefiles
pak_shp <- readRDS("data-raw/pak_geometries.rds")

pak_district <- pak_shp[[2]] %>%
  dplyr::arrange(admin1Name)

usethis::use_data(pak_district, overwrite = TRUE)



