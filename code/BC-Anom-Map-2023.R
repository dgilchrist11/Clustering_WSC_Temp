# Drew Gilchrist
# January 2025

# Map of BC water temperature anomaly

# Updated to include map of stations colored by cluster
# See "Functions-NormalizedMonthlyData.R" for the newest code with maps of cluster location

library(tidyverse)
library(dplyr)
library(tidyhydat)
library(readxl)
library(lubridate)
library(ggplot2)
library(sf)
library(bcmaps)
library(mapview)

setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\Clustering_WSC_Temp\\data")
summer_TW = readRDS("summer_TW.RDS")


meta<-tidyhydat::allstations %>% 
  select(c(STATION_NUMBER, STATION_NAME, LATITUDE, LONGITUDE,PROV_TERR_STATE_LOC )) 

glimpse(meta)
glimpse(summer_TW)

merge_anom_meta <- summer_TW %>% 
  left_join(meta, by = c("station_number" = "STATION_NUMBER"))

merge_anom_meta %>% 
    filter(PROV_TERR_STATE_LOC == "BC") %>% 
  ggplot() + geom_point(aes(LONGITUDE, LATITUDE, color = anom))

anom_meta_sf<- merge_anom_meta %>% 
  filter(PROV_TERR_STATE_LOC == "BC", year == "2023") %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview::mapview(anom_meta_sf, zcol = "anom")

anom_meta_sf %>%
  ggplot() +
  geom_sf(data = bcmaps::bc_bound()) +
  geom_sf()

bc_temp_anom_2023<-mapview::mapview(anom_meta_sf, zcol = "anom", 
                 at = seq(-4,4,by=1),
                 col.regions = colorRampPalette(c("blue", "white", "red")))
bc_temp_anom_2023

# mapshot(bc_temp_anom_2023, 
#         file = "bc_temp_anom_2023.png", 
#         remove_controls = c("zoomControl", "layersControl"))



# Load BC boundary data from bcmaps
bc_bound <- bcmaps::bc_bound()


bc_map <- ggplot() +
  geom_sf(data = bc_bound, fill = "grey90", color = "black") +  
  geom_sf(data = anom_meta_sf, aes(fill = anom), shape = 21, size = 3, stroke = 0.5, color = "black") +   
  scale_fill_gradientn(
    colors = c("blue","white", "red"),  
    limits = c(-4, 4),                   
    name = "Anomaly (°C)"               
  ) +
  labs(
    title = "BC River Temperature Anomalies (2023)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()

# Save the plot as an image
ggsave("bc_temp_anom_2023.png", plot = bc_map, width = 10, height = 8, dpi = 300)


################################################################################################
#CLUSTER ANALYSIS
################################################################

setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\code\\data")
avg_monthly_tw_long_K_4 = readRDS("avg_monthly_tw_long_K_4.RDS")

merge_4_meta <- avg_monthly_tw_long_K_4 %>% 
  left_join(meta, by = c("station_number" = "STATION_NUMBER")) %>% 
  select(station_number, cluster, LATITUDE, LONGITUDE) %>% 
  distinct()
  
glimpse(merge_4_meta)

K4_meta_sf<- merge_4_meta %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview::mapview(K4_meta_sf, zcol = "cluster")


bc_map_K4 <- ggplot() +
  geom_sf(data = bc_bound, fill = "grey90", color = "black") +  
  geom_sf(data = K4_meta_sf, aes(fill = cluster), shape = 21, size = 3, stroke = 0.5, color = "black") +   
  scale_fill_manual(
    values = c("red", "yellow","blue", "purple"),  
    name = "cluster"               
  ) +
  labs(
    title = "WSC Temperature cluster K = 4",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()
bc_map_K4

# Save the plot as an image
ggsave("bc_temp_K4.png", plot = bc_map_K4, width = 10, height = 8, dpi = 300)
###################################################################
setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\code\\data")
avg_monthly_tw_long_K_5 = readRDS("avg_monthly_tw_long_K_5.RDS")


merge_5_meta <- avg_monthly_tw_long_K_5 %>% 
  left_join(meta, by = c("station_number" = "STATION_NUMBER")) %>% 
  select(station_number, cluster, LATITUDE, LONGITUDE) %>% 
  distinct()

glimpse(merge_5_meta)

K5_meta_sf<- merge_5_meta %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview::mapview(K5_meta_sf, zcol = "cluster")


bc_map_K5 <- ggplot() +
  geom_sf(data = bc_bound, fill = "grey90", color = "black") +  
  geom_sf(data = K5_meta_sf, aes(fill = cluster), shape = 21, size = 3, stroke = 0.5, color = "black") +   
  scale_fill_manual(
    values = c("red", "yellow","blue", "purple", "black"),  
    name = "cluster"               
  ) +
  labs(
    title = "WSC Temperature cluster K = 5",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()
bc_map_K5

# Save the plot as an image
ggsave("bc_temp_K5.png", plot = bc_map_K5, width = 10, height = 8, dpi = 300)




#####################################################################################
#ALEX CODE TO HELP ME GET STARTED
####################################################################################



meta %>% 
  ggplot() + geom_point(aes(LONGITUDE, LATITUDE, color = DRAINAGE_AREA_GROSS))

meta_sf %>% st_transform(3005) %>% 
  ggplot() +
  geom_sf(data = bcmaps::bc_bound()) +
  geom_sf()

install.packages("mapview")

mapview::mapview(meta_sf, zcol = "DRAINAGE_AREA_GROSS")