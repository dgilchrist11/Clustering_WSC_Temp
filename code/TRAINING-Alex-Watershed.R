library(sf)


wsc_07 <- st_read("C:/Users/DGILCHRI/Downloads/MDA_ADP_07.gpkg")

wsc_07 %>% 
  filter(StationNum %in% bc_df$STATION_NUMBER)
  
mapview::mapview(wsc_07)
