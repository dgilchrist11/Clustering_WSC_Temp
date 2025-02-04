#Drew Gilchrist
# Jan 2025
# Create Daily df of WSC temperature gauge stations 
# Complete Kmeans Cluster Analysis 

library(tidyverse)
library(dplyr)
library(tidyhydat)
library(readxl)
library(lubridate)
library(ggplot2)


##############

#Zoo package 
# na_interp()
# Has parameter for max interp

#


# #Bring in water temp data
# #Code below is from "TW-Analysis-Focus"
setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\Clustering_WSC_Temp\\data")
mega <- read.csv("mega_2024.csv")
mega$date<-ymd(mega$date)
glimpse(mega)

mega$file %>%
  unique()

mega<- mega %>%
  filter(tw <= 40) %>%
  filter(tw>= -0.5) %>%
  filter(file != "Envcanada_ec_tw_spot_measurements") %>%
  filter(file != "QR_ProvisionalDailyValues_20151231_to_20221216")

#CREATE AVERAGE MONTHLY TEMP FOR ALL STATIONS
#WANT STATIONS IN BC, WHICH HAVE 5 YEARS OF DATA AND HAVE ATLEAST 20 MSMTS PER MONTH

#NEED META DATA TO FILTER FOR PROVINCE OF BC
meta<-tidyhydat::allstations %>%
  select(c(STATION_NUMBER, STATION_NAME, LATITUDE, LONGITUDE,PROV_TERR_STATE_LOC ))

mega_meta <- mega %>%
  left_join(meta, by = c("station_number" = "STATION_NUMBER"))
glimpse(mega_meta)

#CREATE DAILY DATA FOR CLUSTERING

daily_tw <- mega_meta %>%
  drop_na() %>% #REMOVE NA VALUES
  filter(PROV_TERR_STATE_LOC == "BC") %>% #SELECT ONLY SITES IN BC
  mutate(year = year(date),
         doy = yday(date)) %>% #CREATE YEAR AND DOY COLUMNS
  group_by(station_number, doy) %>%
  filter(n() >=10) %>% 
  summarise(mean_tw = mean(tw, na.rm = TRUE), .groups = "drop") %>%   # CALCULATE AVERAGE DOY FOR EACH STATION 
  group_by(station_number) %>%
  filter(n_distinct(doy) >= 365) %>%  # ENSURE EACH STATION HAS DATA FOR ALL DOYs
  ungroup()

#FILTER OUT REGULATED SYSTEMS 

stations_reg <- hy_stn_regulation()
daily_tw<- daily_tw %>%
  filter(station_number != '07EB002') %>% 
  left_join(stations_reg, by = c("station_number" = "STATION_NUMBER")) %>%
  filter(REGULATED == FALSE) %>%
  ungroup() %>%
  select(-c(Year_from, Year_to, REGULATED))


ggplot(daily_tw, aes(x = doy, y = mean_tw)) +
  geom_line(group = 1, color = "blue") + 
  geom_point(color = "red") + 
  facet_wrap(~ station_number) + # FACET WRAP BY STATION NUMBER
  labs(
    title = "Daily River Temperature by Station",
    x = "Day",
    y = "Average Temperature (°C)"
  ) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8) 
  )

saveRDS(daily_tw, "daily_tw.RDS")
daily_tw = readRDS("daily_tw.RDS")
glimpse(daily_tw)

# CREATE DATA FOR CLUSTERING 

cluster_data <- daily_tw %>%
  filter(doy != 366) %>%
  group_by(station_number, doy) %>%  
  pivot_wider(
    names_from = doy,       
    values_from = mean_tw   
  ) %>% ungroup() %>% 
  select(-station_number) 

glimpse(cluster_data)

# K VALUE RANGE
ks <- 1:10

# TOTAL WITHIN CLUSTER SUM OF SQUARES
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(cluster_data, centers = k, nstart = 10)
  cl$tot.withinss
})

# PLOT TOTAL CLUSTER SUM OF SQUARES 
plot(ks, tot_within_ss, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal # of Groups")
# CHOOSE K = 3 OR K = 4

######################################################################################
#TRYING K = 4
###################################################################################
#THIS IS WHERE I LEFT OFF - FEB 3
# ALEX SUGGESTED USING THE ZOO PACKAGE TO INTERPOLATE DATA TO HAVE MORE STATIONS 

set.seed(114) # SO WE CAN REPRODUCE
cl <- kmeans(cluster_data, centers = 4, nstart = 10)

#ADD ASSIGNMENTS INTO TO DATA FRAME
cluster_data$cluster <- as.factor(cl$cluster)

glimpse(cluster_data)

daily_tw_wide <-daily_tw_wide %>%
  mutate(cluster = cluster_data$cluster)
glimpse(avg_monthly_tw_wide)

# PIVOT WIDER
avg_monthly_tw_long <- norm_avg_monthly_tw_wide %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "tw"
  ) %>%
  select(station_number, month, tw, cluster)

glimpse(norm_avg_monthly_tw_long)
saveRDS(norm_avg_monthly_tw_long, "norm_avg_monthly_tw_long_K_3.RDS")


#FUNCTION CREATE DATA FRAMES OF EACH CLUSTER 
separate_cluster_data <- function(data, i) {
  cluster_data <- data %>%
    filter(.data$cluster == i) %>% 
    mutate(month = factor(month, levels = month.abb)) %>% 
    arrange(station_number, month) #ARRANGE BY MONTH
  return(cluster_data)
}

cluster_1_data <- separate_cluster_data(norm_avg_monthly_tw_long, i = 1)
cluster_2_data <- separate_cluster_data(norm_avg_monthly_tw_long, i = 2)
cluster_3_data <- separate_cluster_data(norm_avg_monthly_tw_long, i = 3)
#cluster_4_data <- separate_cluster_data(norm_avg_monthly_tw_long, i = 4)

#CREATE PLOT FOR EACH CLUSTER

plot_cluster_data <- function(data, i) {
  ggplot(data, aes(x = month, y = tw, group = station_number, color = station_number)) +
    geom_line(size = 1) +
    labs(
      title = paste("Normalized Monthly Temperature Patterns for Cluster", i),
      x = "Month",
      y = "Value",
      color = "Station"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      legend.position = "none" 
    )
}

plot_cluster_1 <- plot_cluster_data(cluster_1_data, i = 1)
plot_cluster_2 <- plot_cluster_data(cluster_2_data, i = 2)
plot_cluster_3 <- plot_cluster_data(cluster_3_data, i = 3)
#plot_cluster_4 <- plot_cluster_data(cluster_4_data, i = 4)

combined_plot <- (plot_cluster_1 / plot_cluster_2 / plot_cluster_3)
combined_plot

#CALCULATE AVERAGE THERMAL REGIME OF EACH NORMALIZED CLUSTER WHEN K = 3
norm_avg_thermal_regime_K3 <- norm_avg_monthly_tw_long %>%
  group_by(cluster, month) %>%
  summarise(mean_tw = mean(tw, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec")))

norm_avg_thermal_plot_K3 <- ggplot(norm_avg_thermal_regime_K3, aes(x = month, y = mean_tw, group = cluster, color = cluster)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2) +  
  scale_color_manual(values = c("red", "yellow", "blue"), name = "Cluster") +
  labs(
    title = "Normalized Average Thermal Regime by Cluster, K = 3",
    x = "Month",
    y = "Value"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  )

# Display the plot
norm_avg_thermal_plot_K3

meta<-tidyhydat::allstations %>% 
  select(c(STATION_NUMBER, STATION_NAME, LATITUDE, LONGITUDE,PROV_TERR_STATE_LOC )) 

merge_3_meta <- norm_avg_monthly_tw_long %>% 
  left_join(meta, by = c("station_number" = "STATION_NUMBER")) %>% 
  select(station_number, cluster, LATITUDE, LONGITUDE) %>% 
  distinct()

glimpse(merge_3_meta)

K3_meta_sf<- merge_3_meta %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview::mapview(K3_meta_sf, zcol = "cluster")


bc_map_K3 <- ggplot() +
  geom_sf(data = bc_bound, fill = "grey90", color = "black") +  
  geom_sf(data = K3_meta_sf, aes(fill = cluster), shape = 21, size = 3, stroke = 0.5, color = "black") +   
  scale_fill_manual(
    values = c("red", "yellow","blue"),  
    name = "cluster"               
  ) +
  labs(
    title = "WSC Normalized Temperature cluster K = 3",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()
bc_map_K3

avg_thermal_cluster_plus_map <- (norm_avg_thermal_plot_K3 + bc_map_K3)




  