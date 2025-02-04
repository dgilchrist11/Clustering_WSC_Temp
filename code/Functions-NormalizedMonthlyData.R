#Trying to edit my clustering data analysis code by using functions 
#This code creates a side by side plot of the clusters thermal regime and location on a map of BC 
#CLUSTERING NORMALIZED, MONTHLY TEMPERATURE DATA. For k = 4, 3, 2
#This did not show promising results. Will try again, using daily data.

#Drew Gilchrist
#Winter 2025 

#####################################################################################
##FIND HOMOGENOUS CLUSTERS USING K (centers) = 4 NORMALIZED 
######################################################################################
# set.seed(11) # SO WE CAN REPRODUCE
# cl <- kmeans(norm_cluster_data, centers = 4, nstart = 10)
# 
# # Add cluster assignments back to the dataset
# norm_cluster_data$cluster <- as.factor(cl$cluster)
# 
# glimpse(norm_cluster_data)
# 
# norm_avg_monthly_tw_wide <- norm_avg_monthly_tw_wide %>% 
#   mutate(cluster = norm_cluster_data$cluster)
# glimpse(norm_avg_monthly_tw_wide)
# 
# # PIVOT WIDER
# norm_avg_monthly_tw_long <- norm_avg_monthly_tw_wide %>%
#   pivot_longer(
#     cols = Jan:Dec,       
#     names_to = "month",       
#     values_to = "tw"    
#   ) %>%
#   select(station_number, month, tw, cluster)

# glimpse(norm_avg_monthly_tw_long)
# saveRDS(norm_avg_monthly_tw_long, "norm_avg_monthly_tw_long_K_4.RDS")

#########################################################################
#Clustering with 3 centers

set.seed(112) # SO WE CAN REPRODUCE
cl <- kmeans(norm_cluster_data, centers = 3, nstart = 10)

#ADD ASSIGNMENTS INTO TO DATA FRAME
norm_cluster_data$cluster <- as.factor(cl$cluster)

glimpse(norm_cluster_data)

norm_avg_monthly_tw_wide <- norm_avg_monthly_tw_wide %>%
  mutate(cluster = norm_cluster_data$cluster)
glimpse(norm_avg_monthly_tw_wide)

# PIVOT WIDER
norm_avg_monthly_tw_long <- norm_avg_monthly_tw_wide %>%
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

############################################################################################
#CLUSTERING FOR K = 2 WITH NORMALIZED DATA 

set.seed(113) # SO WE CAN REPRODUCE
cl <- kmeans(norm_cluster_data, centers = 2, nstart = 10)

#ADD ASSIGNMENTS INTO TO DATA FRAME
norm_cluster_data$cluster <- as.factor(cl$cluster)

glimpse(norm_cluster_data)

norm_avg_monthly_tw_wide <- norm_avg_monthly_tw_wide %>%
  mutate(cluster = norm_cluster_data$cluster)
glimpse(norm_avg_monthly_tw_wide)

# PIVOT WIDER
norm_avg_monthly_tw_long <- norm_avg_monthly_tw_wide %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "tw"
  ) %>%
  select(station_number, month, tw, cluster)

glimpse(norm_avg_monthly_tw_long)
saveRDS(norm_avg_monthly_tw_long, "norm_avg_monthly_tw_long_K_2.RDS")


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
#cluster_3_data <- separate_cluster_data(norm_avg_monthly_tw_long, i = 3)
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
#plot_cluster_3 <- plot_cluster_data(cluster_3_data, i = 3)
#plot_cluster_4 <- plot_cluster_data(cluster_4_data, i = 4)

combined_plot <- (plot_cluster_1 + plot_cluster_2)
combined_plot

#CALCULATE AVERAGE THERMAL REGIME OF EACH NORMALIZED CLUSTER WHEN K = 3
norm_avg_thermal_regime_K2 <- norm_avg_monthly_tw_long %>%
  group_by(cluster, month) %>%
  summarise(mean_tw = mean(tw, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec")))

norm_avg_thermal_plot_K2 <- ggplot(norm_avg_thermal_regime_K2, aes(x = month, y = mean_tw, group = cluster, color = cluster)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2) +  
  scale_color_manual(values = c("red", "yellow", "blue"), name = "Cluster") +
  labs(
    title = "Normalized Average Thermal Regime by Cluster, K = 2",
    x = "Month",
    y = "Value"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

norm_avg_thermal_plot_K2

meta<-tidyhydat::allstations %>% 
  select(c(STATION_NUMBER, STATION_NAME, LATITUDE, LONGITUDE,PROV_TERR_STATE_LOC )) 

merge_2_meta <- norm_avg_monthly_tw_long %>% 
  left_join(meta, by = c("station_number" = "STATION_NUMBER")) %>% 
  select(station_number, cluster, LATITUDE, LONGITUDE) %>% 
  distinct()

glimpse(merge_2_meta)

K2_meta_sf<- merge_2_meta %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview::mapview(K2_meta_sf, zcol = "cluster")


bc_map_K2 <- ggplot() +
  geom_sf(data = bc_bound, fill = "grey90", color = "black") +  
  geom_sf(data = K2_meta_sf, aes(fill = cluster), shape = 21, size = 3, stroke = 0.5, color = "black") +   
  scale_fill_manual(
    values = c("red", "yellow"),  
    name = "cluster"               
  ) +
  labs(
    title = "WSC Normalized Temperature cluster K = 2",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()
bc_map_K2

avg_thermal_cluster_plus_map <- (norm_avg_thermal_plot_K2 + bc_map_K2)




##################### ELBOW METHOD #######################################
# K VALUE RANGE
ks <- 1:8

# TOTAL WITHIN CLUSTER SUM OF SQUARES
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(norm_cluster_data, centers = k, nstart = 10)
  cl$tot.withinss
})

# PLOT TOTAL CLUSTER SUM OF SQUARES 
plot(ks, tot_within_ss, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal # of Groups NORMALIZED")


##########################################################
#ALEX'S CODE TO HELP WITH USIG FUNCTIONS 


library(patchwork)

my_plot = function(prov = "AB", title = "bob"){
  meta %>% 
    filter(PROV_TERR_STATE_LOC == prov) %>% 
    ggplot() + geom_point(aes(LONGITUDE, LATITUDE)) + theme_bw() + 
    theme(aspect.ratio = 1) +
    labs(title = title)
}


my_plot("BC", "A: British Columbia")|my_plot("AB", "B: Alberta")


