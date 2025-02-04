#Drew Gilchrist
#January 2025

#Clustering water temperature thermal regimes in BC 
# 
#
library(tidyverse)
library(dplyr)
library(tidyhydat)
library(readxl)
library(lubridate)
library(ggplot2)

# #Bring in water temp data, 
# #Code below is adapted from "TW-Analysis-Focus"

# setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\Clustering_WSC_Temp\\data")
# mega <- read.csv("mega_2024.csv")
# mega$date<-ymd(mega$date)
# glimpse(mega)
# 
# mega$file %>%
#   unique()
# 
# mega<- mega %>%
#   filter(tw <= 40) %>%
#   filter(tw>= -0.5) %>%
#   filter(file != "Envcanada_ec_tw_spot_measurements") %>%
#   filter(file != "QR_ProvisionalDailyValues_20151231_to_20221216")
# 
# #CREATE AVERAGE MONTHLY TEMP FOR ALL STATIONS
# #WANT STATIONS IN BC, WHICH HAVE 5 YEARS OF DATA AND HAVE ATLEAST 20 MSMTS PER MONTH
# 
# #NEED META DATA TO FILTER FOR PROVINCE OF BC
# meta<-tidyhydat::allstations %>%
#   select(c(STATION_NUMBER, STATION_NAME, LATITUDE, LONGITUDE,PROV_TERR_STATE_LOC ))
# 
# mega_meta <- mega %>%
#   left_join(meta, by = c("station_number" = "STATION_NUMBER"))
# 
# avg_monthly_tw <- mega_meta %>%
#   drop_na() %>% #REMOVE NA VALUES
#   filter(PROV_TERR_STATE_LOC == "BC") %>% #SELECT ONLY SITES IN BC
#   mutate(year = year(date),
#          month = month(date, label = TRUE)) %>% #CREATE YEAR AND MONTH COLUMNS
#   group_by(station_number, year, month) %>%
#   filter(n() > 20) %>%
#   group_by(station_number) %>%
#   mutate(nyear = n_distinct(year)) %>% #COUNT NUMBER OF YEARS OF DATA FOR EACH STATION
#   filter( nyear >= 10) %>% #FILTER FOR STATIONS WITH 10 OR MORE YEARS OF DATA
#   group_by(station_number, year, month) %>%
#   summarise(avg_temp = mean(tw), .groups = "drop") %>%  # CALCULATE AVERAGE MONTHLY TEMPERATURE PER YEAR
#   group_by(station_number, month) %>%
#   filter(n() >=5) %>% #MAKE SURE AVERAGES ARE BASED ON ATLEAST 5 YEARS OF AVERAGE MONTHLY DATA
#   summarise(avg_temp = mean(avg_temp), .groups = "drop") %>%  # CALCULATE AVERAGE MONTHLY TEMPERATURE
#   group_by(station_number) %>%
#   filter(n() == 12)


##################################################################################################
# REMOVE REGULATED SYSTEMS 
#################################################################################

# stations_reg <- hy_stn_regulation()
# 
# glimpse(avg_monthly_tw)
# 
# avg_monthly_tw<- avg_monthly_tw %>%
#   left_join(stations_reg, by = c("station_number" = "STATION_NUMBER")) %>%
#   filter(REGULATED == FALSE) %>%
#   ungroup() %>%
#   select(-c(Year_from, Year_to, REGULATED))

################################################################################################

setwd("C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\Clustering_WSC_Temp\\data")
#saveRDS(avg_monthly_tw, "avg_monthly_tw.RDS")
avg_monthly_tw = readRDS("avg_monthly_tw.RDS")

######################## PLOT THERMAL REGIMES ############################################
ggplot(avg_monthly_tw, aes(x = month, y = avg_temp)) +
  geom_line(group = 1, color = "blue") + 
  geom_point(color = "red") + 
  facet_wrap(~ station_number) + # FACET WRAP BY STATION NUMBER
  scale_x_discrete(breaks = levels(avg_monthly_tw$month)[seq(1, 12, by = 2)]) + # FIX LABELS
  labs(
    title = "Average Monthly River Temperature by Station",
    x = "Month",
    y = "Average Temperature (°C)"
  ) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8) 
  )


#NORMALIZE THE DATA 
norm<- avg_monthly_tw %>% 
  group_by(station_number) %>% 
  mutate(norm_avg_temp = ((avg_temp - min(avg_temp)) / (max(avg_temp) - min(avg_temp)))) %>% 
  ungroup()
         
# PLOT NORMALIZED THERMAL REGIMES 
ggplot(norm, aes(x = month, y = norm_avg_temp)) +
  geom_line(group = 1, color = "blue") + 
  geom_point(color = "red") + 
  facet_wrap(~ station_number) + # FACET WRAP BY STATION NUMBER
  scale_x_discrete(breaks = levels(avg_monthly_tw$month)[seq(1, 12, by = 2)]) + # FIX LABELS
  labs(
    title = "NORMALIZED Average Monthly River Temperature by Station",
    x = "Month",
    y = " NORMALIZED Average Temperature (°C)"
  ) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8) 
  )

#PIVOT WIDER

avg_monthly_tw_wide <- avg_monthly_tw %>%
  ungroup() %>% 
  pivot_wider(
    names_from = month, # COLUMN NAMES = MONTH
    values_from = avg_temp # VALUES = AVERAGE TEMPS
  )

glimpse(avg_monthly_tw_wide)

norm_avg_monthly_tw_wide<- norm %>%
  ungroup() %>% 
  select(-avg_temp) %>% 
  pivot_wider(
    names_from = month, # COLUMN NAMES = MONTH
    values_from = norm_avg_temp # VALUES = AVERAGE TEMPS
  )

glimpse(norm_avg_monthly_tw_wide)

####################TRY CLUSTER ANALYSIS USING KMEAN ####################################

# REMOVE STATION NUMBERS
cluster_data <- avg_monthly_tw_wide %>%
  ungroup() %>% 
  select(-station_number) 

norm_cluster_data <- norm_avg_monthly_tw_wide %>% 
  ungroup() %>% 
  select(-station_number)

#####################################################################################
##FIND HOMOGENOUS CLUSTERS USING K (centers) = 4
######################################################################################
set.seed(12) # SO WE CAN REPRODUCE
cl <- kmeans(cluster_data, centers = 4, nstart = 10)

# Add cluster assignments back to the dataset
cluster_data$cluster <- as.factor(cl$cluster)

glimpse(cluster_data)

avg_monthly_tw_wide <- avg_monthly_tw_wide %>% 
  mutate(cluster = cluster_data$cluster)
glimpse(avg_monthly_tw_wide)

# PIVOT WIDER
avg_monthly_tw_long <- avg_monthly_tw_wide %>%
  pivot_longer(
    cols = Jan:Dec,       
    names_to = "month",       
    values_to = "tw"    
  ) %>%
  select(station_number, month, tw, cluster) # Reorder/select columns

glimpse(avg_monthly_tw_long)

saveRDS(avg_monthly_tw_long, "avg_monthly_tw_long_K_4.RDS")


cluster_A_data <- avg_monthly_tw_long %>%
  filter(cluster == 1) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec")))

plot_A<-ggplot(cluster_A_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 1",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none" 
  )

cluster_B_data <- avg_monthly_tw_long %>%
  filter(cluster == 2) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_B<-ggplot(cluster_B_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 2",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  
  )
plot_B

cluster_C_data <- avg_monthly_tw_long %>%
  filter(cluster == 3) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_C<-ggplot(cluster_C_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 3",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  
  )
plot_C

cluster_D_data <- avg_monthly_tw_long %>%
  filter(cluster == 4) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_D<-ggplot(cluster_D_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 4",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # 
  )
plot_D

#COMBINE PLOTS
library(patchwork)

plot_A <- plot_A + ylim(-1, max(avg_monthly_tw_long$tw))
plot_B <- plot_B + ylim(-1, max(avg_monthly_tw_long$tw))
plot_C <- plot_C + ylim(-1, max(avg_monthly_tw_long$tw))
plot_D <- plot_D + ylim(-1, max(avg_monthly_tw_long$tw))

combined_plot_K_4 <- (plot_A + plot_B) / (plot_C + plot_D)
combined_plot_K_4

##########################################################################################
#PLOT FOR AVERAGE THERMAL REGIME OF EACH CLUSTER 
########################################################################


#CALCULATE AVERAGE THERMAL REGIME OF EACH CLUSTER WHEN K = 4
avg_thermal_regime_K4 <- avg_monthly_tw_long %>%
  group_by(cluster, month) %>%
  summarise(mean_tw = mean(tw, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec")))


avg_thermal_plot_K4 <- ggplot(avg_thermal_regime_K4, aes(x = month, y = mean_tw, group = cluster, color = cluster)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2) +  
  scale_color_manual(values = c("red", "yellow", "blue", "purple"), name = "Cluster") +
  labs(
    title = "Average Thermal Regime by Cluster, K = 4",
    x = "Month",
    y = "Average Temperature"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  )

# Display the plot
avg_thermal_plot_K4

avg_thermal_cluster_plus_map <- (avg_thermal_plot_K4 + bc_map_K4)


################################################################################
###################FIND HOMOGENOUS CLUSTERS USING K (centers) = 5
##################################################################################
set.seed(123) # SO WE CAN REPRODUCE

cluster_data <- avg_monthly_tw_wide %>%
  ungroup() %>% 
  select(-station_number) 

cl <- kmeans(cluster_data, centers = 5, nstart = 10)

# Add cluster assignments back to the dataset
cluster_data$cluster <- as.factor(cl$cluster)

glimpse(cluster_data)

avg_monthly_tw_wide <- avg_monthly_tw_wide %>% 
  mutate(cluster = cluster_data$cluster)
glimpse(avg_monthly_tw_wide)

# PIVOT WIDER
avg_monthly_tw_long <- avg_monthly_tw_wide %>%
  pivot_longer(
    cols = Jan:Dec,       
    names_to = "month",       
    values_to = "tw"    
  ) %>%
  select(station_number, month, tw, cluster) # Reorder/select columns

glimpse(avg_monthly_tw_long)

saveRDS(avg_monthly_tw_long, "avg_monthly_tw_long_K_5.RDS")


cluster_1_data <- avg_monthly_tw_long %>%
  filter(cluster == 1) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_1<-ggplot(cluster_1_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 1",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none" 
  )

cluster_2_data <- avg_monthly_tw_long %>%
  filter(cluster == 2) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_2<-ggplot(cluster_2_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 2",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  
  )
plot_2

cluster_3_data <- avg_monthly_tw_long %>%
  filter(cluster == 3) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_3<-ggplot(cluster_3_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 3",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  
  )
plot_3

cluster_4_data <- avg_monthly_tw_long %>%
  filter(cluster == 4) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_4<-ggplot(cluster_4_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 4",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # 
  )
plot_4

cluster_5_data <- avg_monthly_tw_long %>%
  filter(cluster == 5) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Oct", 
                                          "Nov", "Dec"))) # Ensure months are in order

plot_5<-ggplot(cluster_5_data, aes(x = month, y = tw, group = station_number, color = station_number)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Temperature Patterns for Cluster 4",
    x = "Month",
    y = "Temperature",
    color = "Station"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # 
  )
plot_5




#COMBINE PLOTS

plot_1 <- plot_1 + ylim(-1, max(avg_monthly_tw_long$tw))
plot_2 <- plot_2 + ylim(-1, max(avg_monthly_tw_long$tw))
plot_3 <- plot_3 + ylim(-1, max(avg_monthly_tw_long$tw))
plot_4 <- plot_4 + ylim(-1, max(avg_monthly_tw_long$tw))
plot_5 <- plot_5 + ylim(-1, max(avg_monthly_tw_long$tw))

# Arrange the first column (plots 1 to 5)
column_1 <- plot_1 / plot_2 / plot_3 / plot_4 / plot_5

# Arrange the second column (plots A to D)
column_2 <- plot_A / plot_B / plot_C / plot_D

# Combine both columns into a single plot
combined_plot_5_4 <- column_1 | column_2

# Display the combined plot
combined_plot_5_4

# Optionally, save the combined plot
ggsave("combined_plot_5_4.png", combined_plot_5_4, width = 10, height = 12)


# Display the combined plot
combined_plot_5_4



##################### ELBOW METHOD #######################################
# K VALUE RANGE
ks <- 1:8

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







