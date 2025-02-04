library(odbc)
library(DBI)
library(tidyverse)
library(tidyhydat)
library(ggpubr)
library(dplyr)
install.packages("stringr")
library(stringr)


# GET STATION NAME
my_stn <- tidyhydat::allstations %>% filter(HYD_STATUS == "ACTIVE", str_detect(STATION_NAME,"INGENIKA RIVER"))

# ADD WSD MICROSOFT DATABASE
conn <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; 
                  Dbq=C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\code\\data\\WaterTemperature_1994to2016.mdb;")


dailyt <- dbReadTable(conn, "Envcanada_dailyt_data_envcanada") %>% as_tibble() %>% filter(StationName == my_stn$STATION_NAME) %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = as_datetime(paste0(an_yr,"-",mo,"-",jj_dd," 00:00:00")), 
         n_value = Tmean,
         n_name = "dailyt") %>% select(starts_with("n_"))

spot <- dbReadTable(conn, "Envcanada_ec_tw_spot_measurements") %>% as_tibble()%>% filter(StationID == my_stn$STATION_NUMBER) %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = as_datetime(DateObs), 
         n_value = Tw,
         n_name = "spot") %>% select(starts_with("n_"))

files <- list.files(pattern = my_stn$STATION_NUMBER, recursive = T)

ts1 <- read_csv(files[1], skip = 6) %>% mutate(file = "ts1") %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = Time, 
         n_value = Value,
         n_name = "ts1") %>% select(starts_with("n_"))
ts2 <- read_csv(files[2], skip = 6) %>% mutate(file = "ts2") %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = Time, 
         n_value = Value,
         n_name = "ts2") %>% select(starts_with("n_"))
ts3 <- read_csv(files[3], skip = 6) %>% mutate(file = "ts3") %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = Time, 
         n_value = Value,
         n_name = "ts3") %>% select(starts_with("n_"))
ts4 <- read_csv(files[4], skip = 6) %>% mutate(file = "ts4") %>% 
  mutate(n_site = my_stn$STATION_NAME, 
         n_date = Time, 
         n_value = Value,
         n_name = "ts4") %>% select(starts_with("n_"))

ts <- bind_rows(#ts1,
  ts2,
  ts3,
  ts4, 
  dailyt, 
  spot)

ts_annual <- ts %>% filter(n_name != "spot") %>% 
  group_by(year = year(n_date)) %>% 
  filter(n_value == max(n_value, na.rm = T)) %>% 
  filter(row_number() == 1)

ggplot() +
  geom_line(data = ts %>% filter(n_name != "spot"), aes(n_date, n_value, color = n_name)) +
  geom_point(data = ts_annual, aes(n_date, n_value, color = "annualmax")) +
  geom_smooth(data = ts_annual, aes(n_date, n_value, color = "annualmax"), se = F, method = "lm") +
  geom_smooth(data = ts %>% filter(n_name != "spot"), aes(n_date, n_value), se = F, method = "lm") +
  geom_point(data = ts %>% filter(n_name == "spot"), aes(n_date, n_value, color = n_name)) + 
  scale_color_manual(values = c("red","grey","black","orange","blue","steelblue")) +
  theme_bw()  +
  theme(aspect.ratio = 0.5) +
  scale_x_datetime(date_breaks = "5 years", date_labels = "%Y", date_minor_breaks = "1 year") + 
  labs(x = "", y = "Tw", color = "File", title = my_stn$STATION_NAME) + 
  stat_regline_equation(
    data = ts %>% filter(n_name != "spot"), 
    aes(n_date, n_value, label = ..eq.label..),
    label.y.npc = 0.95) + 
  stat_regline_equation(
    data = ts_annual, 
    aes(n_date, n_value, label = ..eq.label..),
    label.y.npc = 0.95, label.x.npc = 0.5, color = "red")
