#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####


#### Script 5: Weather variables extraction and analysis ####

# This script allows to calculate weather variables. It is included to allow to get these variables in case hydrological variables estimation
# is not possible due to the lack of streamflow data. Using the weather datasets mentioned in the methodology, weather variables can be calculated 
# for the Spanish territory


# The files created using the Script 1 are neccesary to run this script


# Used libraries
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
library(gt)
library(patchwork)


study_period <- 1980:2018 # User action: Define the studied period. Weather data for this period will be extracted

#### 1. Precipitation calculation ####

path <- "1_Used_files/Data/weather_data/pcp_spain/" # Directory where the precipitation file for each point of the grid is located

# User action: Define the starting and ending dates for the weather data
init_date <- as.Date("1951-01-01")
end_date <- as.Date("2019-12-31")
dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire period with data is created

pcp_grid_points <-  read.csv("1_Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  

# Loop for calculating the annual precipitation of each basin trough the average of the annual precipitation for each station within the basin
pcp_bas_list <- list() #empty list
for(i in 1:length(unique(pcp_grid_points$Basin_ID))){  # i --> Basin ID
  filt_st <- filter(pcp_grid_points, Basin_ID == i) # Basin data and precipitations points inside
  stations <- filt_st[,1] #Precipitations points inside each basin
  pcps_sts <- c()
  for(n in 1:length(stations)){   # n --> Weather stations identifier within each basin
    st_dat <- read_table(paste(path, stations[n], "_PCP.txt", sep = ""), skip = 1, col_names = F) %>% #read the precipitation file for each point
      mutate(date = ymd(dates), pcp = X1) %>% select(date, pcp) 
    
    dates_seq <- filter(st_dat, year(date) %in% study_period) %>% select(date) 
    pcp_st <- filter(st_dat, year(date) %in% study_period) %>% select(pcp) # Filtering with the study period
    pcps_sts <- tibble(pcps_sts, pcp_st, .name_repair = "unique")    # Table with annual precipitation data for all the points of a basin 
    
  }
  pcp_bas <- pcps_sts %>% apply(., 1, mean) %>% tibble(dates_seq, pcp = .)  # Calculate for each basin the average daily precipitation of all the precipitation points within
  pcp_bas_list[[i]] <- pcp_bas%>% mutate(bas = i)  
}


# Convert the list into a table

basins_pcp <- tibble(date = c(), pcp = c())
  
for (i in 1:length(pcp_bas_list)) {
  
  basin_dat <- pcp_bas_list[[i]] 

  basins_pcp <- basins_pcp %>% rbind(basin_dat) 
  
}

basins_pcp # Table with daily pcp for every basin


#### 2. Temperature calculation ####

# User action: Define the starting and ending dates for the weather data
init_date <- as.Date("1951-01-01")
end_date <- as.Date("2019-12-31")
dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire serie is created

path <- "1_Used_files/Data/weather_data/tmp_spain/" # Directory where the temperature file for each point of the grid is located
tmp_grid_points <-  read.csv("1_Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  

# Loop for calculating the temperature of each basin trough the average of the annual temperature (Max, min, mean) for each station within the basin
tmp_bas_list <- list() #empty list 
for(i in 1:length(unique(tmp_grid_points$Basin_ID))){  # i --> Basin ID
  filt_st_t <- filter(tmp_grid_points, Basin_ID == i)
  stations_t <- filt_st_t[,1]
  tmps <- c()
  tmp_mins <- c()
  tmp_maxs <- c()
  tmp_means <- c()
  for(n in 1:length(stations_t)){   # n --> Weather stations identifier within each basin
   
     st_dat_t <- read.csv(paste(path, stations_t[n], "_TMP.txt", sep = ""), skip = 1,header = F) %>% 
     mutate(date = ymd(dates), tmp_Max = V1, tmp_min = V2) %>% 
     select(date, tmp_Max, tmp_min) %>% 
     filter(year(date) %in% study_period) %>% 
       group_by(date) %>% mutate(tmp_mean = mean(c(tmp_Max, tmp_min))) %>% ungroup(.)
    
     dates_seq <- st_dat_t$date
     
    tmp_min <- select(st_dat_t, tmp_min)
    tmp_mins <- tibble(tmp_mins, tmp_min, .name_repair = "unique")    # Minimum temperature for each station 
    tmp_max <- select(st_dat_t, tmp_Max)
    tmp_maxs <- tibble(tmp_maxs, tmp_max, .name_repair = "unique")    # Maximum temperature for each station 
    tmp_mean <- select(st_dat_t, tmp_mean)
    tmp_means <- tibble(tmp_means, tmp_mean, .name_repair = "unique") # Mean temperature for each station 
    
  }
  
  tmp_bas_list[[i]] <- tibble(Date = dates_seq, 
                              Tmp_min = apply(tmp_mins, 1, mean),   # Minimun temperature for each basin
                              Tmp_Max = apply(tmp_maxs, 1, mean),   # Maximun temperature for each basin
                              Tmp_mean = apply(tmp_means, 1, mean)) # Mean temperature for each basin
  
}

# Convert the list into a table

basins_tmp <- tibble(Date = c(), Tmp_min = c(), Tmp_Max = c(), Tmp_mean = c())

for (i in 1:length(tmp_bas_list)) {
  
  basin_dat <- tmp_bas_list[[i]] %>% mutate(bas = i)
  
  basins_tmp <- basins_tmp %>% rbind(basin_dat) 
  
}


#### 3.Working and saving the obtained data ####

# Merging TMP and PCP tables
daily_weather_tables <- basins_tmp %>% rename(date = Date) %>%  
  left_join(., basins_pcp, c("bas", "date")) %>% 
  select(bas, date, pcp, Tmp_min, Tmp_Max, Tmp_mean)


# This file can be saved
write.csv(daily_weather_tables, 
          "3_Output_data/Weather_outputs/daily_data.csv", row.names = F)

# An loaded
daily_weather_tables <- read_csv("3_Output_data/Weather_outputs/daily_data.csv")


# Aggregating to annual scale
annual_weather_tables <- daily_weather_tables %>% group_by(year(date), bas) %>% 
  summarise(pcp_ac = sum(pcp),
            Tmean_mean = mean(Tmp_mean),
            Tmax_mean = mean(Tmp_Max), 
            Tmin_mean = mean(Tmp_min)) %>% ungroup(.) %>% 
  rename(Year = `year(date)`)
            

# Aggregating to monthly scale (12 values per year)
annual_monthly_weather_tables <- daily_weather_tables %>% 
  group_by(year(date), month(date), bas) %>% 
  summarise(pcp_ac = sum(pcp),
            Tmean_mean = mean(Tmp_mean),
            Tmax_mean = mean(Tmp_Max), 
            Tmin_mean = mean(Tmp_min)) %>% ungroup(.) %>% 
  mutate(Date = ym(paste(`year(date)`, `month(date)`, sep = "/")), .before = 1) %>% 
  select(., -c( `year(date)`, `month(date)`))


# Aggregating to monthly scale (Avergae monthly values, 12 values per studied period)
average_monthly_weather_tables <- daily_weather_tables %>% 
  group_by(year(date), month(date), bas) %>% 
  summarise(pcp_ac = sum(pcp),
            Tmean_mean = mean(Tmp_mean),
            Tmax_mean = mean(Tmp_Max), 
            Tmin_mean = mean(Tmp_min)) %>% 
  group_by(bas, `month(date)`) %>% 
  summarise_all(., mean) %>%   ungroup(.) %>% 
  rename(Month = `month(date)`) %>% 
  select(., -c(  `year(date)`))


#### 4. Weather data analysis ####


##### 4.1. Average annual values for all the basins #####

av_annual_data <- annual_weather_tables %>% group_by(bas) %>% 
  summarise_all(., mean) %>% select(., -Year)

av_annual_data %>% mutate_at(c("Tmean_mean", "Tmax_mean", "Tmin_mean"), round, digits = 2) %>% 
  mutate(pcp_ac = round(pcp_ac, 0)) %>% 
  rename("Basin ID" = "bas",
   "Annual precipitation (mm)" = "pcp_ac", 
   "Average mean temperature (°C)" = "Tmean_mean", 
   "Average maximum temperature (°C)" = "Tmax_mean", 
   "Average minimum temperature (°C)" = "Tmin_mean") %>% 
  gt(.)


# Plotting one or several variables
# Ej. TMPs
tmp_plot_basins <-av_annual_data %>% 
  pivot_longer(., -bas, names_to = "Variable") %>% 
  filter(Variable != "pcp_ac") %>% 
  ggplot(., aes(x = bas, y = value)) +
  geom_point(aes(shape = Variable, fill = factor(bas, levels = as.character(1:19))), size = 3)+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  scale_shape_manual(values = c(24, 22, 25))+
  theme_bw()+
  labs(y= "Temperature (°C)", fill = "Basin", text = element_text(size = 12), x = "Basin")

tmp_plot_basins

# Ej. PCP
 av_annual_data %>% 
  pivot_longer(., -bas, names_to = "Variable") %>% 
  filter(Variable == "pcp_ac") %>% 
  ggplot(., aes(x = bas, y = value)) +
  geom_point(aes(color = bas), size = 5)+
  theme_bw()+
  labs(y= "Precipitation (mm/year)", color = "Basin", text = element_text(size = 12), x = "Basin")


# User may want to save the created plots: To do so, a plot should be assigned to a variable  (variable -> plot), and then saved with the following code

ggsave(plot = tmp_plot_basins, device = "png", 
       filename = "3_Output_data/Weather_outputs/plot_tmp_basins.png",
       width = 10, height = 8, dpi = 600)
        


##### 4.2. Comparisson of the study period among basins at annual scale ####

annual_weather_tables %>% 
  pivot_longer(., -c(Year, bas), names_to = "Variable") %>% 
  filter(Variable != "pcp_ac") %>% 
  ggplot(., aes(x = bas))+
  geom_boxplot(aes(group = bas, y = value, fill = factor(bas, levels = as.character(1:19))))+
  geom_point(aes(y = value), shape = 1)+
  facet_wrap(facets = "Variable") +
  theme_bw()+
  labs(y= "Temperature (°C)", fill = "Basin", text = element_text(size = 12), x = "Basin")


annual_weather_tables %>% 
  pivot_longer(., -c(Year, bas), names_to = "Variable") %>% 
  filter(Variable == "pcp_ac") %>% 
  ggplot(., aes(x = bas))+
  geom_boxplot(aes(group = bas, y = value, fill = factor(bas, levels = as.character(1:19))))+
  geom_point(aes(y = value), shape = 1)+
  theme_bw()+
  labs(y= "Precipitation (mm/year)", fill = "Basin", text = element_text(size = 12), x = "Basin")


##### 4.3. Analysing weather for each subbasin #####

# An analysis of weather variables for each subbasin can be done at different scales. 
# Some examples are included, but users might want to perform different analysis

studied_basin <- 1 # User action: Define the studied basin with its Basin ID

# 4.3.1. Annual scale

annual_data <- annual_weather_tables %>% filter(bas == studied_basin)

# Quick plot
annual_data %>% 
  pivot_longer(., -c(Year, bas), names_to = "Variable") %>% 
  ggplot(., aes(x = Year, y = value, color = Variable))+
  geom_line()+
  facet_wrap(facets = "Variable", scales = "free")


# Average values

av_annual_data <- annual_data %>% summarise_all(., mean) %>% mutate(Date = "Entire period")


# Values of one year

year_interest <- 2005
annual_data %>% filter(Year == year_interest)



# Values in the driest and wettest years

annual_data[annual_data$pcp_ac == max(annual_data$pcp_ac),]
annual_data[annual_data$pcp_ac == min(annual_data$pcp_ac),]



# Visualizing variables for the study period

# Annual and average precipitation
annual_data %>% select(Year, pcp_ac) %>% 
  ggplot(., aes(x = Year))+
  geom_bar(aes(y = pcp_ac), stat = "identity", fill = "deepskyblue", color = "darkblue")+
  geom_line(aes(y = mean(pcp_ac)), linetype = 2, linewidth = 0.7)+
  theme_bw()+
  labs(y= "Precipitation (mm/year)", text = element_text(size = 12))



# Temperatures variation
  annual_data %>% select(-pcp_ac) %>% 
  ggplot(., aes(x = Year))+
  geom_ribbon(aes(ymin =  Tmin_mean, ymax = Tmax_mean), fill = "darkolivegreen3", alpha = 0.8)+
  geom_line(aes(y = Tmean_mean),linewidth = 1.2, color = "green4" )+
  theme_bw()+
  labs(y= "Temperature (°C)", text = element_text(size = 12), x = "Basin")+
  ggtitle("Average annual mean, maximum and minimum temperature")


# 4.3.2. Monthly scale (average)

monthly_data <- average_monthly_weather_tables %>% filter(bas == studied_basin)


  monthly_data %>% 
  select(Month, pcp_ac) %>% 
  mutate(Month = factor(month.abb[Month], levels = month.abb)) %>% 
  ggplot(., aes(x = Month, y = pcp_ac))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "darkblue")+
  geom_line(aes(group = 1), color = "blue", size = 1) +
  theme_bw()+
  labs(y= "Precipitation (mm/month)", text = element_text(size = 12))

monthly_data %>% 
  select(-c(bas, pcp_ac)) %>% 
  mutate(Month = factor(month.abb[Month], levels = month.abb)) %>% 
  pivot_longer(., -c(Month), names_to = "Variable") %>% 
  ggplot(., aes(x = Month, y = value))+
  geom_line(aes(group = Variable, color = Variable), size = 2) +
  geom_point(shape = 17)+
  scale_color_manual(values = c("orangered3", "coral", "khaki3"))+
  theme_bw()+
  labs(y= "Temperature (°C)", text = element_text(size = 12))
  
  

# 4.3.3. Monthly scale (time series)


monthly_serie <- annual_monthly_weather_tables %>% filter(bas == studied_basin)


# Precipitation serie

monthly_serie %>% 
  
  # Filter using some period (Remove next #)
  #  filter(year(Date) %in% 1990:2000) %>% 
 
  # Filter using a year
  #  filter(year(Date) == 1995) %>%  
 
   ggplot(aes(x = Date, y = pcp_ac))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  labs(y= "Precipitation (mm/month)", text = element_text(size = 12))

# Temperature serie

monthly_serie %>% select(-pcp_ac) %>% 
  
  # Filter using some period
  #  filter(year(Date) %in% 1990:2000) %>% 
  
  # Filter using a year
  #  filter(year(Date) == 1995) %>%  
  
  ggplot(., aes(x = Date))+
  geom_ribbon(aes(ymin =  Tmin_mean, ymax = Tmax_mean), fill = "darkolivegreen3", alpha = 0.8)+
  geom_line(aes(y = Tmean_mean),linewidth = 1.2, color = "green4" )+
  theme_bw()+
  labs(y= "Temperature (°C)", text = element_text(size = 12), x = "Basin")+
  ggtitle("Average annual mean, maximum and minimum temperature")


# Comparing two or more periods

period_1 <- 1980:1999
period_2 <- 2000:2019

monthly_serie_periods <- monthly_serie %>% 
  mutate(period = case_when(year(Date) %in% period_1 ~ paste(period_1[1], period_1[length(period_1)], sep = "-"),
                            year(Date) %in% period_2 ~ paste(period_2[1], period_2[length(period_2)], sep = "-")))


# Average values comparison
tmp_comp <- monthly_serie_periods %>% 
  group_by(period, month(Date)) %>% 
  summarise_at(c("Tmean_mean", "Tmax_mean", "Tmin_mean"), mean) %>% 
  rename(Month = `month(Date)`)

pcp_comp <- monthly_serie_periods %>% 
  group_by(period, month(Date)) %>% 
  summarise(pcp_ac = mean(pcp_ac)) %>% 
  rename(Month = `month(Date)`)

# Ej. PCP Comparison
 pcp_comp %>% mutate(Month = month.abb[Month]) %>% 
  pivot_wider(names_from = c(Month), values_from = pcp_ac)
 
# Ej. TMP Mean Comparison
tmean_comp <-  tmp_comp %>%  mutate(Month = month.abb[Month]) %>% 
 select(., -c(Tmax_mean, Tmin_mean)) %>% 
   pivot_wider(names_from = c(Month), values_from = c(Tmean_mean))
 

# TMP Comparison Time series
  monthly_serie_periods %>% 
  select(-pcp_ac) %>%
  ggplot(., aes(x = Date))+
  geom_ribbon(aes(ymin =  Tmin_mean, ymax = Tmax_mean), fill = "darkolivegreen3", alpha = 0.8)+
  geom_line(aes(y = Tmean_mean),linewidth = 1.2, color = "green4" )+
  theme_bw()+
  facet_wrap(facets = "period", scales = "free_x")+
  labs(y= "Temperature (°C)", text = element_text(size = 12), x = "Basin")+
  ggtitle("Average annual mean, maximum and minimum temperature")

# PCP Comparison Time series
monthly_serie_periods %>% 
  select(Date, period, pcp_ac) %>%
  ggplot(., aes(x = Date, y = pcp_ac))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  facet_wrap(facets = "period", scales = "free_x")+
  labs(y= "Precipitation (mm/month)", text = element_text(size = 12))



