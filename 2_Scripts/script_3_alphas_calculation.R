#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 3: Estimation of alpha values  ####

# Methodology: This script has been used to estimate the Groundwater recession constant, and in turn the alpha values.
# Therefore, it can be considered the first part of the groundwater index estimation, as the obtained alpha values 
# have been used as reference when using the digital filter.

#For each basin, three representative peaks between 2010-2018 have been selected, and a linear regression has been adjusted to 
# the baseflow recession curve in order to determine the groundwater recession constant (α). The initial point for the recession 
#curve has been selected when the direct runoff peak ends. The estimated slope of the regression is the α value. A minimum of 10 
# days of recession curve was selected as criteria, but a longer recession curve has been preferred despite having some irregularities than a shorter one.
# A minimum determination coefficient of 0.80 was also used as criteria. 

# The linear adjustment regression has been performed with the stats::lm function. With this function, two coefficients are obtained in this case:
# Intercept is the log() of the streamflow when time (t) = 0. 
# The other coefficient is the Slope of the lineal regression, which is the groundwater recession coefficient * t, being t the number of days after t0.


# As input data, the files with streamflow data and wih the subbasins data have been used. The daily precipitation has been used to select the peaks and the
# start and end of the recession curve. Therefore, it is necessary to run the "RUN THIS FIRST" section.
# An output csv file with the obtained alpha values and the determination coefficient has been generated (3_alpha_estimation)     

# The user should use the SCRIPT FOR OBTAINING ALPHA VALUES section to obtain the information for each basin.
# All the steps where the user have to make an action are indicated as # User action, and an explanation is included

# At the end of the script, the analysis performed for each of the evaluated subbasins of the presented work are included in section

# Used libraries
library(readr)
library(plotly)
library(lubridate)
library(tidyverse)
library(gt)
library(patchwork)


#### RUN THIS FIRST ####

# Streamflow data
gauging_data_tagus <- read.csv("1_Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
  tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
  .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))

# File with IDs, names, regions and areas of the basin, and gauging stations codes  
basins_file <- read.csv("1_Used_files/Created_csv/1_basins_file.csv") 

# Daily precipitation obtention
path <- "1_Used_files/Data/Climate_data_extracted/pcp_spain/" # Directory where the precipitation file for each point of the grid is located
init_date <- as.Date("1951-01-01")
end_date <- as.Date("2019-12-31")
dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire period with data is created
period_dates <- tibble(dates) %>% filter(., year(dates) %in% 2010:2018)
pcp_grid_points <-  read.csv("1_Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  

# Loop for calculating the daily precipitation of each basin 
pcpday_bas_list <- list()
for(i in 1:length(unique(pcp_grid_points$Basin_ID))){  # i --> Basin ID
  filt_st <- filter(pcp_grid_points, Basin_ID == i) # Basin data and precipitations points inside
  stations <- filt_st[,1] #Precipitations points inside each basin
  pcps_sts <- c()
  for(n in 1:length(stations)){   # n --> Weather stations identifier within each basin
    st_dat <- read_table(paste(path, stations[n], "_PCP.txt", sep = ""), skip = 1, col_names = F) %>% #read the precipitation file for each point
      mutate(date = ymd(dates), pcp = X1) %>% .[,c("date", "pcp")] 
    colnames(st_dat) <- c("date", "pcp")
    pcp_st <- filter(st_dat, year(date) %in% 2010:2018) %>% .[,"pcp"] # Filtering with the study period
    pcps_sts <- tibble(pcps_sts, pcp_st, .name_repair = "unique")    # Table with annual precipitation data for all the points of a basin 
  }
  pcp_bas <- pcps_sts %>% apply(., 1, mean) %>% cbind(period_dates) %>% .[,c(2,1)]
  colnames(pcp_bas) <- c("date", "precipitation")
  pcpday_bas_list[[i]] <- pcp_bas  # List with the daily precipitation in each basin
}



#### SCRIPT FOR OBTAINING ALPHA VALUES ####

# Period that is being analysed (years)
evaluated_period <- c(2010:2018) # User action: Define the study period

##### 0 --> Extracting data for each subbasin: Define the Basin_ID #####  
basin_ID <- 1 # User action: Define the basin to analyse

basin_information <- paste("Basin ", basins_file$Basin_ID[basin_ID], ", ",
                           basins_file$Basin[basin_ID], " (gauging code = ",
                           basins_file$gauging_code[basin_ID], ")", sep = "")

streamflow_data <- gauging_data_tagus %>% 
  filter(., cod == basins_file$gauging_code[basin_ID],
         year(date) %in% evaluated_period) 

precipitation_data <- tibble(pcpday_bas_list[[basin_ID]]) 

basin_data <- streamflow_data %>% left_join(precipitation_data, "date") %>%
  mutate(day = seq(1, length(.$date), 1)) %>% .[,c(1,2,4,3,5)]


#####  1 --> Peaks selection: use the interactive plot of the streamflow to select 3 different peaks #####  

ggplotly(ggplot(basin_data, aes(x = day))+
           geom_line(aes(y = obs_flow))+
           geom_col(aes(y = precipitation), fill = "deepskyblue")) # Check streamflow data and peak selection

# Range of x values for the peaks 
peak_1 <- c(660:835)    # User action: define the beggining and end of the peak (x axis)
peak_2 <- c(1100:1330)  # User action: define the beggining and end of the peak (x axis)
peak_3 <- c(2975:3120)  # User action: define the beggining and end of the peak (x axis)


#####  2 --> For each peak, define the starting and ending point for the recession #####  

peak_1_data <- basin_data[peak_1,]
ggplotly(ggplot(peak_1_data, aes(x = seq(1, length(date), 1)))+
           geom_line(aes(y = obs_flow))+
           geom_col(aes(y = precipitation), , fill = "deepskyblue")) 

peak_2_data <- basin_data[peak_2,]
ggplotly(ggplot(peak_1_data, aes(x = seq(1, length(date), 1)))+
           geom_line(aes(y = obs_flow))+
           geom_col(aes(y = precipitation), , fill = "deepskyblue")) 

peak_3_data <- basin_data[peak_3,]
ggplotly(ggplot(peak_1_data, aes(x = seq(1, length(date), 1)))+
           geom_line(aes(y = obs_flow))+
           geom_col(aes(y = precipitation), , fill = "deepskyblue")) 


# Range of x values for the recession
peak_1_recesion <- c(45:106)  # # User action: define the beggining and end of the recession curve (x axis)
peak_2_recesion <- c(111:200) # # User action: define the beggining and end of the recession curve (x axis)
peak_3_recesion <- c(63:132)  # # User action: define the beggining and end of the recession curve (x axis)


# The streamflow against date should be approximately a straigth line, specially at logarithm scale 

# Normal scale
plot(peak_1_data$obs_flow[peak_1_recesion]~peak_1_data$date[peak_1_recesion], type = "l")
plot(peak_2_data$obs_flow[peak_2_recesion]~peak_1_data$date[peak_2_recesion], type = "l")
plot(peak_3_data$obs_flow[peak_3_recesion]~peak_1_data$date[peak_3_recesion], type = "l")

# Logarithm scale
plot(log(peak_1_data$obs_flow[peak_1_recesion])~peak_1_data$date[peak_1_recesion], type = "l")
plot(log(peak_2_data$obs_flow[peak_2_recesion])~peak_1_data$date[peak_2_recesion], type = "l")
plot(log(peak_3_data$obs_flow[peak_3_recesion])~peak_1_data$date[peak_3_recesion], type = "l")


#####  3 --> Linear regression to obtain recession coeffient for each peak #####  

# Peak 1


reg_pk1 <- lm(log(peak_1_data$obs_flow[peak_1_recesion])~ seq(1, length(peak_1_data$date[peak_1_recesion]), 1)) # Linear regression
sum_reg_pk1 <- summary(reg_pk1) # Results of the regression

rec_const_pk1 <- sum_reg_pk1$coefficients[2,1] # Slope of the curve
adj_r2_pk1 <- sum_reg_pk1$adj.r.squared # Coefficient of determination
alpha_value <- 2.71828182846^(rec_const_pk1)  # Alpha value calculation

peak_1_recession_data <- tibble(Basin_ID = basin_ID,
                                peak = 1,
                                recess_days = length(peak_1_recesion), # Recession curve duration
                                det_coef = adj_r2_pk1, 
                                gw_rec_const = rec_const_pk1,
                                alpha_value = alpha_value,
                                peak_range = paste(peak_1[1], peak_1[length(peak_1)], sep= ":"), 
                                recess_range = paste(peak_1_recesion[1], peak_1_recesion[length(peak_1_recesion)], sep= ":"),
                                basin_info = basin_information)

# Peak 2

reg_pk2 <- lm(log(peak_2_data$obs_flow[peak_2_recesion])~ seq(1, length(peak_2_data$date[peak_2_recesion]), 1))
sum_reg_pk2 <- summary(reg_pk2) 

rec_const_pk2 <- sum_reg_pk2$coefficients[2,1]
adj_r2_pk2 <- sum_reg_pk2$adj.r.squared
alpha_value <- 2.71828182846^(rec_const_pk2) 

peak_2_recession_data <- tibble(Basin_ID = basin_ID,
                                peak = 2,
                                recess_days = length(peak_2_recesion),
                                det_coef = adj_r2_pk2, 
                                gw_rec_const = rec_const_pk2,
                                alpha_value = alpha_value,
                                peak_range = paste(peak_2[1], peak_2[length(peak_2)], sep= ":"), 
                                recess_range = paste(peak_2_recesion[1], peak_2_recesion[length(peak_2_recesion)], sep= ":"),
                                basin_info = basin_information)

# Peak 3

reg_pk3 <- lm(log(peak_3_data$obs_flow[peak_3_recesion])~ seq(1, length(peak_3_data$date[peak_3_recesion]), 1))
sum_reg_pk3 <- summary(reg_pk3) 

rec_const_pk3 <- sum_reg_pk3$coefficients[2,1]
adj_r2_pk3 <- sum_reg_pk3$adj.r.squared
alpha_value <- 2.71828182846^(rec_const_pk3) 

peak_3_recession_data <- tibble(Basin_ID = basin_ID,
                                peak = 3,
                                recess_days = length(peak_3_recesion),
                                det_coef = adj_r2_pk3, 
                                gw_rec_const = rec_const_pk3,
                                alpha_value = alpha_value,
                                peak_range = paste(peak_3[1], peak_3[length(peak_3)], sep= ":"), 
                                recess_range = paste(peak_3_recesion[1], peak_3_recesion[length(peak_3_recesion)], sep= ":"),
                                basin_info = basin_information)


#####  4 --> Create a table with the data for the evaluated basin ##### 

basin_alpha_estimation_table <- peak_1_recession_data %>% rbind(., peak_2_recession_data, peak_3_recession_data)


##### 5 --> Create a table with the data for all the basins ##### 

# alpha_estimation_table <- c() # User action: Remove the # and run, then include again the #. This line should be run only one time, after the first basin evaluation

alpha_estimation_table <- alpha_estimation_table %>%  rbind(basin_alpha_estimation_table)


##### 6 --> Save the created table ##### 

#  alpha_estimation_table %>%  #  User action: Remove the # and run, then include again the #. These lines should be run only one time, after the last basin evaluation
#  left_join(., basins_file[,c("Basin", "Basin_ID", "region")], "Basin_ID") %>% 
#  select(-basin_info) %>% 
#  write.csv(.,      
#          "1_Used_files/Created_csv/3_alpha_estimation_n.csv", quote = F, row.names = F)


#### END OF SCRIPT FOR OBTAINING ALPHA VALUES ####



#### REPORT TABLES ####
 
# Reading the saved data
alphas_output_file <- read.csv("1_Used_files/Created_csv/3_alpha_estimation.csv")


#####  Report table 3: Results at basin scale   ##### 

# Average values at basin scale


basin_alpha_values <- alphas_output_file %>% 
  group_by(Basin, Basin_ID) %>% 
  summarise_at(c("recess_days", 
                 "det_coef",
                 "gw_rec_const",
                 "alpha_value"), mean) %>% ungroup(.) %>%
  mutate_at(., c("det_coef", "gw_rec_const", "alpha_value"), ~ round(.x, 3)) %>% 
  mutate(recess_days = as.integer(recess_days)) %>% 
  left_join(., basins_file[,c("Basin_ID", "region")], "Basin_ID") %>% 
  select(Basin_ID, Basin, region, recess_days, det_coef, gw_rec_const, alpha_value) %>% 
  arrange(., Basin_ID)
  
write.csv(basin_alpha_values, "3_Output_data/R3_Groundwater_recession_basin.csv", quote = F, row.names = F) 

colnames(basin_alpha_values) <- c("Basin ID", "Basin", "Region", 
                                  "Average recession duration", "Average determination coefficient",
                                  "Average groundwater recession constant", "Average alpha value")


gt(basin_alpha_values)


#####  Results at region scale   ##### 

basin_alpha_values <- alphas_output_file %>% 
  group_by(Basin, Basin_ID) %>% 
  summarise_at(c("recess_days", 
                 "det_coef",
                 "gw_rec_const",
                 "alpha_value"), mean) %>% ungroup(.) %>%
  mutate_at(., c("det_coef", "gw_rec_const", "alpha_value"), ~ round(.x, 3)) %>% 
  mutate(recess_days = as.integer(recess_days)) %>% 
  left_join(., basins_file[,c("Basin_ID", "region")], "Basin_ID") %>% 
  select(Basin_ID, Basin, region, recess_days, det_coef, gw_rec_const, alpha_value) %>% 
  arrange(., Basin_ID)

region_alpha_values <- basin_alpha_values %>% 
  group_by(region) %>% 
  summarise_at(c("recess_days", 
                 "det_coef",
                 "gw_rec_const",
                 "alpha_value"), mean) %>% ungroup(.) %>%
  mutate_at(., c("det_coef", "gw_rec_const", "alpha_value"), ~ round(.x, 3)) %>% 
  mutate(recess_days = as.integer(recess_days)) %>% 
  select(region, recess_days, det_coef, gw_rec_const, alpha_value) 
  

colnames(region_alpha_values) <- c("Region", 
                                  "Average recession duration", "Average determination coefficient",
                                  "Average groundwater recession constant", "Average alpha value")
gt(region_alpha_values)
