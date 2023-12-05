#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 2: Runoff coefficient calculation ####
   
   # Methodology: the runoff coefficient for 19 subbasins within the upper sector of Tagus River basin has been calculated. Through this script,
   # the anual precipitation and the anual runoff have been calculated, deriving the runoff coefficient.
   # Temperature values in each of the basin has been also obtained in order to discuss the obtained runoff rate values.

   # Previous csv files were prepared with Script 1: 1_basins_file and 2_ids_stations_file, which contain data about the subbasins and their weather data points
   # In addition, as input data, daily weather (precipitation and temperature) and streamflow data are being used (see manuscript or README file for more info).

   # Used libraries
   library(readr)
   library(tidyverse)
   library(lubridate)
   library(plotly)
   library(gt)
   
   # Used files
   
       #Created csvs
       basins_file <- read.csv("Used_files/Created_csv/1_basins_file.csv") # File with IDs, names, regions and areas of the basin, and gauging stations codes  
       pcp_grid_points <-  read.csv("Used_files/Created_csv/2_ids_stations_file.csv") # File with IDs, names, and location of the grid points, and basins data  
       
       # Streamflow data
       gauging_data_tagus <- read.csv("Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
         tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
         .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))
       
       cods <- basins_file$gauging_code # Gauging stations codes for filtering
   
   #Checking the available gauging data dates: 
   gauging_data_tagus %>% filter(cod %in% cods, year(date) > 2009) %>% mutate(gauging_code = cod) %>% 
     group_by(gauging_code) %>% summarise(min(date), max(date)) %>% 
     left_join(., basins_file[,c(1:2,4)],by =  "gauging_code") %>% arrange(., Basin_ID) %>% .[,c(5,4,1,2,3)]
   
   #Checking if data is complete for the entire period (2010-2018)
   
   study_period <- 2010:2018
   
   ini <-  as.Date("2010/01/01")
   end <-  as.Date("2018/12/31")
   ndys <- seq(ini, end, 1) %>% length(.)
   
   complete_data <- gauging_data_tagus %>% filter(cod %in% cods, year(date) %in% study_period) %>%
     mutate(gauging_code = cod) %>% group_by(gauging_code) %>% summarise( ndays = n()) %>% 
     mutate('Complete data (2010-2018)' =  case_when(ndys == ndays ~ "Yes", 
     ndays < ndys ~ paste("No, ", ndys-ndays, " days missing", sep = ""))) %>% left_join(., basins_file, "gauging_code") %>%
     .[,c(5, 4, 3)] %>% arrange(., Basin_ID)
   complete_data
   
   ##### Report table 1: Summary of data availability   ##### 
   
  r1_tab <-  gauging_data_tagus %>% filter(cod %in% cods) %>%  
     mutate(gauging_code = cod) %>% 
     group_by(gauging_code) %>% summarise(min(year(date)), max(year(date))) %>% 
     left_join(., basins_file[,c(1:2,4)],by =  "gauging_code") %>% arrange(., Basin_ID) %>% .[,c(5,4,1,2,3)] %>% 
     left_join(., complete_data, "Basin_ID") %>% 
     mutate(available = paste(`min(year(date))`, `max(year(date))`, sep = "-")) %>% 
     .[, c("Basin_ID", "Basin.x", "available", "Complete data (2010-2018)")] %>% 
     rename( "Basin ID" = "Basin_ID", 
             "Basin" = "Basin.x", 
             "Period with available data" = "available") 
     gt(r1_tab) 
             
      write.csv(r1_tab, "Output_data/R1_Data_availability.csv", quote = F, row.names = F)       
             
   
   # IMPORTANT: Basins 2, 6 and 14 only have complete data from 2011 to 2018 in the case of 2 and 6, and from 2012 in the case of Basin 14. 
   # Therefore, for the runoff rate calculation, only these years will be used
   
   #### Runoff calculation ####
   
   areas <- basins_file$area # Drainage areas for converting flow to milimeters
   
   
   obs_anual <-  list() # Empty list 
   for(i in 1:length(cods)){
     gaug_st <- filter(gauging_data_tagus, cod == cods[i]) %>% filter(year(date) %in% 2010:2018) #Filtering with the gauging codes
     caud_anual <- gaug_st[,c("date", "obs_flow")] %>% group_by(year = year(date)) %>% summarise(., obs_m3 = mean(obs_flow)) %>% # Average annual flow (mB3/s)
        mutate(obs_mm = (obs_m3*86400*365*1000)/ (areas[i])) %>% # Annual contribution (mm/year)
       cbind(bas = i) %>% .[,c("bas", "year", "obs_mm")]  # Final table with Basin_ID, Year and Annual contribution
     obs_anual[[i]] <- caud_anual # List with the gauged data for all the basins
   }
   
  # User action: ONLY NEEDED IF OBSERVED DATA IS NOT COMPLETE
   
   # Basins 2 and 6 does also have data only October 2010. The first row (2010) is therefore eliminated
   obs_anual[[2]] <- obs_anual[[2]][-1,]
   obs_anual[[6]] <- obs_anual[[6]][-1,]
   
   # Basin 15 does also have data only from 2012. The first row (2011) is therefore eliminated
   obs_anual[[14]] <- obs_anual[[14]][-1,]
   
   
   
   
   #### Precipitation calculation ####
   
   path <- "Used_files/Data/Climate_data_extracted/pcp_spain/" # Directory where the precipitation file for each point of the grid is located
   
   init_date <- as.Date("1951-01-01")
   end_date <- as.Date("2019-12-31")
   dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire period with data is created
   
   pcp_grid_points <-  read.csv("Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  
   
   # Loop for calculating the annual precipitation of each basin trough the average of the annual precipitation for each station within the basin
   pcp_bas_list <- list() #empty list
   for(i in 1:length(unique(pcp_grid_points$Basin_ID))){  # i --> Basin ID
     filt_st <- filter(pcp_grid_points, Basin_ID == i) # Basin data and precipitations points inside
     stations <- filt_st[,1] #Precipitations points inside each basin
     pcps_sts <- c()
     for(n in 1:length(stations)){   # n --> Weather stations identifier within each basin
       st_dat <- read_table(paste(path, stations[n], "_PCP.txt", sep = ""), skip = 1, col_names = F) %>% #read the precipitation file for each point
         mutate(date = ymd(dates), pcp = X1) %>% .[,c("date", "pcp")] %>% group_by(year(date)) %>% 
         summarise(pcp_year = sum(pcp)) # calculate the total precipitation for each year
         colnames(st_dat) <- c("year", "pcp")
       
       pcp_st <- filter(st_dat, year %in% 2010:2018) %>% .[,"pcp"] # Filtering with the study period
       pcps_sts <- tibble(pcps_sts, pcp_st, .name_repair = "unique")    # Table with annual precipitation data for all the points of a basin 
       
     }
     pcp_bas <- pcps_sts %>% apply(., 1, mean) %>% cbind(year = c(2010:2018)) %>% 
       tibble(year = .[,"year"], pcp_y = .[,"."]) %>% .[,c("year", "pcp_y")]  # Calculate for each basin the average precipitation of all the precipitation points within
     pcp_bas_list[[i]] <- pcp_bas[, "pcp_y"] %>% cbind(year = c(2010:2018), bas = i) %>% .[,c("bas", "year", "pcp_y")] # Introduce in the list the obtained precipitation in order
   }
   
   #### Runoff rate calculation ####
   
   anual_runoff_rate <- list() #empty list 
   basin_runoff_rate <- list() #empty list
   basin_runoff_rates <- c()   #empty vector
   for(i in 1:length(pcp_bas_list)){
     anual_runoff_rate[[i]] <- obs_anual[[i]] %>% left_join(pcp_bas_list[[i]], by = "year") %>% 
                               mutate(Basin_ID = bas.x, Year = year, Pcp = pcp_y, Runoff = obs_mm, Runoff_rt = Runoff/Pcp) %>% 
                               .[,c("Basin_ID", "Year", "Pcp", "Runoff", "Runoff_rt")] # List with the annual values
     basin_runoff_rate[[i]] <- anual_runoff_rate[[i]] %>% summarise(Basin_ID = mean(Basin_ID), Mean_pcp = mean(Pcp), Mean_runoff = mean(Runoff), 
                          Runoff_rate = mean(Runoff_rt), Max_runoff_rate = max(Runoff_rt), min_runoff_rate = min(Runoff_rt), Runoff_rate_sd = sd(Runoff_rt)) %>% 
                          unlist(.) # List with the average precipitatoin, runoff and runoff rate values for the entire period; and maximum and minimum runoff rate
     
     basin_runoff_rates <- basin_runoff_rates %>% rbind(basin_runoff_rate[[i]]) # Merge the average list values
     
   }
   
   tib_basin_runoff_rates <-  basin_runoff_rates%>% data.frame(.) %>% tibble(.) 
   tib_basin_runoff_rates <- tib_basin_runoff_rates%>% left_join(., basins_file[,c(1:2,5)], "Basin_ID") %>% # Creating a table with the obtained values for each basin
                             .[,c("region", "Basin_ID" , "Basin", "Mean_pcp", "Mean_runoff", "Runoff_rate", "Runoff_rate_sd", "Max_runoff_rate", "min_runoff_rate")] 
   
   tib_region_runoff_rates <- tib_basin_runoff_rates %>% group_by(region) %>% summarise(Mean_pcp = mean(Mean_pcp) , Mean_runoff = mean(Mean_runoff ), 
                                                         Mean_runoffrt = mean(Runoff_rate ), Runoffrt_sd = mean(Runoff_rate_sd )) #Creating a table with the obtained values for regions
   
   #### Temperature calculation ####
   
   init_date <- as.Date("1951-01-01")
   end_date <- as.Date("2019-12-31")
   dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire serie is created
   
   path <- "Used_files/Data/Climate_data_extracted/tmp_spain/" # Directory where the temperature file for each point of the grid is located
   tmp_grid_points <-  read.csv("Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  
   
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
         mutate(date = ymd(dates), tmp_M = V1, tmp_m = V2) %>% .[,c("date", "tmp_M", "tmp_m")] %>% group_by(year(date)) %>% 
         summarise(tmp_M = mean(tmp_M),tmp_m = mean(tmp_m), tmp_mean = (tmp_M + tmp_m) /2) %>% .[,c(1,3,2,4)]
       
       colnames(st_dat_t) <- c("Year", "Average minimum temperature", "Average maximum temperature", "Mean temperature")
       
       tmp_min <- filter(st_dat_t, Year %in% 2010:2018) %>% .[,"Average minimum temperature"]
       tmp_mins <- tibble(tmp_mins, tmp_min, .name_repair = "unique")    # Minimum temperature for each station 
       tmp_max <- filter(st_dat_t, Year %in% 2010:2018) %>% .[,"Average maximum temperature"]
       tmp_maxs <- tibble(tmp_maxs, tmp_max, .name_repair = "unique")    # Maximum temperature for each station 
       tmp_mean <- filter(st_dat_t, Year %in% 2010:2018) %>% .[,"Mean temperature"]
       tmp_means <- tibble(tmp_means, tmp_mean, .name_repair = "unique") # Mean temperature for each station 
       
     }
     
     tmp_bas_list[[i]] <- tibble(Year = c(2010:2018), 
                                 Tmp_min = apply(tmp_mins, 1, mean),   # Minimun temperature for each basin
                                 Tmp_Max = apply(tmp_maxs, 1, mean),   # Maximun temperature for each basin
                                 Tmp_mean = apply(tmp_means, 1, mean)) # Mean temperature for each basin
   
      }
      
      #With this data, the average values for the entire period has been calculated for each basins
      # As Basins 2 and 6 only have gauged data for 2011-2018 and Basin 14 for 2012:2018, average temperature has been calculated for this period.
   
      tmp_bas_list[[2]] <- tmp_bas_list[[2]][-c(1),]   # Shortening data for Basin 2
      tmp_bas_list[[6]] <- tmp_bas_list[[6]][-c(1),]   # Shortening data for Basin 6
      tmp_bas_list[[14]] <- tmp_bas_list[[14]][-c(1,2),] # Shortening data for Basin 14
   
      # Loop for calculating the average minimun, maximun and mean temperature for all the basins
      mins <- c()
      maxs <- c()
      means <- c()
      for(i in 1:length(tmp_bas_list)){
       min <- mean(tmp_bas_list[[i]][[2]] )
       mins <- c(mins, min)
       max <- mean(tmp_bas_list[[i]][[3]] )
       maxs <- c(maxs, max)
       mean <- mean(tmp_bas_list[[i]][[4]] )
       means <- c(means, mean)
      }
      
      temperature_tibb <- tibble(Basin_ID = c(1:length(tmp_bas_list)), mins, maxs, means)
   
     
      
 #####  Report table 2: precipitation, temperature, runoff coefficient data   ##### 
      
     
      # Anual values (not printed as output)
      runoff_tmp_annual_list <- list() #For each basin, a list for the annual values
      for(i in 1:length(tmp_bas_list)){
        tmp <- tibble(tmp_bas_list[[i]])
        runoff <- tibble(anual_runoff_rate[[i]])
        tibb_merge <- left_join(tmp, runoff, "Year") %>% left_join(., basins_file[,c(1,2,5)], "Basin_ID") %>% .[,c(10,5,9,1,4,2,3,6,8)]
        colnames(tibb_merge) <- c("Region", "Basin_ID", "Basin", "Year", "Mean Temperature", "Min Temperature", "Max Temperature", "Mean Precipitation", "Runoff Rate")
        runoff_tmp_annual_list[[i]] <- tibb_merge
      }
      

      runoff_rate_tibble <- tib_basin_runoff_rates %>% 
        left_join(., temperature_tibb, "Basin_ID") %>% .[,c(1:3, 12,4, 6, 9, 8)] #For each basin, a summary table for all the period
      
      runoff_rate_tibble <- runoff_rate_tibble %>% 
        mutate(Mean_pcp = round(Mean_pcp,0), 
               Mean_tmp = round(means, 2), 
               Runoff_rate = round(Runoff_rate,3),
               min_runoff_rate = round(min_runoff_rate,3), 
               Max_runoff_rate = round(Max_runoff_rate,3)) %>% 
        select(., region, Basin_ID, Basin, Mean_tmp, Mean_pcp, Runoff_rate, min_runoff_rate, Max_runoff_rate)
      
      colnames(runoff_rate_tibble) <- c("Region", "Basin_ID", "Basin", "Mean Temperature", 
                                        "Mean Precipitation", "Mean Runoff rate", 
                                        "Min Runoff rate", "Max Runoff rate")
      
      write.csv(runoff_rate_tibble, "Output_data/R2_weather-runoff_data.csv", quote = F, row.names = F)
      
      gt(runoff_rate_tibble)
      
    