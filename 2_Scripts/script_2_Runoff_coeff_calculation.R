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
   basins_file <- read.csv("1_Used_files/Created_csv/1_basins_file.csv") # File with IDs, names, regions and areas of the basin, and gauging stations codes  
   pcp_grid_points <-  read.csv("1_Used_files/Created_csv/2_ids_stations_file.csv") # File with IDs, names, and location of the grid points, and basins data  
   
   # Streamflow data
   gauging_data_tagus <- read.csv("1_Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% # User action: Change path if necessary
     tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
     .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))
   
   cods <- basins_file$gauging_code # Gauging stations codes for filtering
     
  
   #### 1. Availability of streamflow observed data ####
   
   
   ##### 1.1 Checking the available gauging data dates:     ##### 
   range_data <- gauging_data_tagus %>% filter(cod %in% cods) %>% 
     mutate(gauging_code = cod) %>% 
     group_by(gauging_code) %>% summarise(min(date), max(date)) %>% 
     left_join(., basins_file[,c(1:2,4)],by =  "gauging_code") %>% arrange(., Basin_ID) %>% .[,c(5,4,1,2,3)] %>% 
     rename("First_record" = "min(date)", "Last_record" = "max(date)") %>% 
     mutate(years_data = year(Last_record)-year(First_record))
     
   range_data
   
   #####  1.2 Checking if data is complete for the years of the study period    ##### 
   
   study_period <- 2010:2018 # User action: Define study period years
   
   ini <-  as.Date(paste(study_period[1],"01/01", sep = "/"))
   end <-  as.Date(paste(study_period[length(study_period)],"12/31", sep = "/"))
   ndys <- seq(ini, end, 1) %>% length(.)
   # Number of days in the studied years
   ndys_yr <- tibble(date = seq(ini, end, 1)) %>% group_by(year(date)) %>% 
     summarise(days_yr = n())
   
   
   # Complete and uncomplete years
   
   complete_data_years <- gauging_data_tagus %>% 
     filter(cod %in% cods, year(date) %in% study_period) %>% 
     mutate(gauging_code = cod) %>% 
     group_by(gauging_code, year(date)) %>% summarise( ndays = n()) %>% 
     inner_join(., ndys_yr, "year(date)") %>% 
     mutate(years_incomplete = case_when(ndays == days_yr ~ "Complete",
     ndays != days_yr ~ (paste(`year(date)`, "Incomplete"))), days_left = days_yr-ndays)
   
   uncomp_years <- complete_data_years %>% 
     filter(years_incomplete != "Complete") %>% 
     rename("year" = "year(date)") %>% 
     select(gauging_code, year ,days_left)  %>% 
     summarise(years = paste(year, collapse = ", "),
               Days_left  = paste(days_left, collapse = ", ")) %>% 
     mutate(Data_availability = paste("Uncomplete years: ", years, ".", " Days left = ", Days_left, sep = "")) 
   
   dat_avail_tab <- complete_data_years %>% filter(years_incomplete == "Complete") %>% 
     group_by(gauging_code) %>% summarise("Data_availability" = unique(years_incomplete)) %>%
     mutate(years = "None", Days_left = "0") %>% select(gauging_code, years, Days_left, Data_availability)

   dat_avail_tab[dat_avail_tab$gauging_code %in% uncomp_years$gauging_code ,]  <-   uncomp_years
   
   
   # Years that are totally incomplete
   unc_years <-  dat_avail_tab %>% left_join(., range_data, "gauging_code") %>% select(Basin_ID , First_record, Last_record) %>% 
     mutate(min_yr = year(First_record), max_yr = year(Last_record)) %>% arrange(Basin_ID) %>% select(Basin_ID, min_yr, max_yr) %>% 
     mutate(lack_yrs = case_when(min_yr >year(ini) & max_yr < year(end)~ "Both",
                                 min_yr > year(ini) ~ "Initial",
                                 max_yr < year(end) ~ "End", 
                                 .default = "None")) %>% filter(., lack_yrs != "None")
   
         basins_unc <- c()
         
         for(i in 1:length(unc_years$Basin_ID)){ 
          
            init_yrs <- c()
            end_yrs <- c()
           
           bas_d <- unc_years %>%  filter(., Basin_ID == unc_years$Basin_ID[i])
          
           if(bas_d$lack_yrs == "Initial"){ 
             init_yrs <- paste(seq(year(ini), bas_d$min_yr-1, 1), collapse = ",")
             }
           
           if(bas_d$lack_yrs == "End"){ 
             end_yrs <- paste(seq(bas_d$max_yr+1,year(end),  1), collapse = ",")
             }
           
           if(bas_d$lack_yrs == "Both"){ 
             init_yrs <- paste(seq(year(ini), bas_d$min_yr-1, 1), collapse = ",")
             end_yrs <- paste(seq(bas_d$max_yr+1,year(end),  1), collapse = ",")
             
             }
           
           basin_unc <- tibble(Basin_ID = unc_years$Basin_ID[i], uncount_yrs =  paste(init_yrs, end_yrs))
           basins_unc <- basins_unc %>% rbind(basin_unc)
          
          }
              
   
   #####   1.3. Creating a table with all this information   #####  
   R1_dat_avail_table <-  dat_avail_tab %>%  
     left_join(., range_data, "gauging_code") %>% 
     left_join(., basins_unc, "Basin_ID") %>% 
     left_join(., basins_file[,c("Basin_ID", "area")], "Basin_ID") %>% 
     mutate(Data_availability = paste("Years without records = ", uncount_yrs, 
     ", ", "Uncomplete Years = ", years, ",", " days left = ", Days_left, sep = "")) %>% 
     mutate(Data_availability = str_remove(Data_availability, "Years without records = NA, ")) %>%
     mutate(min_yr = year(First_record), max_yr = year(Last_record)) %>% 
           mutate(minmax_yr = paste(min_yr, "-", max_yr, " (", years_data, ")", sep = "")) %>% 
           mutate(Area_sqkm = round(area/1e6, 2)) %>% 
     select(Basin_ID, Basin, gauging_code, Area_sqkm , minmax_yr, Data_availability) %>% arrange(Basin_ID) %>% 
     mutate(Data_availability = case_when(Data_availability == "Uncomplete Years = None, days left = 0"~ "Complete", .default = Data_availability)) %>% 
     rename("Gauging_code" = "gauging_code", "Range_obs_data" = "minmax_yr" , "Study_period_data_avail" = "Data_availability")
    
              
              
     
   
   ##### Report table 1: Summary of data availability   ##### 
   
   write_csv(R1_dat_avail_table, "3_Output_data/R1_Data_availability.csv")       
   
   read.csv("3_Output_data/R1_Data_availability.csv") %>% 
     mutate(Basin = paste(Basin_ID, ", ", Basin, " (", Gauging_code, ")", sep = "")) %>% 
     select(Basin, Range_obs_data, Study_period_data_avail, Area_sqkm) %>% 
     rename("Basin data" = "Basin", "Area (km²)" = "Area_sqkm",
            "Range and number of years with observed data" = "Range_obs_data",
            "Data avaiability for the studied period"= "Study_period_data_avail") %>% 
     gt(.)      
             
   
   
   
   
   #### 2. Runoff calculation ####
   
   study_period <- c(2010:2018) # User action: Define study period
   areas <- basins_file$area # Drainage areas for converting flow to milimeters
   
   
   obs_anual <-  list() # Empty list 
   for(i in 1:length(cods)){
     gaug_st <- filter(gauging_data_tagus, cod == cods[i]) %>% filter(year(date) %in% study_period) #Filtering with the gauging codes
     caud_anual <- gaug_st[,c("date", "obs_flow")] %>% group_by(year = year(date)) %>% summarise(., obs_m3 = mean(obs_flow)) %>% # Average annual flow (mB3/s)
        mutate(obs_mm = (obs_m3*86400*365*1000)/ (areas[i])) %>% # Annual contribution (mm/year)
       cbind(bas = i) %>% .[,c("bas", "year", "obs_mm")]  # Final table with Basin_ID, Year and Annual contribution
     obs_anual[[i]] <- caud_anual # List with the gauged data for all the basins
   }
   
   
  # User action: ONLY NEEDED IF OBSERVED DATA IS NOT COMPLETE
   
   # In the case study, Basins 2, 6 and 14 only have complete data from 2011 to 2018 in the case of 2 and 6, and from 2012 in the case of Basin 14. 
   # Therefore, for the runoff rate calculation, only these years were used
   
   # Basins 2 and 6 does also have data only October 2010. The first row (2010) is therefore eliminated
   obs_anual[[2]] <- obs_anual[[2]] %>% filter(year > 2010)
   obs_anual[[6]] <- obs_anual[[6]] %>% filter(year > 2010)
   
   # Basin 15 does also have data only from 2012. The first row (2011) is therefore eliminated
   obs_anual[[14]] <- obs_anual[[14]] %>% filter(year > 2011)
   
   

   #### 3. Precipitation calculation ####
   
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
         mutate(date = ymd(dates), pcp = X1) %>% .[,c("date", "pcp")] %>% group_by(year(date)) %>% 
         summarise(pcp_year = sum(pcp)) # calculate the total precipitation for each year
         colnames(st_dat) <- c("year", "pcp")
       
       pcp_st <- filter(st_dat, year %in% study_period) %>% .[,"pcp"] # Filtering with the study period
       pcps_sts <- tibble(pcps_sts, pcp_st, .name_repair = "unique")    # Table with annual precipitation data for all the points of a basin 
       
     }
     pcp_bas <- pcps_sts %>% apply(., 1, mean) %>% cbind(year = c(study_period)) %>% 
       tibble(year = .[,"year"], pcp_y = .[,"."]) %>% .[,c("year", "pcp_y")]  # Calculate for each basin the average precipitation of all the precipitation points within
     pcp_bas_list[[i]] <- pcp_bas[, "pcp_y"] %>% cbind(year = c(study_period), bas = i) %>% .[,c("bas", "year", "pcp_y")] # Introduce in the list the obtained precipitation in order
   }
   
   
   #### 4. Runoff rate calculation ####
   
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
   
   
   #### 5. Temperature calculation ####
   
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
         mutate(date = ymd(dates), tmp_M = V1, tmp_m = V2) %>% .[,c("date", "tmp_M", "tmp_m")] %>% group_by(year(date)) %>% 
         summarise(tmp_M = mean(tmp_M),tmp_m = mean(tmp_m), tmp_mean = (tmp_M + tmp_m) /2) %>% .[,c(1,3,2,4)]
       
       colnames(st_dat_t) <- c("Year", "Average minimum temperature", "Average maximum temperature", "Mean temperature")
       
       tmp_min <- filter(st_dat_t, Year %in% study_period) %>% .[,"Average minimum temperature"]
       tmp_mins <- tibble(tmp_mins, tmp_min, .name_repair = "unique")    # Minimum temperature for each station 
       tmp_max <- filter(st_dat_t, Year %in% study_period) %>% .[,"Average maximum temperature"]
       tmp_maxs <- tibble(tmp_maxs, tmp_max, .name_repair = "unique")    # Maximum temperature for each station 
       tmp_mean <- filter(st_dat_t, Year %in% study_period) %>% .[,"Mean temperature"]
       tmp_means <- tibble(tmp_means, tmp_mean, .name_repair = "unique") # Mean temperature for each station 
       
     }
     
     tmp_bas_list[[i]] <- tibble(Year = c(study_period), 
                                 Tmp_min = apply(tmp_mins, 1, mean),   # Minimun temperature for each basin
                                 Tmp_Max = apply(tmp_maxs, 1, mean),   # Maximun temperature for each basin
                                 Tmp_mean = apply(tmp_means, 1, mean)) # Mean temperature for each basin
   
      }
      
      #With this data, the average values for the entire period has been calculated for each basins
      # As Basins 2 and 6 only have gauged data for 2011-2018 and Basin 14 for 2012:2018, average temperature has been calculated for this period.
   
      # User action: ONLY NEEDED IF OBSERVED DATA IS NOT COMPLETE
   
      tmp_bas_list[[2]] <- tmp_bas_list[[2]] %>% filter(Year > 2010)   # Shortening data for Basin 2
      tmp_bas_list[[6]] <- tmp_bas_list[[6]] %>% filter(Year > 2010)   # Shortening data for Basin 6
      tmp_bas_list[[14]] <- tmp_bas_list[[14]] %>% filter(Year > 2011) # Shortening data for Basin 14
   
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
   
     rm(min, max, mean)
      
 #####  Report table 2: precipitation, temperature, runoff coefficient data   ##### 
      
     
      # Anual values (not printed as output)
      runoff_tmp_annual_list <- list() #For each basin, a list for the annual values
      for(i in 1:length(tmp_bas_list)){
        tmp <- tibble(tmp_bas_list[[i]])
        runoff <- tibble(anual_runoff_rate[[i]])
        tibb_merge <- left_join(tmp, runoff, "Year") %>% left_join(., basins_file[,c(1,2,5)], "Basin_ID") %>% .[,c(10,5,9,1,4,2,3,6,8)]
        colnames(tibb_merge) <- c("Region", "Basin_ID", "Basin", "Year", "Mean Temperature", 
                                  "Min Temperature", "Max Temperature", "Mean Precipitation", "Runoff Rate")
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
      
      write.csv(runoff_rate_tibble, "3_Output_data/R2_weather-runoff_data.csv", quote = F, row.names = F)
      
      gt(runoff_rate_tibble)
      
    