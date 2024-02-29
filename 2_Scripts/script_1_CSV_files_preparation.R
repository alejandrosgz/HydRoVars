#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 1: Creating csv files ####

   # Methodology: two csv files are necessary to perform this methodology. These files can be created from two vector layers; one 
   # with the basins delineated, and the other with the grid of weather data. This script can be used as example to create these csv
   # files.
   
   # Used libraries
   library(sf)
   library(tidyverse)

   # Environment: set working directory to the main folder of the repository if you are in other working directory

   #setwd("......\Soft_data_collection_methodology")
   
   
   # FILE 1.- basins csv file
   
   # Input data: Shapefile with the delineated basins. 
   basins <- read_sf("1_Used_files/GIS/Shapefiles/basins_studied.shp") %>% arrange(., id)
   # Changing column names
   basins_csv <- basins %>% rename(Basin_ID = id) %>% 
   #Calculating area
   mutate(area = st_area(.)) %>% 
   # Introducing the gauging stations codes (Manually) # User action: Introduce the gauging stations code. # Introduce NA if there is no aguging statoin but you want to evaluate climate with Script 6
   mutate(gauging_code = c(3231, 3049, 3211, 3001, 3045, 3040,
                           3249, 3172, 3193, 3251, 3030, 3173,
                           3164, 3165, 3212, 3268, 3237, 3186 , 3060)) %>% 
   # Spatial data is no longer necessary
   st_drop_geometry(.) %>% 
   # Ordering table
   .[,c("Basin", "Basin_ID", "area", "gauging_code", "region")]
   write.csv(x = basins_csv, file = "1_Used_files/Created_csv/1_basins_file.csv", row.names = F)
   
   
   # FILE 2. Gauging points csv file
   
   #Input data: weather grid and delineated basins. NOTE THAT BOTH CRSs MUST BE THE SAME.
   
   # Grid points: Note that the IDs for precipitation and temperature stations is constant, and therefore only one file is necessary.
   pcp_points <- read_sf("1_Used_files/GIS/Shapefiles/weather_grid_UTM.shp")
   
   basins <- read_sf("1_Used_files/GIS/Shapefiles/basins_studied.shp") %>% arrange(., id)
   
   # If CRS is not the same, CRS of the studied area is changed
   
   if( st_crs(pcp_points) == st_crs(basins)){
     print("CRSs was the same")
   } else { 
     basins <- sf::st_transform(basins, crs =  st_crs(pcp_points))
     print("Study area CRS was modified")
   }
   
   # 2.1. Buffer created for basins (1 km distance)
   
   basins_buffer <- st_buffer(basins, dist = 1000) # User action: define buffer (m)
   
   # 2.2.Clipping grid points with the basins buffer (region column is not necessary)
   
   grid_points_clip <- st_intersection(pcp_points, basins_buffer[, c("id", "Basin", "geometry")])
   
   # Spatial data is no longer necessary, and a variable is renamed before saving
   
   grid_points_clip_csv <- grid_points_clip %>% st_drop_geometry(.) %>% rename(Basin_ID = id)
   
   write.csv(x = grid_points_clip_csv, file = "1_Used_files/Created_csv/2_ids_stations_file.csv", row.names = F)
   
   
   
   # Plot to see the selected points
   
   basins$region <- factor(basins$region, levels = c("DTAL", "DTBJ", "CRB", "MIX", "IMP"), 
                                       labels = c("Detrital, High permeability", "Detrital, Low permeability", 
                                                  "Carbonate", "Mixed", "Impervious"))
   pcps_selected_id <- grid_points_clip$ID
   grid_points <- pcp_points %>% mutate(Selected_points = case_when(ID %in% pcps_selected_id ~ "Selected", 
                                   TRUE ~ "No selected"))
   
   tagus_upp <- read_sf("1_Used_files/GIS/Shapefiles/modeled_basin.shp")
   
     ggplot()+
     geom_sf(data = tagus_upp, fill = "transparent", color = "black", linewidth = 1)+
     geom_sf(data = basins, aes(fill = as.factor(id), color = region), linewidth = 1)+ labs(color = "Lithology")+
     scale_color_manual(values = c("orange", "darkgrey", "blue", "purple", "red"))+
     geom_sf(data = basins_buffer, fill = "transparent", linetype = 2, linewidth = 0.7)+
     guides(fill = "none")+ 
     geom_sf(data = grid_points, aes(shape = Selected_points), size = 2)+
     scale_shape_manual(values = c(1, 16))+
     theme_bw()+
       coord_sf(crs = st_crs('+proj=moll'),xlim = c(-130000, -440000), 
                ylim = c(4710000, 4950000))
   












