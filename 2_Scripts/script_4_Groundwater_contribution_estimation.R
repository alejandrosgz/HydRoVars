#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 4: Groundwater contribution estimation using a baseflow filter function ####  


   #Methodology: Baseflow indexes have been calculated for each basin using the Baseflow filter proposed by Eckhardt, K. (2005) (Ecuation 19) and writted in R by
   # Hendrik Rathjens. This filter have two parameters: BFIMax, which is the maximum expected value of baseflow contribution expected for one day, and alpha, 
   # which is a constant obtained from the groundwater recession constant (alpha = e^α). For each basin, a range of possible values of alohas was calculated in the 
   # Script 3 (alphas_calculation), and the BFIMax values have been established depending on the lithology and properties of each river and adjusting it for different 
   # peaks (note that these peaks were not always the same as the used for the alpha estimation). When appropriated values of alpha and BFIMax has been chosen, 
   # the groundwater contribution has been estimated. 
   # Daily precipitation graphs have been used to determine a realistic baseflow contribution rate
   
   # NOTE THAT
   # For running the script FIRSTLY is necessary to run the "RUN THIS FIRST" section. This section contains the alpha values obtained, the streamflow data,
   # the code used to create a list with the daily precipitation for each basin and the baseflow filter function used to separate the hydrograph components.
    
   # As input data, the files with streamflow data and wih the subbasins data have been used.
   # An output csv file with the obtained baseflow index and the values of the parameters has been generadted (4_groundwater_results)     

   # Used libraries
   library(readr)
   library(tidyverse)
   library(lubridate)
   library(plotly)
   library(gt)
   library(patchwork)  

   
   
   ####RUN THIS FIRST####
   
   # Baseflow Filter Function, written by Hendrik Rathjens. The two_param method has been used in all the cases
   # Baseflow filter equation source
   #https://doi.org/10.1002/hyp.5675
   #https://user.engineering.uiowa.edu/~flood/handouts/HO-L17-Baseflow-Separation.pdf
   # Recommended values of BFImax --> Perennial streams, porous aquifers (0.8), hard rock aquifers (0.25); Ephemeral streams porous aquifers (0.5)

   baseflow_sep <- function(df=NA, Q="Q",
                            alpha=0.98,
                            BFIma=0.5,
                            method="two_param")
   {
     Q <- df[colnames(df) == Q][,1]
     Q[is.na(Q)] <- -9999.9
     R <- as.vector(matrix(data=NA, nrow=length(Q), ncol=1))
     B <- as.vector(matrix(data=NA, nrow=length(Q), ncol=1))
     R[1] <- 0
     B[1] <- 0
     # Nathan McMahon (1990): Evaluation of Automated techniques for base flow and recession Analyses (WRR 26, 1465-1473)
     if(method=="one_param") {    
       for(i in 2:length(Q)){
         if(Q[i] != -9999.9){
           R[i] <- alpha * R[i-1] + (1+alpha)/2 * (Q[i]-Q[i-1])
           if(R[i] < 0)    {R[i] <- 0}
           if(R[i] > Q[i]) {R[i] <- Q[i]}
           B[i] <- Q[i]-R[i]
         } else {
           R[i] <- NA
           B[i] <- NA
         }
       }
     }
     #Eckhardt (2005): How to construct recursive digital filters for baseflow separation (Hydrological Processes, 19, 507-515)
     if(method=="two_param") {
       for(i in 2:length(Q)){
         if(Q[i] != -9999.9){
           B[i] <- ((1-BFIma)*alpha * B[i-1] + (1-alpha)*BFIma* Q[i]) /
             (1-alpha*BFIma)
           if(B[i] > Q[i]){B[i] <-Q[i]} 
           R[i] <- Q[i]-B[i]
         } else {
           R[i] <- NA
           B[i] <- NA
         }
       }
     }
     return(data.frame(B,R))
   }
   
   
   # File with IDs, names, regions and areas of the basin, and gauging stations codes  
   basins_file <- read.csv("Used_files/Created_csv/1_basins_file.csv") 
   
   # Gaugin stations data
   gauging_data_tagus <- read.csv("Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
     tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
     .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))
   
   #Alphas calculated in previous script
   alphas_tibble <- read.csv("Used_files/Created_csv/3_alpha_estimation.csv")
   
   
   # Daily precipitation obtention
   path <- "Used_files/Data/Climate_data_extracted/pcp_spain/" # Directory where the precipitation file for each point of the grid is located
   init_date <- as.Date("1951-01-01")
   end_date <- as.Date("2019-12-31")
   dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire period with data is created
   period_dates <- tibble(dates) %>% filter(., year(dates) %in% 2010:2018)
   pcp_grid_points <-  read.csv("Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  
   
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
   
   
   
   
   #### SCRIPT TO ESTIMATE GROUNDWATER CONTRIBUTION ####
   
   
   # Period that is being analysed (years)
   evaluated_period <- c(2010:2018) # USER ACTION
   
   ##### 0 --> Extracting data for each subbasin: Define the Basin_ID #####  
   basin_ID <- 19 # USER ACTION
   
   basin_information <- paste("Basin ", basins_file$Basin_ID[basin_ID], ", ",
                              basins_file$Basin[basin_ID], " (gauging code = ",
                              basins_file$gauging_code[basin_ID], ")", sep = "")
   
   streamflow_data <- gauging_data_tagus %>% 
     filter(., cod == basins_file$gauging_code[basin_ID],
            year(date) %in% evaluated_period) 
   
   
   precipitation_data <- tibble(pcpday_bas_list[[basin_ID]]) 
   
   basin_data <- streamflow_data %>% left_join(precipitation_data, "date") %>%
     mutate(day = seq(1, length(.$date), 1)) %>% .[,c(1,2,4,3,5)] %>% data.frame(.)
   
   
   alpha_info <- alphas_tibble %>% filter(Basin_ID == basin_ID) 
    
   peak_1 <- tibble(rng = alpha_info$peak_range[1]) %>% 
     separate(rng, c("min", "max"), sep = ":") %>% 
     mutate_all(., as.numeric)
   peak_2 <- tibble(rng = alpha_info$peak_range[2]) %>% 
     separate(rng, c("min", "max"), sep = ":") %>% 
     mutate_all(., as.numeric)
   peak_3 <- tibble(rng = alpha_info$peak_range[3]) %>% 
     separate(rng, c("min", "max"), sep = ":") %>% 
     mutate_all(., as.numeric)
   
   
   ##### 1 --> Changing filter parameters #####  
   
   # Estimated alpha values (printed to know the estimated values)
   alpha_info$alpha_value
   mean(alpha_info$alpha_value)
   
   #Filter parameters
   alpha <- 0.993 # USER ACTION
   bfi_max <- 0.5 # USER ACTION
   
  
   ##### 2 --> Apply the filter to estimate baseflow #####  
   
   bfsep <- baseflow_sep(df = basin_data, 
                         Q = "obs_flow", 
                         alpha = alpha, 
                         BFIma =bfi_max, 
                         method = "two_param") # Run baseflow filter with the selected parameters
   
   bf_sep_data <- basin_data %>% # Precipitation and streamflow data
     mutate(baseflow = bfsep$B, runoff = bfsep$R) # Adding the baseflow filter output
   
   
   ##### 3 --> Observe output for the selected peaks #####  
   # Repeat steps 1-3 until achieve a good separation for the three peaks
   
   # Peak 1
   # Streamflow separation plot
   bf_plot_1 <- ggplot(bf_sep_data[peak_1$min:peak_1$max,], aes(x = date))+ 
     geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ 
     scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   # Precipitation  plot
   pcp_plot_1 <- ggplot(bf_sep_data[peak_1$min:peak_1$max,], aes( x=date))+
     geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+ 
     scale_y_reverse()+theme_bw()+ xlab(label = "")+theme(axis.text.x = element_blank(), 
     axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   # Plot of both variables
   (pcp_plot_1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   # Peak 2
   bf_plot_2 <- ggplot(bf_sep_data[peak_2$min:peak_2$max,], aes(x = date))+ 
     geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ 
     scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   
   pcp_plot_2 <- ggplot(bf_sep_data[peak_2$min:peak_2$max,], aes( x=date))+
     geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+ 
     scale_y_reverse()+theme_bw()+ xlab(label = "")+theme(axis.text.x = element_blank(), 
     axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot_2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   # Peak 3
   bf_plot_3 <- ggplot(bf_sep_data[peak_3$min:peak_3$max,], aes(x = date))+ 
     geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ 
     scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   
   pcp_plot_3 <- ggplot(bf_sep_data[peak_3$min:peak_3$max,], aes( x=date))+
     geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+ 
     scale_y_reverse()+theme_bw()+ xlab(label = "")+theme(axis.text.x = element_blank(), 
     axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot_3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   
   # Entire serie (not recommended as first criteria for parameters values decision)
   bf_plot <- ggplot(bf_sep_data, aes(x = date))+ 
     geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ 
     scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   
   pcp_plot <- ggplot(bf_sep_data, aes( x=date))+
     geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+ 
     scale_y_reverse()+theme_bw()+ xlab(label = "")+theme(axis.text.x = element_blank(), 
     axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   
   ##### 4 --> Calculate the baseflow index and create basin output data #####  

   bf_index <- sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R)) #Calculating the baseflow contribution
   bf_index
   
   basin_gwc_data <- tibble(basins_file[basins_file$Basin_ID == basin_ID, c("Basin", "Basin_ID", "region")]) %>% 
     mutate(alpha = round(alpha,3), bfi_max_used = round(bfi_max,3), BF_Rate = round(bf_index,3))
     
   
   ##### 5 --> Create and save output data for all the basin #####  
   
  # basins_gwc_data <- c() # USER ACTION: Remove the # and run, then include again the #. This line should be run only one time, after the first basin evaluation

   basins_gwc_data <- basins_gwc_data %>% rbind(., basin_gwc_data)

   write.csv(basins_gwc_data, #  USER ACTION: Remove the # and run, then include again the #. These lines should be run only one time, after the last basin evaluation
             "Used_files/Created_csv/4_groundwater_results.csv", quote = F, row.names = F)

    
#### END OF SCRIPT TO ESTIMATE GROUNDWATER CONTRIBUTION ####
   
   
   
   #### REPORT TABLES ####
   
   # Reading the saved data
   gw_output_file <- read.csv("Used_files/Created_csv/4_groundwater_results.csv")
   
   
   #####  Report table 5: Results at basin scale   ##### 
   
   # Average values at basin scale
   
   
   basin_gw_values <- gw_output_file %>% 
     rename("Region" = "region",
            "Alpha used" = "alpha",
            "BFImax used" = "bfi_max_used",
            "Estimated baseflow contribution" = "BF_Rate")
   
   write.csv(basin_gw_values, "Output_data/R5_Groundwater_contribution_basin.csv", quote = F, row.names = F) 
   
   
   
   
   #####  Report table 5: Results at region scale   ##### 
   
   region_gw_values <- gw_output_file %>% 
     group_by(region) %>% 
     summarise(mean_alpha = mean(alpha), 
               mean_bfi = mean(bfi_max_used),
               mean_bfr = mean(BF_Rate),
               sd_bfr = sd(BF_Rate)) %>% 
     rename("Region" = "region",
            "Mean alpha used" = "mean_alpha",
            "Mean BFImax used" = "mean_bfi",
            "Mean estimated baseflow contribution" = "mean_bfr",
            "Baseflow contribution standard deviation" = "sd_bfr")
   
   write.csv(region_gw_values, "Output_data/R6_Groundwater_contribution_region.csv", quote = F, row.names = F) 
   
   
   
   #### Plots #### 
   
   # Parameters effect assessment
   
   priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   obs_data <- as.data.frame(priegoes_flow)
   
   # Alpha effect
   bfsep <- c()
   bf <- c()
   bfs <- c()
   alpha <- c()
   alphas <- c()
   id <- c()
   ids <- c()
   
   np <- 30 # Number of simulations
   alpha_min <- 0.9 # Minimum alpha value
   alpha_max <- 0.995 # Maximum alpha value
   BFI_MAX <-0.6 # BFIMax value
   
   alpha_var <- (alpha_max-alpha_min)/np # Alpha value variation
   
   for(i in 1:np){ # Running the filter 30 times changing alpha value
     bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alpha_min+(i*alpha_var), BFIma = BFI_MAX, method = "two_param")
     bf <- bfsep$B
     bf[1] <- bf[2]
     bfs <- c(bfs, bf)
     alpha <-  rep(alpha_min+(i*alpha_var), length(bf))
     alphas <- c(alphas, alpha)
     id <- rep(paste("run_",i), length(bf)) 
     ids <- c(ids, id)
   }
   
   baseflows_alpha_chg <- tibble(bfs, alphas, ids, obs = rep(obs_data$obs_flow,np), days = rep(1:length(bf), np)) 
   
   asss_alpha <- ggplot(baseflows_alpha_chg)+ geom_line(aes(x = days, y = bfs, group = ids, colour = alphas), size = 1)+ # Plot of the simulations
     geom_line(aes(x= days, y = obs),  colour = "black", size = 1.1)+ 
     scale_colour_gradient(
       high = "#132B43",
       low = "#E0FFFF",
       space = "Lab",
       na.value = "red",
       guide = "colourbar",
       aesthetics = "colour")+
     xlim(c(1140,1220))+ylim(c(0,45))+xlab("Time (days)")+ylab("Streamflow (m³/s)")+labs(colour = "Alpha value")+
     ggtitle("Alpha parameter effect (BFIMax = 0.6)")+theme_bw()+theme(text = element_text(size = 18), axis.text = element_text(size = 16, color = "black"))
   
    alpha_plot <- asss_alpha
    
   # BFImax effect
   bfsep <- c()
   bfs <- c()
   bfis <- c()
   alpha <- c()
   alphas <- c()
   id <- c()
   ids <- c()
   
   np <- 30  # Number of simulations
   bfi_min <- 0.1 # Minimum BFIMax value
   bfi_max <- 0.8 # Maximum BFIMax value
   
   bfi_var <-(bfi_max - bfi_min)/np  # BFIMax value variation
   
   alpha_used <-  0.985 # alpha value
   
   for(i in 1:np){ # Running the filter 30 times changing alpha value
     bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alpha_used, BFIma = bfi_min+(i*bfi_var), method = "two_param")
     bf <- bfsep$B
     bf[1] <- bf[2]
     bfs <- c(bfs, bf)
     bfi_used <-  rep(bfi_min+(i*bfi_var), length(bf))
     bfis <- c(bfis, bfi_used)
     id <- rep(paste("run_",i), length(bf)) 
     ids <- c(ids, id)
   }
   
   baseflows_bfi_chg <- tibble(bfs, bfis, ids, obs = rep(obs_data$obs_flow,np), days = rep(1:length(bf), np)) 
   
   asss_bfimax <- ggplot(baseflows_bfi_chg)+ geom_line(aes(x = days, y = bfs, group = ids, colour = bfis), size = 1)+ # Plot of the simulations
     geom_line(aes(x= days, y = obs),  colour = "black", size = 1.1)+ 
     scale_colour_gradient(
       high = "#132B43",
       low = "#E0FFFF",
       space = "Lab",
       na.value = "red",
       guide = "colourbar",
       aesthetics = "colour")+
     xlim(c(1140,1220))+ylim(c(0,45))+xlab("Time (days)")+ylab("Streamflow (m³/s)")+labs(colour = "BFIMax value")+
     ggtitle("BFIMax parameter effect (alpha = 0.985)")+theme_bw()+theme(text = element_text(size = 18), axis.text = element_text(size = 16, color = "black"))
   
   bfi_plot <- asss_bfimax

   
  library(patchwork)
  pars_effect_plot <-  bfi_plot + alpha_plot # Plot of the two parameters effect
  
  #ggsave(plot = pars_effect_plot, filename = "Figures/parameters_effect_plot.tiff",
         device = "png", width = 18, height = 12, dpi = 600)
  
  
  
  # Baseflow filter example plot  
  
  # Example of baseflow filter application to Subbasin 4
  # Mean Alpha obtained : 0.982, Max 0.986, Min 0.976
  peralejo_flow <-  gauging_data_tagus %>% filter(., cod == 3001) %>% 
    filter(year(date) %in% 2010:2018) %>% 
    mutate(day = seq(1, length(date), 1))
  peralejo_pcp <- pcpday_bas_list[[4]] 
  # Joining pcp and streamflow data by date and creating data frame
  peralejo_all <- peralejo_flow %>% left_join(peralejo_pcp, "date") %>% 
    as.data.frame(.[,c(1,2,4,3,5)])
  #Filter parameters
  alpha <- 0.982
  bfi_max <- 0.5
  # Running the filter
  bfsep <- baseflow_sep(df = peralejo_all, 
                        Q = "obs_flow", 
                        alpha = alpha, 
                        BFIma =bfi_max, 
                        method = "two_param")
  bfsep_tibble <- peralejo_all %>% mutate(baseflow = bfsep$B, #baseflow
                                          runoff = bfsep$R)   #runoff
  # Calculating the streamflow contribution
  sum(bfsep_tibble$baseflow) / (sum(bfsep_tibble$baseflow)+sum(bfsep_tibble$runoff))
  # Checking the adjustment for one of the peaks
  peak_2 <- bfsep_tibble[c(1440:1710),]
  bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
    geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Streamflow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
  pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill =     "skyblue")+  scale_y_reverse()+theme_bw()+
    xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
  plot_b4 <- (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
  
  #ggsave(plot = plot_b4, filename = "Figures/subb_4_bfsep.png",
         device = "png", width = 10, height = 8, dpi = 600)
  #ggsave(plot = plot_b4, filename = "Figures/subb_4_bfsep.tiff",
         device = "tiff", width = 10, height = 8, dpi = 600)
  



  # Example plot of time scale effect on baseflow separation
  
  # Example with Basin 6, Priego Escabas, gauging code = 3045, region = CRB
  #Mean Alpha obtained : 0.975, Max 0.99,  Min 0.965
  
  priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% 
    mutate(day = seq(1, length(date), 1))
  
  obs_data <- as.data.frame(priegoes_flow)
  
  #Filter parameters
  alpha <- 0.985
  bfi_max <- 0.6
  
  bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alpha, BFIma =bfi_max, method = "two_param") # Running the filter
  n <- priegoes_flow %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
  
  # Peak example:  1070:1300
  peak_1 <- n[c(1070:1300),]
  bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
    geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Streamflow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "")
  
  # Entire period
  bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
    geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "") + 
    annotate("rect",xmin = as.Date(peak_1$date[1]), xmax = as.Date(peak_1$date[length(peak_1$date)]), 
             ymin = -2, ymax = 45,  color = "blue", linetype = 2, fill = "transparent")
  
  # Plot comparing the peak regarding the entire time period
  plott <- bf_plot_1 /bf_plot 
  
  #ggsave(plot = plott, filename = "Figures/subb_4_bfsep.png",
         device = "png", width = 14, height = 10, dpi = 600)
