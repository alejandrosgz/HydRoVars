# HydRoVars: An R tool to collect hydrological variables

This is the accompanying code repository for the manuscript *"HydRoVars: An R tool to collect hydrological variables."*. The repository contains the data and code which were used to perform the soft-data collection methodology presented in the manuscript. Feel free to use the methodology or any parts of the code, but please make sure to cite our work (see [License](#license) and [Citation](#citation)).

### Purpose and utility

In hydrology soft data can be used to characterize the hydrological behavior of a basin or region and can therefore be used in the soft calibration process for a hydrological model setup of a basin. In countries such as Spain there are several potential sources for collecting soft data for the entire territory, such as weather and hydrological data. Yet, studies which include soft data in hydrological modeling are few and often limited to small areas and short time series. 

In this work, we present a soft data collection methodology to obtain soft data from available weather and streamflow data, while focusing on two variables: the runoff coefficient and the baseflow index. This methodology can be reproduced for any gauged catchment in Spain, and can be also used for any other region with similar available datasets. The upper part of the Tagus River basin has been used as study case, evaluating the two variables runoff coefficient and baseflow index in 19 subbasins of the Tagus River in different geological regions.

![](https://github.com/alejandrosgz/HydRoVars/blob/main/3_Output_data/Weather_outputs/plot_rwrs_dist.png)

![](https://github.com/alejandrosgz/HydRoVars/blob/main/4_Figures/gif_BFImax_parameter.gif)

In spite of the focus of this work is to collect hydrological variables, data and code to collect, analyse and represent weather (precipitation and temperature) data has been also included. This allow to work at cactment scale, but also analyze weather variables in any study area desired (introduced as a shapefile).

![](https://github.com/alejandrosgz/HydRoVars/blob/main/3_Output_data/Weather_outputs/plot_tmp_basin_1_anual.png)

### Structure

Following files can be found in the main directory of the repository:

* **README** (this file). It is recommended to read it to know how the repository has been structured and how this methodology can be applied in other regions.

* **Manuscript.qmd** Contains all the work and data presented in the manuscript. In this file, the relevant parts of the code can be found (which can be also be found in the scripts) and some examples have been introduced. The code to reproduce all the results of the presented manuscript can be found in the **2_Scripts** folder. This document can be used to generate different output formats of the document (**Manuscript.docx**, **Manuscript.html**), which are also located in the main repository directory. Note that the qmd document has been optimized to rendering to html.

* **Manuscript.bib** contains the references in BibTex format, generated with the software *Zotero*.

Other files are related to the repository functioning or to the output adjustments and can be ignored.

Within the repository, four directories can be found:

#### 1. Used_files directory

The data necessary to reproduce the work is located in the **Used_files** folder, which contains three folders within:

 * **Created_csv** Contains the csv files that should be created for making the scripts work. To apply this methodology in other regions, these script have to be reproduced as explained in the manuscript or in the corresponding script (Script 1, **script_1_CSV_files_preparation.R**). A vector layer with the weather data grid and a vector layer with the basin/s that will be assessed are necessary to create these files. A brief description of these files follows:
 
    + **1_basins_file.csv** This file contains basic data about the basin or subbasins that will be assessed. Concretely, the name, ID, and area of each basin, and the ID of the gauging station are the fields of this table (*Basin*, *Basin_ID*, *area*, *gauging_code*, respectively). Another field, (which has been named *region* in this case) can be used to group the assessed basins according to any characteristic. Created with Script 1.
    
    + **2_ids_stations_file.csv** Contains the ID, name and location of each weather grid point located within the buffer of each basin (*ID*, *NAME*, *LAT*, *LONG*, *ELEVATION*). The basins where they are located (both ID and name are also indicated, as some points are present in more than one contiguous subbasins). Note that in this case, precipitation and temperature points IDs were the same, but if not one file for each grid should be created and used. Created with Script 1.
    
    + **3_alpha_estimation.csv** This file contains for each basin the duration of the recession curve (*recess_days*) and the coefficient of determination  obtained with their regression (*det_coefs*), the data necessary to reproduce the peaks and recession curves (*peak*, *peak_range*, *recess_range*), and the groundwater recession constant and alpha obtained for the three peaks (*gw_rec_const*,*alpha_valu*). The name, ID and region of each basin is also included. Created with Script 3 (**script_3_alphas_calculation.R**).
    
    + **4_groundwater_results.csv** This file contains the data obtained during the baseflow index estimation. Concretely, for each basin, the alpha and BFImax parameters values used for the filter and the estimated baseflow index (columns *alpha*, *BFImax_used*, *BF_Rate*, respectively) are stored. Created with the Script 4 (**script_4_Groundwater_contribution_estimation.R**).
 
 * **Data** This directory contains two folders:
 
    + **weather_data** Contains two folders (one for each variable, *pcp_spain* and *tmp_spain*, respectively) which contain the files with the data for each point of the grid used in this work. Files for all the Spanish territory can be downloaded from (https://swat.tamu.edu/data/spain/). These files have one column with the daily value of the variable, being the name of the column the initial date of the time series (19510101).
    
    + **gauging_data** Contains the file **afliq.csv**, downloaded from (https://ceh.cedex.es/anuarioaforos/TAJO_csv.asp). This file contains daily streamflow data for all the gauging stations located within the Tagus River basin, and have four columns: the gauging station code (*indroea*), the date (*fecha*), the height of the water (*altura*, which has not been used), and the streamflow (*caudal*, in cubic meters per second).
    
 * **GIS** This directory contains all the vector and raster data used for this work. It have two folders:
 
    + **Shapefiles** Directory that includes the shp (and related) files that has been used for this work: the upper sector of the Tagus River basin (**modeled_basin**), the delineated subbasins (**basins_studied**), weather points of the entire AEMET grid (**weather_grid_UTM**), gauging stations (**gauging_stations**) and the permeability map(**permeabilit_map**). A csv file created from the permeability map and the basins studied files contains the proportion of each subbasin conformed by each lithology (**Basins_lithology.csv**).
 
    + **c_Delineation.zip** Compressed folder which contains three raster layers: the digital elevation model (**ClipMDTproj.tif**) used to create the drainage direction (**drainage_direction_Tagus.tif**) and accumulation rasters (**drainage_acumulation_Tagus.tif**), which were used to delineate the subbasins with the GRASS tools. This file was compressed to reduce its weight.

#### 2. Scripts directory

The code for reproducing all the results of the paper can be found in the folder **scripts**, where 4 scripts are located: 
 
 * **script_1_CSV_files_preparation.R** Can be used to create the csv files needed to use this methodology. As inputs, two vector files (delineated basins and gridded data) are needed.

 * **script_2_Runoff_rate calculation.R** Include all the code necessary to calculate the runoff coefficients at annual and average basis, for subbasins and geological regions. With this script, the annual precipitation, temperature, runoff and runoff coefficient can be calculated.
 
 * **script_3_alphas_calculation.R** Allow to calculate the *alpha* values using a linear regression. Three recession curves for each subbasin have been performed, and the length of the recession curve, the determination coefficient, the groundwater recession constant, and the alpha values with their standard deviation have been extracted.
 
 * **script_4_Groundwater_contribution_estimation.R** Include all the code necessary to apply the baseflow filter for three peaks in each subbasin and calculate the groundwater contribution to the streamflow. 
 
 * **script_5_Weather_data_exploration.R** This script can be ignored if weather data analysis is not in our scope. It allow to obtain, export and analyze weather data at different scales, and might be very useful to characterize the climate of a basin. This script not depends of the rest of the methodology, since the needed parts of *script 1* are also included.
 

#### 3. Output_data directory

In addition to the created files explained above, which are created during the workflow and are in some cases needed for the different process, this folder contains some output files with the most important generated data. Four files are automatically generated:

* **R1_Data_availability.csv** Information about the studied subbasins and the availability of observed data.

* **R2_Weather-runoff_data.csv** For each subbasin, contains some of the data generated with the script 2 (average precipitation, average temperature, and statistics of the runoff coefficient).

* **R3_Groundwater_recession_basin.csv** For each subbasin, contains some of the data generated with the script 3 (average recession curve duration, coefficient of determination, recession constant and alpha).

* **R4_Groundwater_contribution_basin.csv** Contains the same information that **4_groundwater_results.csv**, the results of the groundwater contrivution estimation.

An specific folder for weather data outputs (*Weather_outputs*) is included, which can be created using the **Script 6**. This folder contain weather data extracted for a certain period and figures created with this data.


#### 4. Figures directory

**4_Figures** directory contains the images used in the manuscript


### Instructions for the user

In the manuscript the instructions for reproducing this work can be found. In this section the main workflow will be explained in brief. 

**Open the R Project** will be necessary to access to the different files (*HydRoVars.Rproj*). Doing so, the main folder of the software will appear in the R Studio Files panel, and the different scripts can be easily opened from there.

As inputs for making this assessment in other regions, the user would need to prepare:

* A vector file with the basins to be assessed. Instructions for delineating subbasins can be found in the GRASS module documentation (r.watershed, r.water.outlet and r.to.vect tools). It is recommended to adjust the fields of this layer to control the order when calculating the variables, i.e., indicating the name and creating IDs if multiple basins will be assessed. The code of the gauging station for each subbasin have to be included in this vector file or in the csv that it is created from it using the **Script 1**.

* A vector file with the weather data points. In this case, a grid has been used, but this is not strictly necessary. Note that, if no grid is used, the average values of the points may not be accurately interpolated. If precipitation and temperature points/stations are not coincident, two files may be used. A csv file with the points located within a buffer for each assessed basin can be created using the **Script 1**.

If these files are prepared in the same way as in the example, the **Script 2** will work  automatically. In the example case, the subbasins have been grouped by geological regions, but any other characteristic or none can be used. Runoff coefficients will be generated at annual and average basis for the chosen period.

For reproducing the groundwater assessment, as it has been done manually, the user should follow the presented steps, but adapting the code for its streamflow data. This applies for **Scripts 3 and 4**. It is recommended to create csv files with the results obtained with these scripts, as it has been done in the example.

If the user only wants to perfrom a weather data anaylsis, only the *Script 6* is neccessary. This script allows to estimate for any polygon weather variables and produce plots. Using the weather data source used in this work (https://swat.tamu.edu/data/spain/), data for the entire Spanish territory (5x5 km resolution, daily scale, 1951-2019) can be used.

### License

The content of this repository is licensed under the [MIT License](https://github.com/alejandrosgz/Hydro_vars_collection/blob/main/LICENSE).

### Citation

**Note**: This project is currently work in progress and the manuscript is about to be submitted. Please check the citation at a later time for updates.

Sánchez Gómez A.: *HydRoVars: An R tool to collect hydrological variables*, GitHub Repository, https://github.com/alejandrosgz/HydRoVars, 2023.

```
@misc{alejandrosgz2023,
  author  = {Alejandro Sánchez Gómez},
  note    = {GitHub Repository},
  title   = {{HydRoVars: An R tool to collect hydrological variables}},
  year    = {2023},
  url     = {https://github.com/alejandrosgz/HydRoVars},
  version = {1.0},
}
```

### Contact

Alejandro Sánchez Gómez 

alejandro.sanchezgomez97@gmail.com
alejandro.sanchezg@uah.es







