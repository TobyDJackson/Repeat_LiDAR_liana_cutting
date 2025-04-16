# This script processes canopy height rasters to give disturbance and recovery dynamics
# Written by Toby Jackson for the paper: 
# 'Tall Bornean forests experience higher disturbance rates than eastern Amazonia'

# Adapted September 2024 for the Sabah Biodiversity Experiment project

# FUNCTIONS ##########

CD_tile_class_summary = function(tile_stack){
  
  
  # This function summarizes the change in each gap and disturbance based on the rasters only 
  # It relies on the order of cells in the raster, and dplyr summarize. this is much faster than polygonizing 
  tile_tb = tile_stack %>% as.data.frame(xy=TRUE)
  
  # Define classess
  # 0 = background. 1 = Recovering. 2 = Disturbance. 
  # This is a heirarchy. The higher numbers overwrite the others
  tile_tb$class_num = 0 #"Intact canopy"
  tile_tb$class_num[tile_tb$a_gaps>=1]=1 # Recovered gap
  tile_tb$class_num[tile_tb$disturbances>=1]=2 # Disturbance (not resulting in a gap)

  
  #summary(as.factor(tile_tb$class_num))
  
  tile_tb$class=NA
  tile_tb$class[tile_tb$class_num==0] = "Intact canopy"
  tile_tb$class[tile_tb$class_num==1] = "Recovering gaps"
  tile_tb$class[tile_tb$class_num==2] = "Canopy disturbances"

  # Pixel summary
  tile_summary_tb = tile_tb %>% dplyr::ungroup() %>% dplyr::group_by(class) %>%
    dplyr::summarize(area = n(), 
                     CHM1_mean = mean(a_chm,na.rm=TRUE), 
                     height_increase_mean = mean(height_increase,na.rm=TRUE))

  tile_summary_tb$num_events = NA
  tile_summary_tb$num_events[tile_summary_tb$class == "Canopy disturbances"] = length(unique(tile_tb$disturbances))
  tile_summary_tb$num_events[tile_summary_tb$class == "Recovering gaps"] = length(unique(tile_tb$a_gaps))
  return(tile_summary_tb)
}


getForestGaps_terra <- function(chm_layer, threshold = 10, size = c(1, 10^4)) {
  chm_layer[chm_layer > threshold] <- NA
  chm_layer[chm_layer <= threshold] <- 1
  gaps <- terra::patches(chm_layer, directions = 8, allowGaps = FALSE)
  rcl <- terra::freq(gaps)
  rcl$layer<-NULL
  rcl[, 2] <- rcl[, 2] * terra::res(chm_layer)[1]^2
  z <- terra::classify(gaps, rcl = rcl, right = FALSE)
  z[is.na(gaps)] <- NA
  gaps[z > size[2]] <- NA
  gaps[z < size[1]] <- NA
  gaps <- terra::patches(gaps, directions = 8, allowGaps = FALSE)
  names(gaps) <- "gaps"
  return(gaps)
}


  
# Packages and paths =========
library(tidyverse); library(magrittr)
library(terra)

# Set parameters #######
tilesize = 4 # ha
min_area_threshold = 25
gap_height_threshold = 10; 
disturbance_height_threshold = 5
#site = "Danum"

# Load rasters & polygons ######
sbe_folder="/Users/av23907/dropbox/3_sbe/"
#whole_stack = terra::rast(list.files(paste0(sbe_folder,"data/chms/sbe_chms/"),pattern="*.tiff",full.names = T))
whole_stack = terra::rast(list.files(paste0(sbe_folder,"data/chms/dan_chms/"),pattern="*.tiff",full.names = T))

names(whole_stack) = c("a_chm","b_chm", "height_increase","dtm")

plots_sbe=terra::vect(paste0(sbe_folder,"data/Plot_locations/SBE_plots_corrected_with_GPS_2025/SBE_plots_corrected_with_GPS_2025.shp")) %>% 
  terra::project("epsg:32650") %>% sf::st_as_sf()

plots_dan=sf::st_read(paste0(sbe_folder,"data/Plot_locations/Danum/Dan_4ha_grid_clipped.shp")) 




# Loop over plots ####
tile_polygons = plots_dan

temp_list = list(); c=1
for (tile_id in seq(1,nrow(tile_polygons))){
  
    # Crop rasters to the tile
    tile_stack = whole_stack %>% terra::crop(tile_polygons[tile_id,]) %>% terra::mask(tile_polygons[tile_id,])
    #terra::plot(tile_stack)
    
    # Get values to filter out tiles with lots of NAs or no trees
    tile_a_chm_values  = terra::values(tile_stack$a_chm)
    tile_mean_height_a = mean(tile_a_chm_values,na.rm=T)
    tile_dtm_values    = terra::values(tile_stack$dtm)
    notNA = sum(is.na(tile_a_chm_values)==F)
    (percentage_notNA = 100*notNA/(tile_polygons$tile_area[tile_id]))
    
    if (is.na(tile_mean_height_a)==0){
      if (percentage_notNA>90){
        
        #  __ Find gaps & disturbance #######
        tile_stack$a_gaps = tile_stack$a_chm %>% getForestGaps_terra(gap_height_threshold, size=c(min_area_threshold,1e6))
        tile_stack$b_gaps = tile_stack$b_chm %>% getForestGaps_terra(gap_height_threshold, size=c(min_area_threshold,1e6)) 
        tile_stack$disturbances = tile_stack$height_increase %>% getForestGaps_terra(-disturbance_height_threshold , size=c(min_area_threshold,1e6)) 
        #plot(tile_stack$disturbances)
        
       
        # Log all parameters used
        
        log = data.frame(tile_id , treatment = tile_polygons$treatment[tile_id], 
                         name = tile_polygons$name[tile_id], name_ha = tile_polygons$name_ha[tile_id],
                         n_tiles = nrow(tile_polygons),  percentage_notNA, tile_area = tile_polygons$tile_area[tile_id],
                         tile_mean_height_a = mean(tile_a_chm_values,na.rm=T),
                         tile_max_height_a = max(tile_a_chm_values,na.rm=T),
                         tile_mean_height_increase = mean(terra::values(tile_stack$height_increase),na.rm=T),
                         tile_elevation_mean = mean(tile_dtm_values,na.rm=T))  
                    
        # __ Run analysis  #####
        temp_results  = CD_tile_class_summary(tile_stack)
        temp_list[[c]] = cbind(log,temp_results) 
        c=c+1
        
      } # End if notNA statmenet
    } #  End of if mean height != na statement 
    #log = data.frame(datetime_processed = Sys.time(),site = site, tilesize = tilesize, raster_algorithm = raster_algorithm, tile_id = tile_id, height_threshold_type = height_threshold_type)
      print(log)
} # End tile loop  


combined_results = dplyr::bind_rows(temp_list)


# SAVE THE RESULTS ##########
readr::write_csv(combined_results,file=paste0(sbe_folder,"data/temp_data/Dan_disturbance.csv"))






