

library(tidyverse); library(magrittr); library(readxl)
library(sf);   library(terra)
library(lidR); library(leafR);
library(ggplot2); library(MetBrewer)

sbe_folder="/Users/av23907/Dropbox/3_sbe/"


# Load SBE data ########
# plot polygons
plots_sbe=terra::vect(paste0(sbe_folder,"data/Plot_locations/SBE_plots_corrected_with_GPS_2025/SBE_plots_corrected_with_GPS_2025.shp")) %>% 
  terra::project("epsg:32650") %>% sf::st_as_sf()
plots_sbe$ID = seq(1,nrow(plots_sbe))

plots_dan=sf::st_read(paste0(sbe_folder,"data/Plot_locations/Danum/Dan_4ha_grid_clipped.gpkg")) 
plots_dan$area = sf::st_area(plots_dan) %>% units::drop_units()
plots_dan %<>% filter(area== 40000)
plots_dan$treatment="Danum"
plots_dan$name="Danum"
plots_dan$ID = seq(1,nrow(plots_dan))

# LiDAR data


# 1. CHM profiles #########
rasters_sbe = terra::rast(list.files(paste0(sbe_folder,"data/chms/sbe_chms/"),pattern="*.tiff",full.names = T))
rasters_dan = terra::rast(list.files(paste0(sbe_folder,"data/chms/dan_chms/"),pattern="*.tiff",full.names = T))


chm_2013 = terra::extract(rasters_sbe$chm_2013,sbe_plots) %>% left_join(sbe_plots,by="ID")
chm_2020 = terra::extract(rasters_sbe$chm_2020,sbe_plots) %>% left_join(sbe_plots,by="ID")

chm_2013_dt = chm_2013 %>% mutate(height_bin = round(chm_2013)) %>% 
  group_by(treatment,plot,height_bin) %>% summarize(n=n()) %>% mutate(year="2013")

chm_2020_dt = chm_2020 %>% mutate(height_bin = round(chm_2020)) %>% 
  group_by(treatment,plot,height_bin) %>% summarize(n=n()) %>% mutate(year="2020")

chm_combined_dt = full_join(chm_2013_dt,chm_2020_dt,by=c("treatment","plot","height_bin")) %>%
  mutate(n = n.y-n.x,year = "change") %>% select(treatment,height_bin,n) %>% mutate(year="change")

chms = bind_rows(chm_2013_dt,chm_2020_dt,chm_combined_dt)

# summarize across treatments
chms$name = NA
chms$name[chms$treatment =="0_spp"] = "Control"
chms$name[chms$treatment =="1_spp"] = "Planting"
chms$name[chms$treatment =="4_spp"] = "Planting"
chms$name[chms$treatment =="16_spp"] = "Planting"
chms$name[chms$treatment =="16_cut"] = "Liana removal"
chms$name  %<>% factor(levels = (c("Control","Planting","Liana removal" )))

readr::write_csv(chms, paste0(sbe_folder,file = "data/SBE_chm_profiles.csv"))

chm_mean = chms %>% group_by(name,height_bin,year) %>%
  summarize(n = mean(n,na.rm=T))
ggplot(chm_mean,aes(n,height_bin,color=name))+geom_point()+facet_wrap(~year)


# 2. PD profile  ####

pds_out = list()
plot_polygons = sbe_plots
site="SBE"
#"C:/Users/av23907/Dropbox/1_SBE_project/data/normalized_point_clounds/SBE/2013/SBE_nlaz_2013_7_1_spp.laz"
for (i in seq(1,nrow(plot_polygons))){
  print(paste(i, "of", nrow(plot_polygons)))
  this_plot = plot_polygons[i,]
  
  a = lidR::readLAS(paste0(sbe_folder,"data/normalized_plot_clouds/SBE/2013/SBE_nlaz_2013_",this_plot$plot,"_",this_plot$treatment,".laz"))
  b = leafR::pointsByZSlice(a$Z,100)
  c = as.data.frame(t(bind_rows(b)))
  c %<>% mutate(prop_points = 100*V1/sum(V1)) %>% select(prop_points)
  pd_2013 = c %>% mutate(year="2013", height_bin = seq(1,nrow(c)))
  
  
  a = lidR::readLAS(paste0(sbe_folder,"data/normalized_plot_clouds/SBE/2020/SBE_nlaz_2020_",this_plot$plot,"_",this_plot$treatment,".laz"))
  b = leafR::pointsByZSlice(a$Z,100)
  c = as.data.frame(t(bind_rows(b)))
  c %<>% mutate(prop_points = 100*V1/sum(V1)) %>% select(prop_points)
  pd_2020 = c %>% mutate(year="2020", height_bin = seq(1,nrow(c)))
  
  # Combine 2013 and 2020 & difference
  pds = full_join(pd_2013,pd_2020,by="height_bin")
  pds %<>% mutate(prop_points = prop_points.y-prop_points.x,year = "change")
  
  pds_combined = bind_rows(pd_2013,pd_2020,pds)
  #ggplot(pds_combined,aes(pd,height,color=year))+geom_point()
  
  # Combine plot info and save in list
  pds_out[[i]] = cbind(pds_combined,this_plot %>% sf::st_drop_geometry())
  
}
pds = pds_out %>% bind_rows()

readr::write_csv(pds, paste0(dropbox,file = "3_SBE_project/SBE_pd_profiles.csv"))
#pds = readr::read_csv(paste0(dropbox,file = "3_SBE_project/SBE_pd_profiles.csv"))

pds$name = NA
pds$name[pds$treatment =="0_spp"] = "Control"
pds$name[pds$treatment =="1_spp"] = "Planting"
pds$name[pds$treatment =="4_spp"] = "Planting"
pds$name[pds$treatment =="16_spp"] = "Planting"
pds$name[pds$treatment =="16_cut"] = "Liana removal"
#pds$name[pds$treatment =="Danum"] = "Danum"
pds$name  %<>% factor(levels = (c("Control","Planting","Liana removal" )))

ggplot(pds %>% dplyr::filter(year=="change"),aes(num_points, height_bin))+
  ggpointdensity::geom_pointdensity()+facet_wrap(~name)+geom_vline(xintercept=0,color="red")

pd_summary = pds %>% ungroup() %>% group_by(name,height_bin,year) %>%
  summarize(mean_prop_points = mean(prop_points,na.rm=T))



ggplot(pd_summary,aes(mean_prop_points, height_bin,color=name,group=name))+geom_point()+facet_wrap(~year)+geom_vline(xintercept=0,color="red")

ggsave(filename = paste0(dropbox,"Rscripts/SBE/SBE_figures/",
                         "4_point_density_profiles.png"),height = 3, width = 7)



# 3. LAI profiles ##########

# __SBE ######
site="SBE"
plot_polygons = sbe_plots
lads_out = list()
for (i in seq(1,nrow(plot_polygons))){
  print(paste(i, "of", nrow(plot_polygons)))
  this_plot = plot_polygons[i,]
  
  # Calculate LAD from voxelization

  norm_las_2013 = paste0(sbe_folder,"data/normalized_point_clouds/SBE/2013/",site,"_nlaz_2013_",this_plot$plot,"_",this_plot$treatment,".laz")
  norm_las_2020 = paste0(sbe_folder,"data/normalized_point_clouds/SBE/2020/",site,"_nlaz_2020_",this_plot$plot,"_",this_plot$treatment,".laz")
  
  voxels_2013 = leafR::lad.voxels(norm_las_2013, grain.size = 2)
  voxels_2020 = leafR::lad.voxels(norm_las_2020, grain.size = 2)
  
  #voxels_2013 = transmittance.voxels(norm_las_2013, grain.size = 5)
  #voxels_2020 = transmittance.voxels(norm_las_2020, grain.size = 5)
  
  # Calculate the LAD profile
  lad_2013 = leafR::lad.profile(voxels_2013) %>% mutate(year="2013")
  lad_2020 = leafR::lad.profile(voxels_2020) %>% mutate(year="2020")
  
  # Combine 2013 and 2020 & difference
  lads = full_join(lad_2013,lad_2020,by="height")
  lads %<>% mutate(lad = lad.y-lad.x,year = "change")
  
  lads_combined = bind_rows(lad_2013,lad_2020,lads %>% select(height,lad,year))
  #ggplot(lads_combined,aes(lad,height,color=year))+geom_point()
  
  # Combine plot info and save in list
  lads_out[[i]] = cbind(lads_combined,this_plot %>% sf::st_drop_geometry())
  
}

lads_save = lads_out %>% bind_rows()
lads_save$site=site

readr::write_csv(lads_save, paste0(sbe_folder,file = "data/SBE_LAI_profiles.csv"))

# __Danum DO THIS! ##########
lads_out = list()
for (i in seq(1,nrow(plots_dan))){
  print(paste(i, "of", nrow(plots_dan)))
  this_plot = plots_dan[i,]
  
  # Calculate LAD from voxelization
  norm_las_2013 = paste0(sbe_folder,"data/normalized_point_clouds/Danum/2013/Danum_nlaz_2013_",i,".laz")
  norm_las_2020 = paste0(sbe_folder,"data/normalized_point_clouds/Danum/2020/Danum_nlaz_2020_",i,".laz")
  
  voxels_2013 = leafR::lad.voxels(norm_las_2013, grain.size = 2)
  voxels_2020 = leafR::lad.voxels(norm_las_2020, grain.size = 2)
  
  #voxels_2013 = transmittance.voxels(norm_las_2013, grain.size = 5)
  #voxels_2020 = transmittance.voxels(norm_las_2020, grain.size = 5)
  
  # Calculate the LAD profile
  lad_2013 = leafR::lad.profile(voxels_2013) %>% mutate(year="2013")
  lad_2020 = leafR::lad.profile(voxels_2020) %>% mutate(year="2020")
  
  # Combine 2013 and 2020 & difference
  lads = full_join(lad_2013,lad_2020,by="height")
  lads %<>% mutate(lad = lad.y-lad.x,year = "change")
  
  lads_combined = bind_rows(lad_2013,lad_2020,lads %>% select(height,lad,year))
  #ggplot(lads_combined,aes(lad,height,color=year))+geom_point()
  
  # Combine plot info and save in list
  lads_out[[i]] = cbind(lads_combined,this_plot %>% sf::st_drop_geometry())
  
}
lads_save = lads_out %>% bind_rows()
lads_save$site="Danum"


readr::write_csv(lads_save, paste0(sbe_folder,file = "data/Danum_LAI_profiles.csv"))
#lai_dan = readr::read_csv(paste0(dropbox,file = "3_SBE_project/Dan_LAI_profiles.csv"))
#lai_dan$treatment="Danum"




# __3b Transmittance #####
trans = readr::read_csv(paste0(dropbox,file = "3_SBE_project/SBE_transmittance_profiles.csv"))

trans$name = NA
trans$name[trans$treatment =="0_spp"] = "Control"
trans$name[trans$treatment =="1_spp"] = "Planting"
trans$name[trans$treatment =="4_spp"] = "Planting"
trans$name[trans$treatment =="16_spp"] = "Planting"
trans$name[trans$treatment =="16_cut"] = "Liana removal"
#trans$name[trans$treatment =="Danum"] = "Danum"
trans$name  %<>% factor(levels = (c("Control","Planting","Liana removal" )))


ggplot(trans %>% filter(year =="change"),aes(lad,height,color=plot))+
  geom_point()+facet_wrap(~treatment)+ylim(c(0,70))+geom_vline(xintercept = 0,color="black")


th = trans %>% ungroup() %>% group_by(name,height,year) %>%
  summarize(mean_lad = mean(lad,na.rm=T))

ggplot(th ,aes(mean_lad,height,color=name))+geom_point()+
  facet_wrap(~year,scales="free_x")+ylim(c(0,70))+geom_vline(xintercept=0,color="black")+
  xlab("Transmittance")+ylab("Height bin (m)")


# Functions ########
# Define function - COLLAPSED!
clip_and_normalize_point_clouds = function(plot_polygons,site,ALS,dropbox){
  pc_2013 = lidR::readLAScatalog(list.files(paste0(ALS,site,"2013_2020/2013/"),pattern=".las",full.names=T)) 
  pc_2020 = lidR::readLAScatalog(list.files(paste0(ALS,site,"2013_2020/2020/"),pattern=".las",full.names=T))
  
  lads_out = list()
  for (i in seq(1,nrow(plot_polygons))){
    print(paste(i, "of", nrow(plot_polygons)))
    this_plot = plot_polygons[i,]
    
    # Clip point cloud to plot
    
    this_pc_2013 = pc_2013 %>% clip_roi(this_plot) %>% filter_duplicates()
    this_pc_2020 = pc_2020 %>% clip_roi(this_plot) %>% filter_duplicates()
    
    # Normalize point cloud
    this_pc_2013_normalized <- this_pc_2013 - rasterize_terrain(this_pc_2013, 1, knnidw())
    this_pc_2020_normalized <- this_pc_2020 - rasterize_terrain(this_pc_2020, 1, knnidw())
    
    # Save normalized point cloud
    lidR::writeLAS(this_pc_2013_normalized,paste0(dropbox,"9_Data/ALS/Malaysia_ALS/",site,"/2013/normalized_plot_clouds/",site,"_nlaz_2013_",this_plot$plot,"_",this_plot$treatment,".laz"))
    lidR::writeLAS(this_pc_2020_normalized,paste0(dropbox,"9_Data/ALS/Malaysia_ALS/",site,"/2020/normalized_plot_clouds/",site,"_nlaz_2020_",this_plot$plot,"_",this_plot$treatment,".laz"))
  }
  return(i)
}

#clip_and_normalize_point_clouds(plot_polygons=plots_sbe,site="SBE",ALS,dropbox)



transmittance.voxels = function(normlas.file, grain.size = 1, k = 1){
  
  #empty list object that will be fueling with binneds data.frames
  LAD_VOXELS = list()
  Z = NA
  
  #load normalized las cloud
  .las = lidR::readLAS(normlas.file)
  
  .las@data$Z[.las@data$Z < 0] = 0
  
  maxZ = floor(max(.las@data$Z))
  
  func = formula(paste0("~pointsByZSlice(Z, ", maxZ, ")"))
  t.binneds    = lidR::grid_metrics(.las, func, res = grain.size,
                                    start = c(min(.las@data$X), max(.las@data$Y)))
  t.binneds    = data.frame(sp::coordinates(t.binneds), raster::values(t.binneds))
  names(t.binneds)[1:2] = c("X", "Y")
  
  
  #getting the coordinates X and Y
  #t.binneds$X = coordinates(t.binneds)[,1]
  #t.binneds$Y = coordinates(t.binneds)[,2]
  #t.binneds = as.data.frame(t.binneds) #transforming in a data.frame
  
  #clip product by las files limits
  #t.binneds = t.binneds[t.binneds$X < xmax(.las) &
  #                        t.binneds$X > xmin(.las) &
  #                        t.binneds$Y > ymin(.las) &
  #                        t.binneds$Y < ymax(.las),]
  
  
  #select ground returns
  ground.returns = t.binneds[, grep("ground", names(t.binneds))]
  
  #select columns vegetation above 1m:
  if(nrow(t.binneds) != 1){ #this if is necessary when grain size is the whole plot
    pulses.profile.dz1 = t.binneds[, c(grep("pulses", names(t.binneds)))]
  }else{
    pulses.profile.dz1 = data.frame(matrix(as.numeric(as.character(t.binneds[, c(grep("pulses", names(t.binneds)))])), ncol = length(grep("pulses", names(t.binneds)))))
    names(pulses.profile.dz1) = names(t.binneds)[c(grep("pulses", names(t.binneds)))]
  }
  
  #invert data.frames for the sky be first
  pulses.profile.dz1 = pulses.profile.dz1[,length(pulses.profile.dz1):1] #invert columns
  
  #add grounds returns (0-1m)
  pulses.profile.dz1 = cbind(pulses.profile.dz1, ground.returns)
  rm(ground.returns)
  
  ### total matriz and cumsum.matrix:
  total.pulses.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, sum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1))
  cumsum.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, cumsum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1), byrow = TRUE)
  
  rm(pulses.profile.dz1)
  
  #Pulses out for each voxel
  pulse.out.dz1 = total.pulses.matrix.dz1 - cumsum.matrix.dz1
  
  #The pulses.out of voxel 1 is the pulses.in of voxel 2 and so on...
  #Therefore, pulse.in is pulse.out without the last line and adding in the
  #first line the total pulses:
  if(nrow(t.binneds) != 1){ #if used when grain size of the whole plot
    pulse.in.dz1 <- cbind(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
  }else{
    pulse.in.dz1 <- c(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
  } #enf if
  
  rm(total.pulses.matrix.dz1, cumsum.matrix.dz1)
  
  # MacArthur-Horn eqquation
  # LAD = ln(S_bottom/S_top)*(1/(dz*K))
  #k value for LAD equation
  dz = 1
  
  #LAD.dz1 = log(pulse.in.dz1/pulse.out.dz1) * 1/k * 1/dz
  LAD.dz1 = pulse.in.dz1/pulse.out.dz1
  
  rm(pulse.in.dz1, pulse.out.dz1)
  
  # Remove infinite and NaN values
  #Inf ocorre qndo pulses.out eh zero
  #NaN ocorre qndo pulses.in eh zero
  LAD.dz1[is.infinite(LAD.dz1)] <- NA; LAD.dz1[is.nan(LAD.dz1)] <- NA;
  
  #remove the first 1 meter close to the ground (and the ground too)
  LAD.dz1 = LAD.dz1[, -c(ncol(LAD.dz1))]
  
  #fuel list object
  LAD_VOXELS[["LAD"]] = LAD.dz1
  LAD_VOXELS[["coordenates"]] = t.binneds[,c("X", "Y")]
  
  rm(LAD.dz1, t.binneds)
  
  return(LAD_VOXELS)
}#End function
