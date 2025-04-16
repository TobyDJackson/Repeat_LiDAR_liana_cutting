
# Packages and paths =========
library(tidyverse); library(magrittr); library(ggplot2); 
library(readxl)
library(sf);   library(terra); library(tidyterra)
library(ggplot2); library(MetBrewer); library(patchwork); 
library(marginaleffects)

# Load rasters & polygons ########
sbe_folder="/Users/av23907/dropbox/3_sbe/"
rasters_sbe = terra::rast(list.files(paste0(sbe_folder,"data/chms/sbe_chms/"),pattern="*.tiff",full.names = T))
rasters_dan = terra::rast(list.files(paste0(sbe_folder,"data/chms/dan_chms/"),pattern="*.tiff",full.names = T))
plots_sbe=terra::vect(paste0(sbe_folder,"data/Plot_locations/SBE_plots_corrected_with_GPS_2025/SBE_plots_corrected_with_GPS_2025.shp")) %>% 
  terra::project("epsg:32650") %>% sf::st_as_sf()
plots_dan=sf::st_read(paste0(sbe_folder,"data/Plot_locations/Danum/Dan_4ha_grid_clipped.shp")) 

pd13 = terra::rast(paste0(sbe_folder,"data/chms/sbe_chms/pulsedensity_2013.tif")) 
pd20 = terra::rast(paste0(sbe_folder,"data/chms/sbe_chms/pulsedensity_2020.tif"))
names(pd13) = "pd"
names(pd20) = "pd"
d13 = pd13 %>% as.data.frame() %>% summarize(mean_pd= mean(pd, na.rm=T))
d20 = pd20 %>% as.data.frame() %>% summarize(mean_pd= mean(pd, na.rm=T))

d13# Extract & join ########
dt_sbe = terra::extract(rasters_sbe,plots_sbe)
dt_dan = terra::extract(rasters_dan,plots_dan)
dt_sbe = left_join(plots_sbe %>% st_drop_geometry(),dt_sbe,by="ID")
dt_dan = left_join(plots_dan %>% st_drop_geometry(),dt_dan,by="ID")
dt_both = dplyr::bind_rows(dt_sbe,dt_dan)


# Summarize by plot ##########
dt_2013 = dt_both %>% group_by(ID,  block, name, name_ha, treatment) %>% 
    summarize(n_pixels = n(),
              canopy_cover = sum(chm_2013>10)/n_pixels,
              canopy_height = mean(chm_2013,na.rm=T), 
              max_height = quantile(chm_2013,0.99,na.rm=T), 
              elevation = mean(dtm,na.rm=T))
dt_2013$year = "2013"
  
dt_2020 = dt_both %>% group_by(ID,  block,name, name_ha, treatment) %>% 
    summarize(n_pixels = n(),
              canopy_cover = sum(chm_2020>10)/n_pixels,
              canopy_height = mean(chm_2020,na.rm=T), 
              max_height = quantile(chm_2020,0.99,na.rm=T), 
              elevation = mean(dtm,na.rm=T))
dt_2020$year = "2020"
  
dt_change = dt_2020
dt_change$canopy_cover = dt_2020$canopy_cover - dt_2013$canopy_cover
dt_change$canopy_height = dt_2020$canopy_height - dt_2013$canopy_height
dt_change$max_height = dt_2020$max_height - dt_2013$max_height
dt_change$year = "change"
dt = bind_rows(dt_2013,dt_2020,dt_change)
  
#dt_2020 = dt %>% dplyr::filter(year==2020, name !="Danum")
readr::write_csv(dt, file = paste0(sbe_folder,"data/SBE_data_canopy_height_and_elevation.csv"))

dt$name_ha  %<>% factor(levels = (c("Logged control (48 ha)","Enrichment planting (384 ha)",
                                  "Liana cutting (64 ha)", "Primary forest (180 ha)" )))

# Load LAI data ##########
sbe_lads = readr::read_csv(paste0(sbe_folder,file = "data/SBE_LAI_profiles.csv"))
dan_lads = readr::read_csv(paste0(sbe_folder,file = "data/Dan_LAI_profiles.csv"))
dan_lads$plot=dan_lads$ID
plot_lads = bind_rows(sbe_lads,dan_lads)

plot_lads$name_ha = NA
plot_lads$name_ha[plot_lads$treatment =="0_spp"] = "Logged control (48 ha)"
plot_lads$name_ha[plot_lads$treatment =="1_spp"] = "Enrichment planting (384 ha)"
plot_lads$name_ha[plot_lads$treatment =="4_spp"] = "Enrichment planting (384 ha)"
plot_lads$name_ha[plot_lads$treatment =="16_spp"] = "Enrichment planting (384 ha)"
plot_lads$name_ha[plot_lads$treatment =="16_cut"] = "Liana cutting (64 ha)"
plot_lads$name_ha[plot_lads$treatment =="Danum"] = "Primary forest (180 ha)"
plot_lads$name_ha  %<>% factor(levels = (c("Logged control (48 ha)","Enrichment planting (384 ha)",
                                        "Liana cutting (64 ha)", "Primary forest (180 ha)" )))


plot_total_lai = plot_lads %>% group_by(year,treatment,name_ha,plot) %>% 
  summarize(total_lai = sum(lad))

plot_total_lai_2013 = plot_total_lai %>% dplyr::filter(year==2013)
plot_total_lai_2020 = plot_total_lai %>% dplyr::filter(year==2020)
plot_total_lai_change = plot_total_lai_2020
plot_total_lai_change$total_lai = plot_total_lai_2020$total_lai - plot_total_lai_2013$total_lai


plot_lai_means = plot_total_lai %>% ungroup() %>% group_by(name_ha, year) %>%
  summarize(lai_mean = (mean(total_lai,na.rm=T)), 
            lai_sd = (sd(total_lai,na.rm=T))) %>% filter(year == 2020)


# __ Table of means #####

dt_means = dt %>% ungroup() %>% group_by(name_ha,name,  year) %>%
  summarize(n = n(),
            canopy_height_mean = mean(canopy_height), 
            canopy_height_sd = sd(canopy_height,na.rm=T)) %>%
  mutate(canopy_height_se = canopy_height_sd/sqrt(n))

dt_means = dt %>% ungroup() %>% group_by(name_ha,name,  year) %>%
  summarize(canopy_cover_mean = round(100*mean(canopy_cover,na.rm=T)), 
            canopy_cover_sd = round(100*sd(canopy_cover,na.rm=T))) %>% filter(year == 2020)


# __ Fig 1. Boxplots height +  ############

(p1 = ggplot(dt %>% dplyr::filter(year ==2013), aes(canopy_height,name_ha,color=name_ha,fill=name_ha))+theme_bw()+
    scale_x_continuous(breaks=c(20,30,40),limits=c(20,40))+xlab("Canopy height (m)")+
   ggtitle("2013"))

(p2 = ggplot(dt %>% dplyr::filter(year ==2020), aes(canopy_height,name_ha,color=name_ha,fill=name_ha))+theme_bw()+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(20,30,40),limits=c(20,40))+xlab("Canopy height (m)")+
  ggtitle("2020"))

(p3 = ggplot(dt %>% dplyr::filter(year =="change"), aes(canopy_height,name_ha,color=name_ha,fill=name_ha))+
    geom_vline(xintercept=0,color="black")+ theme_bw() +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(-3,0,3),limits=c(-5,5))+xlab("Height change (m)")+
  ggtitle("Change (total)"))


# Canopy cover
(p4 = ggplot(dt %>% dplyr::filter(year ==2013), aes(100*canopy_cover,name_ha,color=name_ha,fill=name_ha))+theme_bw()+
    scale_x_continuous(breaks=c(50,75,100),limits=c(50,100))+xlab("Canopy cover (%)"))

(p5 = ggplot(dt %>% dplyr::filter(year ==2020), aes(100*canopy_cover,name_ha,color=name_ha,fill=name_ha))+theme_bw()+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(50,75,100),limits=c(50,100))+xlab("Canopy cover (%)"))

(p6 = ggplot(dt %>% dplyr::filter(year =="change"), aes(100*canopy_cover,name_ha,color=name_ha,fill=name_ha))+
    geom_vline(xintercept=0,color="black")+ theme_bw() +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(-10,0,10),limits=c(-10,10))+xlab("Cover change (%)"))


# Leaf area
(p7 = ggplot(plot_total_lai_2013, aes(total_lai,name_ha,color=name_ha,fill=name_ha))+ theme_bw()+
    scale_x_continuous(breaks=c(3,4,5),limits=c(3,5))+xlab("Leaf area index"))


(p8 = ggplot(plot_total_lai_2020, aes(total_lai,name_ha,color=name_ha,fill=name_ha))+theme_bw()+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(3,4,5),limits=c(3,5))+xlab("Leaf area index"))

(p9 = ggplot(plot_total_lai_change, aes(total_lai,name_ha,color=name_ha,fill=name_ha))+
    geom_vline(xintercept=0,color="black")+ 
    theme_bw() +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    scale_x_continuous(breaks=c(-1,0,1),limits=c(-1,1))+xlab("LAI change"))

pa = (p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9) + plot_layout(guides ="collect") & 
  geom_boxplot(alpha=0.4, outlier.size = 0) & geom_jitter(height=0.2,size=0.3,alpha=1) & ylab("") &
  theme(legend.position = "none",plot.title = element_text(hjust=0.5,face="bold"),plot.margin = unit(c(3,3,3,3),"mm"),
         panel.grid.minor = element_blank(),panel.grid.major = element_blank()) & 
  scale_fill_manual(values= met.brewer("Derain", 7)[c(7,4,1,3)]) &
  scale_color_manual(values= met.brewer("Derain", 7)[c(7,4,1,3)])

pa


ggsave(pa, filename = paste0(sbe_folder,"SBE_figures/",
                             "1_mean_height_canopy_cover_test.png"),height = 6, width = 7)


# __ Models #####
names(dt)
dt$name  %<>% factor(levels = (c("Logged control","Enrichment planting", "Liana cutting", "Primary forest")))
lm_2013   = stats::lm(100*canopy_cover~elevation+name+block, data=dt %>% dplyr::filter(year=="2013", name != "Primary forest"))
lm_2020   = lm(100*canopy_cover~elevation+name+block, data=dt %>% dplyr::filter(year=="2020", name != "Primary forest"))
lm_change = lm(100*canopy_cover~elevation+name+block, data=dt %>% dplyr::filter(year=="change", name != "Primary forest"))

plot(lm_change)
summary(lm_change)



clipr::write_clip(rbind(tidy(lm_2013),tidy(lm_2020),tidy(lm_change)))






# __ Fig 3. Main text rasters CHM_diffs ########

sbe_liana   = plots_sbe %>% dplyr::filter(treatment=="16_cut") 
sbe_16spp   = plots_sbe %>% dplyr::filter(treatment=="16_spp") 
sbe_control = plots_sbe %>% dplyr::filter(treatment=="0_spp")  

this_danum   = terra::crop(rasters_dan,terra::ext(587157,  587357, 548309, 548509))
this_liana   = terra::crop(rasters_sbe,terra::ext(571693,  571893, 561440, 561640))
this_16spp   = terra::crop(rasters_sbe,terra::ext(573541,  573741, 562355, 562555))
this_control = terra::crop(rasters_sbe,terra::ext(572300,  572500, 562100, 562300))


p_danum1 = ggplot() + geom_spatraster(data = this_danum$chm_2013)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")+
  ggtitle("2013")+ylab("Primary \nforest")+theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))

p_danum2 = ggplot() + geom_spatraster(data = this_danum$chm_2020)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")+
  ggtitle("2020")

p_danum3 =  ggplot() + geom_spatraster(data = this_danum$chm_diff)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps2(name="Height change (m)",low = "red", mid="white",high="darkblue",midpoint=0 , limits = c(-12,12),n.breaks=6,na.value = "black")+
  ggtitle("Height change")

p_liana1 = ggplot() + geom_spatraster(data = this_liana$chm_2013)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")+
  ylab("Liana \ncutting")+theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))

p_liana2 = ggplot() + geom_spatraster(data = this_liana$chm_2020)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")

p_liana3 =  ggplot() + geom_spatraster(data = this_liana$chm_diff)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps2(name="Height change (m)",low = "red", mid="white",high="darkblue",midpoint=0 , limits = c(-12,12),n.breaks=6,na.value = "black")

p_16spp1 = ggplot() + geom_spatraster(data = this_16spp$chm_2013)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")+
  ylab("Enrichment \nplanting")+theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))

p_16spp2 = ggplot() + geom_spatraster(data = this_16spp$chm_2020)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")

p_16spp3 = ggplot() + geom_spatraster(data = this_16spp$chm_diff)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps2(name="Height change (m)",low = "red", mid="white",high="darkblue",midpoint=0 , limits = c(-12,12),n.breaks=6,na.value = "black")

p_control1 = ggplot() + geom_spatraster(data = this_control$chm_2013)+coord_sf(crs = 32650) +theme_void()+
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")+
  ylab("Logged \ncontrol")+theme(axis.title.y =  element_text(angle = 90, vjust = 0.5, hjust=0.5))

p_control2 = ggplot() + geom_spatraster(data = this_control$chm_2020)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps(name="Canopy height (m)",low = "black", high="white", limits = c(10,60),n.breaks=5,na.value = "black")

p_control3 = ggplot() + geom_spatraster(data = this_control$chm_diff)+theme_void()+coord_sf(crs = 32650) +
  scale_fill_steps2(name="Height change (m)",low = "red", mid="white",high="darkblue",midpoint=0 , limits = c(-12,12),n.breaks=6,na.value = "black")

p_control3
a = (p_danum1+p_danum2+p_danum3)/
  (p_liana1+p_liana2+p_liana3)/
  (p_16spp1+p_16spp2+p_16spp3)/
  (p_control1+p_control2+p_control3)+
  plot_layout(guides = "collect") & #plot_annotation(tag_levels = "a") & 
  theme(legend.position = "bottom",plot.margin = unit(c(2,2,2,2),"mm")) & 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) &
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

a

ggsave( plot=a, filename = paste0(sbe_folder,"SBE_figures/",
                                  "CHMs_main_text.png"),height = 7, width = 5)






# __ S5-7 Additional rasters CHM_diffs ########


sbe_control = plots_sbe %>% dplyr::filter(treatment=="0_spp")
p_control = list()
for (i in seq(1,12)){
  this_control = terra::crop(rasters_sbe,sbe_control[i,])
  
  p_control[[i]] = 
    ggplot() + geom_spatraster(data = this_control$chm_diff)+theme_void()+
    scale_fill_steps2(name="m",low = "red", mid="lightblue",high="darkblue",midpoint=0 , limits = c(-10,10),n.breaks=20,na.value = "black")+
    theme(legend.position = "none")
}

sbe_16spp = plots_sbe %>% dplyr::filter(treatment=="16_spp")
p_16spp = list()
for (i in seq(1,16)){
  this_16spp = terra::crop(rasters_sbe,sbe_16spp[i,])

  p_16spp[[i]] = 
    ggplot() + geom_spatraster(data = this_16spp$chm_diff)+theme_void()+
    scale_fill_steps2(name="m",low = "red", mid="lightblue",high="darkblue",midpoint=0 , limits = c(-10,10),n.breaks=20,na.value = "black")+
    theme(legend.position = "none")
}


p_danum = list()
for (i in seq(1,16)){
    this_danum   = terra::crop(rasters_dan,plots_dan[i,])
  
  p_danum[[i]] = 
    ggplot() + geom_spatraster(data = this_danum$chm_diff)+theme_void()+
    scale_fill_steps2(name="m",low = "red", mid="lightblue",high="darkblue",midpoint=0 , limits = c(-10,10),n.breaks=20,na.value = "black")+
    theme(legend.position = "none")
}


sbe_liana_removal = plots_sbe %>% dplyr::filter(treatment=="16_cut")
p_liana_removal = list()
for (i in seq(1,16)){
  this_liana_removal = terra::crop(rasters_sbe,sbe_liana_removal[i,])
 
  p_liana_removal[[i]] = ggplot() + geom_spatraster(data = this_liana_removal$chm_diff)+theme_void()+
    scale_fill_steps2(name="m",low = "red", mid="lightblue",high="darkblue",midpoint=0 , limits = c(-10,10),n.breaks=20,na.value = "black")+
    theme(legend.position = "none")
}



a = patchwork::wrap_plots(p_liana_removal, nrow = 4,ncol=4)+plot_annotation(title="Liana cutting") & theme(plot.title = element_text(hjust = 0.5))
b = patchwork::wrap_plots(p_16spp, nrow = 4,ncol=4)+plot_annotation(title="Enrichment planting (16 species)") & theme(plot.title = element_text(hjust = 0.5))
c = patchwork::wrap_plots(p_control, nrow = 4,ncol=3)+plot_annotation(title="Logged control") & theme(plot.title = element_text(hjust = 0.5))
d = patchwork::wrap_plots(p_danum, nrow = 4,ncol=4)+plot_annotation(title="Primary forest (Danum)") & theme(plot.title = element_text(hjust = 0.5))


ggsave( plot=a, filename = paste0(sbe_folder,"SBE_figures/",
                          "CHM_diff_liana_removal_north.png"),height = 6, width = 5)

ggsave( plot=b, filename = paste0(sbe_folder,"SBE_figures/",
                          "CHM_diff_16_spp.png"),height = 6, width = 5)

 ggsave( plot=c, filename = paste0(sbe_folder,"SBE_figures/",
                          "CHM_diff_control.png"),height = 6, width = 4)

ggsave( plot=d, filename = paste0(sbe_folder,"SBE_figures/",
                          "CHM_diff_danum.png"),height = 6, width = 5)

#a + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top",plot.margin=margin(0.5,0.5,0.5,0.5))


ggplot() + geom_spatraster(data = this_liana_removal$chm_diff)+
  scale_fill_steps2(name="",low = "red", mid="lightblue",high="darkblue",midpoint=0 , limits = c(-20,20),n.breaks=7,na.value = "black")+
  theme(legend.position = "right")



