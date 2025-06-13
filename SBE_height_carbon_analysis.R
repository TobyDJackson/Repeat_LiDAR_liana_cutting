
library(tidyverse); library(magrittr);  library(readxl)
library(ggplot2);   library(MetBrewer); library(stats)
library(terra); library(sf)
library(marginaleffects); library(lme4)

# 1. Load rasters & extract values ########
sbe_folder="/Users/av23907/dropbox/3_sbe/"
rasters_sbe = terra::rast(list.files(paste0(sbe_folder,"data/chms/sbe_chms/"),pattern="*.tiff",full.names = T))
plots_sbe=terra::vect(paste0(sbe_folder,"data/Plot_locations/SBE_plots_corrected_with_GPS_2025/SBE_plots_corrected_with_GPS_2025.shp")) %>% 
  terra::project("epsg:32650") %>% sf::st_as_sf()

# Extract values from rasters
dt_sbe = terra::extract(rasters_sbe,plots_sbe)
dt_sbe %<>% right_join(plots_sbe %>% sf::st_drop_geometry(),by="ID")

# Summarize for each 4 ha plot
dt = dt_sbe %>% group_by(ID,  block,name, name_ha, treatment) %>% 
  summarize(TCH = mean(chm_2020,na.rm=T), 
            elevation = mean(dtm,na.rm=T))

# Fix names and levels
dt$name[dt$name=="Logged control"] = "Control"

dt$name  %<>% factor(levels = (c("Control","Enrichment planting",
                                 "Liana cutting" )))

# Manually add intervals
dt$interval = 18 # This is for the enrichment planting
dt$interval[dt$block =="North" & dt$treatment =="16_cut" ] = 6
dt$interval[dt$block =="South" & dt$treatment =="16_cut" ] = 9


# 2.  Apply topography correction #####
dt$elevation_original = dt$elevation  
dt$TCH_original = dt$TCH
lm_elevation  = lm(TCH ~ elevation+name+block, data = dt)
summary(lm_elevation)

# replace with mean elevation
dt$elevation = mean(dt$elevation, na.rm=T)

# replace with corrected TCH
dt$TCH = predict(lm_elevation)

dt$fit.lm = stats::predict(lm_elevation)

# Plot model effect with topography
dt %>% 
  ggplot(aes(elevation_original, TCH_original,color=name))+theme_bw()+
  geom_point() + #facet_wrap(~block)+
  geom_line(aes(y=fit.lm),linewidth=1)+
  theme(legend.position="top", legend.title = element_blank())+
  ylab("2020 mean canopy height (m)")+xlab("Elevation (m)")+
  scale_color_manual(values= c("black", met.brewer("Derain", 7)[c(4,1)])) 

ggsave( paste0(sbe_folder,file = "SBE_figures/height_elevation_model.png"),
        height=4,width=4)



# 3. Build carbon model ####
# Load data from Jucker et al. (2018). https://doi.org/10.5194/bg-15-3811-2018 

dt_jucker  = readr::read_csv(paste0(sbe_folder,"data/carbon_model/Sabah_ACD_data.csv"))
ggplot(dt_jucker,aes(TCH, ACD))+geom_point()+geom_smooth(method="lm")+theme_bw()+
  xlab("Mean canopy height (m)")+ylab("Aboveground carbon density (tons per ha)")+
  ggtitle("Sabah carbon model (Jucker et al 2018)") #+#ggpmisc::stat_poly_eq(use_label(c("eq", "R2")))
#scale_x_log10()+scale_y_log10()
#ggsave( paste0(sbe_folder,file = "SBE_figures/S11_Carbon_model.png"),
#       height=4,width=4)

dt_jucker %<>% mutate(log_ACD = log(ACD), 
                      log_TCH = log(TCH))

# Test both a linear and logged model - the results are very similar


lm_carbon_linear = lm(ACD ~ TCH-1, data=dt_jucker) 
summary(lm_carbon_linear)
lm_carbon_logged = lm(log_ACD ~ log_TCH, data=dt_jucker) 

#plot(lm_carbon_logged)

# __ Apply carbon model ####
dt %<>% mutate(log_TCH = log(TCH))

dt$ACD_linear = predict(lm_carbon_linear, dt, se.fit=T)$fit

dt$log_ACD = predict(lm_carbon_logged, dt, se.fit=T)$fit
dt %<>% mutate(ACD_powerlaw = exp(log_ACD))


# 4. Calculate additional height above control plots #######

dt_control =  dt %>% dplyr::filter(treatment=="0_spp")
dt_planting = dt %>% dplyr::filter(treatment !="0_spp", treatment != "16_cut")
dt_liana =    dt %>% dplyr::filter(treatment == "16_cut")

dt_control_mean = dt_control %>% group_by(block) %>%
  summarize(control_TCH = mean(TCH, na.rm=T), 
            control_ACD = mean(ACD_powerlaw, na.rm=T))

# The enrichment planting is the 'control' for the liana cutting
dt_planting_mean = dt_planting %>% group_by(block) %>%
  summarize(control_TCH = mean(TCH, na.rm=T), 
            control_ACD = mean(ACD_powerlaw, na.rm=T))

dt_planting_norm = dt_planting %>% dplyr::left_join(dt_control_mean, by="block")
dt_liana_norm = dt_liana %>% dplyr::left_join(dt_planting_mean, by="block")
dt_norm = rbind(dt_planting_norm, dt_liana_norm)


dt_norm %<>% mutate(height_above_control = TCH - control_TCH, 
                    height_above_control_py = height_above_control/interval,
               additional_carbon = ACD_powerlaw - control_ACD, 
               additional_carbon_py = additional_carbon/interval)


# 5. Carbon boxplot #######
dt_norm$name  %<>% factor(levels = (c("Liana cutting", "Enrichment planting" )))

ggplot(dt_norm , aes(100*height_above_control_py, name,color=name))+theme_bw()+
  geom_boxplot(alpha=0.4, outlier.size = 0) + geom_jitter(height=0.2,size=1,alpha=0.5)+
  #theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(legend.position="none")+ylab("")+
  scale_x_continuous(sec.axis = sec_axis(~.*0.052, 
                                         name = expression(~ Additional ~ carbon ~ "("~ MgC ~ ha^-1 ~ yr^-1 ~")"))) +
  xlab(expression(~ Additional ~ canopy ~ growth ~ "(" ~ cm ~ yr^-1 ~")"))+
  #geom_vline(xintercept = 0, color="black") +
  theme(legend.position = "none",plot.title = element_text(hjust=0.5,face="bold"),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank()) + 
  scale_fill_manual(values= met.brewer("Derain", 7)[c(1,4)]) +
  scale_color_manual(values= met.brewer("Derain", 7)[c(1,4)]) 

ggsave( paste0(sbe_folder,file = "SBE_figures/1_carbon_single_boxplot_horizontal.png"),
        height=2.5,width=3.5)

# 6. Model results #####
dt$name  %<>% factor(levels = (c("Enrichment planting","Control",
                                 "Liana cutting" )))
lm1 = lm(TCH_original ~elevation_original+name+block, data=dt)
summary(lm1)


dt_norm_summary = dt_norm %>% group_by(name) %>%
  summarize(additional_carbon_py_mean = mean(additional_carbon_py), 
            additional_carbon_py_se = sd(additional_carbon_py)/sqrt(n()))

dt_norm$name  %<>% factor(levels = (c("Enrichment planting", "Liana cutting" )))


# 7. Cost of carbon to 2050 ######
# These results are in row F of Table 1.

calculate_cost_to_2050 = function(cost_per_ha, carbon_per_ha_per_year ){
  # Written in 2025, so there are 25 years to 2050
  cost_to_2050 = cost_per_ha/(25*carbon_per_ha_per_year)
}

# These numbers were added manually from the table.

# Enrichment planting

# Average cost 
(calculate_cost_to_2050(cost_per_ha = 2000, carbon_per_ha_per_year = 0.38   ))
# Lowest cost, highest effectiveness
(calculate_cost_to_2050(cost_per_ha = 1500, carbon_per_ha_per_year = 0.38 +  1.96*0.03  ))
# Highest cost, lowest effectiveness
(calculate_cost_to_2050(cost_per_ha = 2500, carbon_per_ha_per_year = 0.38 - 1.96*0.03  ))


# Liana cutting 

# Average cost 
(calculate_cost_to_2050(cost_per_ha = 235,  carbon_per_ha_per_year = 1.31   ))
# Lowest cost, highest effectiveness
(calculate_cost_to_2050(cost_per_ha = 140, carbon_per_ha_per_year =  1.31 + 1.96*0.13  ))
# Highest cost, lowest effectiveness
(calculate_cost_to_2050(cost_per_ha = 330,  carbon_per_ha_per_year = 1.31 - 1.96*0.13  ))



