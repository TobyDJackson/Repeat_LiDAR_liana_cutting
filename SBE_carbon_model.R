
library(tidyverse); library(magrittr); library(readxl)
library(ggplot2); library(MetBrewer); library(stats)
library(marginaleffects)

# Load summary data ####
sbe_folder="/Users/av23907/dropbox/3_sbe/"


dt = readr::read_csv(paste0(sbe_folder,"data/SBE_data_canopy_height_and_elevation.csv"))

# Remove primary forest as I'm only interested in the experiment
dt %<>% dplyr::filter(treatment !="Danum") 

summary(as.factor(dt$name))
dt$treatment  %<>% factor(levels = (c("0_spp","1_spp","4_spp", "16_spp","16_cut" )))
dt$name  %<>% factor(levels = (c("Logged control","Enrichment planting","Liana cutting")))




# Build carbon model ####
# Load data from Jucker et al. (2018). https://doi.org/10.5194/bg-15-3811-2018 

dt_jucker  = readr::read_csv(paste0(sbe_folder,"data/carbon_model/Sabah_ACD_data.csv"))
ggplot(dt_jucker,aes(TCH, ACD))+geom_point()+geom_smooth(method="lm")+theme_bw()+
  xlab("Mean canopy height (m)")+ylab("Aboveground carbon density (tons per ha)")+
  ggtitle("Sabah carbon model (Jucker et al 2018)") #+#ggpmisc::stat_poly_eq(use_label(c("eq", "R2")))
  #scale_x_log10()+scale_y_log10()
#ggsave( paste0(sbe_folder,file = "SBE_figures/S11_Carbon_model.png"),
#       height=4,width=4)

dt_jucker %<>% mutate(log_ACD = log(ACD), 
                      log_H = log(TCH))

# Test both a linear and logged model - the results are very similar
lm_carbon_linear = lm(ACD ~ TCH, data=dt_jucker) 
lm_carbon_logged = lm(log_ACD ~ log_H, data=dt_jucker) 


# Apply carbon model ####
dt %<>% mutate(TCH = canopy_height, 
               log_H = log(canopy_height))

dt$ACD_linear = predict(lm_carbon_linear, dt, se.fit=T)$fit

dt$log_ACD = predict(lm_carbon_logged, dt, se.fit=T)$fit
dt %<>% mutate(ACD_transformed = exp(log_ACD))


# __ Models #####
names(dt)
dt$name  %<>% factor(levels = (c("Logged control","Enrichment planting", "Liana cutting", "Primary forest")))
lm_2013   = lm(ACD_linear ~ elevation + name + block, data=dt %>% dplyr::filter(year=="2013"))
lm_2020   = lm(ACD_linear ~ elevation + name + block, data=dt %>% dplyr::filter(year=="2020"))
lm_change = lm(ACD_linear ~ elevation + name + block, data=dt %>% dplyr::filter(year=="change"))

plot(lm_change)
summary(lm_change)

clipr::write_clip(rbind(tidy(lm_2013),tidy(lm_2020),tidy(lm_change)))

lm_2020_linear   = lm(ACD_linear ~ elevation + name + block, data=dt %>% dplyr::filter(year=="2020"))
summary(lm_2020_linear)
lm_2020_transformed   = lm(ACD_transformed ~ elevation + name + block, data=dt %>% dplyr::filter(year=="2020"))
summary(lm_2020_transformed)
# Cost of carbon to 2050 ######
# These results are in row F of Table 1.

calculate_cost_to_2050 = function(cost_per_ha, carbon_per_ha_per_year ){
  # Written in 2025, so there are 25 years to 2050
  cost_to_2050 = cost_per_ha/(25*carbon_per_ha_per_year)
}

# These numbers were added manually from the table.

# Enrichment planting
# Lowest cost, highest effectiveness
(calculate_cost_to_2050(cost_per_ha = 1500, carbon_per_ha_per_year = 0.51 +  1.96*0.22  ))
# Highest cost, lowest effectiveness
(calculate_cost_to_2050(cost_per_ha = 2500, carbon_per_ha_per_year = 0.51 - 1.96*0.22  ))
# Average cost 
(calculate_cost_to_2050(cost_per_ha = 2000, carbon_per_ha_per_year = 0.51   ))

# Liana cutting 
# Lowest cost, highest effectiveness
(calculate_cost_to_2050(cost_per_ha = 140, carbon_per_ha_per_year =  1.71 + 1.96*0.27  ))
# Highest cost, lowest effectiveness
(calculate_cost_to_2050(cost_per_ha = 330,  carbon_per_ha_per_year = 1.71 - 1.96*0.27  ))
# Average cost 
(calculate_cost_to_2050(cost_per_ha = 235,  carbon_per_ha_per_year = 1.71   ))
