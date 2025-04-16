
# Written by Toby Jackson for the paper: 

library(tidyverse); library(magrittr)
library(readxl); library(MetBrewer)
library(ggplot2); library(patchwork); 

# Load & summarize the data ######

sbe_folder="/Users/av23907/dropbox/3_sbe/"
df_sbe = readr::read_csv(file=paste0(sbe_folder,"data/temp_data/SBE_disturbance.csv"))
df_dan = readr::read_csv(file=paste0(sbe_folder,"data/temp_data/Dan_disturbance.csv"))
df = dplyr::bind_rows(df_sbe,df_dan)

# Manually add scanning interval to data frame
df$interval=6.25 # years

# Sort out factors for figures ########
df$name %<>%  factor(levels = c("Logged control", "Enrichment planting","Liana cutting", "Primary forest"))
df$class %<>% factor(levels = c("Intact canopy", "Recovering gaps","Canopy disturbances" ))


# Convert to annual change rates
df %<>% dplyr::mutate(proportion_of_area = 10000*area/(percentage_notNA*tile_area),
                      height_increase_m = height_increase_mean)
                

# Summarize disturbance and recovery for each treatment
df_per_treatment_by_class = df  %>% 
  dplyr::group_by( name, interval, class) %>% 
  summarize(n=n(),num_events = mean(num_events, na.rm=T), 
            mean_area =mean(area,na.rm=T),
            proportion_of_area =mean(proportion_of_area,na.rm=T),
            height_increase_m = mean(height_increase_m*area,na.rm=T)/mean_area) 
          

df_per_treatment_by_class %<>% mutate(volume_increase_m3_pha = mean_area*height_increase_m/4)

# Fig 4. Disturbance and recovery bar charts ##########

# Area
(p1 = ggplot(arrange(df_per_treatment_by_class,class)
             , aes(y= name,x=proportion_of_area ,fill=class, label =round(proportion_of_area )))+
   geom_bar(stat="identity")+
   geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
   theme_bw()+xlim(c(0,101))+
   ylab("")+xlab(paste0("Proportion of canopy area (%)"))
)

# Height 
(p2 = ggplot(arrange(df_per_treatment_by_class,class)
             , aes(y= name,x=height_increase_m ,fill=class, label =round(height_increase_m )))+
    geom_bar(stat="identity")+
    geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
    geom_vline(xintercept=0,color="black")+
    theme_bw()+xlim(c(-21,21))+
    ylab("")+xlab(expression(~ Mean ~ height ~ change ~ m))
)

# Volume
(p3 = ggplot(arrange(df_per_treatment_by_class,class )
             , aes(y= name,x=volume_increase_m3_pha/1000 ,fill=class,label =round(volume_increase_m3_pha/1000)))+
    geom_bar(stat="identity")+
    geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
    geom_vline(xintercept=0,color="black")+
    theme_bw()+
    ylab("")+xlim(c(-35,35))+
    xlab(expression(~ Canopy ~ volume ~ change ~ 1000 ~ m^3 ~ ha^-1 ))
)


combined =  p1/p2/p3 +plot_layout(guides = "collect") &
  scale_fill_manual(name = "", values=met.brewer("Archambault", 7)[c(1,2,5)]) & guides(fill = guide_legend(reverse=TRUE)) & 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
combined

ggsave(plot=combined, filename = paste0(sbe_folder,"SBE_figures/Disturbance_vs_recovery_threeclasses_3groups.png"),
       height = 5, width = 4)


# Counterfactual  calculations #######

# Counterfactual has the height increase from liana cutting
df_counterfactual = df_per_treatment_by_class %>% dplyr::filter(name == "Liana cutting")
df_counterfactual$name = "Counterfactual"

# Set counterfactual area classes to the same as enrichment planting
df_planting = df_per_treatment_by_class %>% dplyr::filter(name == "Enrichment planting")
df_counterfactual$mean_area = df_planting$mean_area
df_counterfactual$proportion_of_area = df_planting$proportion_of_area

# Calculate counterfactual volume change
df_counterfactual %<>% mutate(volume_increase_m3_pha = mean_area*height_increase_m/4)

# Append counterfactual to real data
df_per_treatment_with_counterfactual = rbind(df_per_treatment_by_class,df_counterfactual)

# Remove primary forest and logged control plots because we're not using them here
df_per_treatment_with_counterfactual %<>% dplyr::filter(name !="Primary forest", name != "Logged control")

# Calculate mean height change from volume change
dfs =  df_per_treatment_with_counterfactual %>% ungroup() %>% group_by(name) %>%
  summarize(h_increase = sum(volume_increase_m3_pha,na.rm=T)/10000)

# Real H difference between liana cutting and enrichment planting
liana_minus_planting = dfs$h_increase[dfs$name == "Liana cutting"] - dfs$h_increase[dfs$name == "Enrichment planting"] 

# Difference between counterfactual and enrichment planting. This difference is ONLY due to the faster growth in the counterfactual
counterfactual_minus_planting = dfs$h_increase[dfs$name == "Counterfactual"] - dfs$h_increase[dfs$name == "Enrichment planting"] 

# Proportion of the difference between treatments due ONLY to faster growth
round(100*counterfactual_minus_planting/liana_minus_planting)


  
# S9. Add counterfactual to the bar chart ######
# Area
(p1 = ggplot(arrange(df_per_treatment_with_counterfactual,class)
             , aes(y= name,x=proportion_of_area ,fill=class, label =round(proportion_of_area )))+
   geom_bar(stat="identity")+
   geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
   theme_bw()+xlim(c(0,101))+
   ylab("")+xlab(paste0("Proportion of canopy area (%)"))
)

# Height 
(p2 = ggplot(arrange(df_per_treatment_with_counterfactual,class)
             , aes(y= name,x=height_increase_m ,fill=class, label =round(height_increase_m )))+
    geom_bar(stat="identity")+
    geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
    geom_vline(xintercept=0,color="black")+
    theme_bw()+xlim(c(-21,21))+
    ylab("")+xlab(expression(~ Mean ~ height ~ change ~ m))
)

# Volume
(p3 = ggplot(arrange(df_per_treatment_with_counterfactual,class )
             , aes(y= name,x=volume_increase_m3_pha/1000 ,fill=class,label =round(volume_increase_m3_pha/1000)))+
    geom_bar(stat="identity")+
    geom_text(size = 2.5, position = position_stack(vjust = 0.5),color="white")+
    geom_vline(xintercept=0,color="black")+
    theme_bw()+
    ylab("")+xlim(c(-35,35))+
    xlab(expression(~ Canopy ~ volume ~ change ~ 1000 ~ m^3 ~ ha^-1 ))
)


combined =  p1/p2/p3 +plot_layout(guides = "collect") &
  scale_fill_manual(name = "", values=met.brewer("Archambault", 7)[c(1,2,5)]) & guides(fill = guide_legend(reverse=TRUE)) & 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
combined

ggsave(plot=combined, filename = paste0(sbe_folder,"SBE_figures/Disturbance_vs_recovery_counterfactual.png"),
       height = 4, width = 4)
