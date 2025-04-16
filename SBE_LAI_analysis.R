

library(tidyverse); library(magrittr); library(readxl)
library(sf);   library(terra)
library(lidR); library(leafR);
library(ggplot2); library(MetBrewer); library(patchwork)

# Read processed data
sbe_folder="/Users/av23907/Dropbox/3_sbe/"

plot_lads = readr::read_csv(paste0(sbe_folder,file = "data/SBE_LAI_profiles.csv"))

plot_lads$name = NA
plot_lads$name[plot_lads$treatment =="0_spp"] = "Logged control"
plot_lads$name[plot_lads$treatment =="1_spp"] = "Enrichment planting"
plot_lads$name[plot_lads$treatment =="4_spp"] = "Enrichment planting"
plot_lads$name[plot_lads$treatment =="16_spp"] = "Enrichment planting"
plot_lads$name[plot_lads$treatment =="16_cut"] = "Liana removal"
plot_lads$name  %<>% factor(levels = (c("Logged control","Enrichment planting","Liana removal" )))
plot_lads$treatment  %<>% factor(levels = (c("0_spp","1_spp","4_spp","16_spp","16_cut" )))
plot_lads$plot[plot_lads$block == "North"] = plot_lads$plot[plot_lads$block == "North"]-64


plot_total_lai = plot_lads %>% group_by(year,treatment,name,plot) %>% 
  summarize(total_lai = sum(lad))


# Fig 2: LAI profiles ######

# Summarize by treatment
control_lad = plot_lads %>% dplyr::filter(name =="Logged control") %>% 
  ungroup() %>% group_by(year, height) %>%
  summarize(control_lad = mean(lad,na.rm=T)) %>% 
  dplyr::select(control_lad, height)  

plot_lads_norm = dplyr::right_join(plot_lads,  control_lad, by = c("height","year"))
plot_lads_norm %<>% dplyr::mutate(norm_lad = lad - control_lad)

grouped_lads_norm = plot_lads_norm %>% group_by(name, year, height) %>%
  summarize(mean_lad = mean(lad,na.rm=T), 
            mean_lad_norm = mean(norm_lad, na.rm=T))


g1 = ggplot(grouped_lads_norm %>% dplyr::filter(year ==2013) ,
            aes(mean_lad,height,color=name,group=name))+geom_point(size=0.8)+
  scale_x_continuous(breaks=c(0,0.2,0.4),limits=c(0,0.4))+
  scale_y_continuous(breaks=c(0,20,40,60),limits=c(0,70))+
  xlab("LAI profile")+ylab("Canopy height (m)")+ggtitle("2013")

g2 = ggplot(grouped_lads_norm %>% dplyr::filter(year ==2020) ,
            aes(mean_lad,height,color=name,group=name))+geom_point(size=0.8)+
  scale_x_continuous(breaks=c(0,0.2,0.4),limits=c(0,0.4))+
  scale_y_continuous(breaks=c(0,20,40,60),limits=c(0,70))+
  xlab("LAI profile")+ylab("")+ggtitle("2020")


g3 = ggplot(grouped_lads_norm %>% dplyr::filter(year ==2013) ,
            aes(mean_lad_norm,height,color=name,group=name))+geom_point(size=0.8)+
    geom_vline(xintercept=0)+xlab("LAI difference \n(treatment - control)")+
  scale_x_continuous(breaks=c(-0.05,0,0.05),limits=c(-0.055,0.055))+
  scale_y_continuous(breaks=c(0,20,40,60),limits=c(0,70))+
    ylab("Canopy height (m)")

g4 = ggplot(grouped_lads_norm %>% dplyr::filter(year ==2020) ,
            aes(mean_lad_norm,height,color=name,group=name))+geom_point(size=0.8)+
    geom_vline(xintercept=0)+xlab("LAI difference \n(treatment - control)")+
  scale_x_continuous(breaks=c(-0.05,0,0.05),limits=c(-0.055,0.055))+
  scale_y_continuous(breaks=c(0,20,40,60),limits=c(0,70))+
  ylab("")

g5 = (g1+g2)/(g3+g4) + plot_layout(guides = "collect") & 
  guides(colour = guide_legend(override.aes = list(size=5)))& theme_bw() &
  scale_color_manual(values= met.brewer("Derain", 7)[c(7,4,1)]) & 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), legend.position = "top",
        legend.title = element_blank(),  panel.grid.minor = element_blank(), panel.grid.major = element_blank()) 

g5

ggsave(plot = g5, paste0(sbe_folder,file = "SBE_figures/4_treatment_mean_lai_profiles_2013_2020.png"),
       height=5,width=4)

# S4. LAI profiles per plot ######
plot_lads_norm_temp = plot_lads_norm %>% dplyr::filter(year ==2020)
grouped_lads_norm_temp = grouped_lads_norm %>% dplyr::filter(year ==2020)
#plot_lads_norm_temp$name  %<>% factor(levels = (c("Enrichment planting","Planting","Liana removal" )))

ggplot()+
  geom_point(data = plot_lads_norm_temp,aes(norm_lad,height,color=as.factor(plot)),size=0.1)+
  geom_point(data = grouped_lads_norm_temp,aes(mean_lad_norm,height),color="black")+
  theme_bw()+facet_wrap(~block+name,nrow=2)+scale_color_viridis_d()+
  geom_vline(xintercept=0)+ylim(c(0,60))+xlim(c(-0.07,0.07))+
  xlab("2020 LAI (treatment - control)")+ylab("Height bin (m)")+  
  theme(legend.position="none")

ggsave(paste0(sbe_folder,file = "SBE_figures/4_plot_lai_profiles_2020_by_treatment.png"),height=6,width=6)

