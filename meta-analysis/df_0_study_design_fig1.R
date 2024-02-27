dat=read.csv(file = 'df_2.csv', na.strings = "NA", header=T)

#### circumpolar map #####
library(ggplot2)
library(rgdal)                                                                                                      
library(raster)
library(dplyr)

# # read in data
# dat=read.csv(file = 'SMD_final.csv', na.strings = "NA", header=T)

# add country code
library(countrycode)
dat$Code<- countrycode(dat$Country, origin = 'country.name', destination = 'iso3c')
#does not recognize Svalbard -> manually change NA to SJM
dat$Code[is.na(dat$Code)] <- "SJM"

# number of years as radius
years<-dat %>% dplyr::count(Site_ID) 
#rename n in "number of experiments"
#merge with dat
dat<-full_join(dat,years)
n<-as.numeric(dat$n)

# observations per dite as color gradient
obs<-dat %>% dplyr::group_by(Site_ID)  %>% 
  summarise(obs_sum = sum(Nr_Obs))


# observations per dite as color gradient
sumi<-dat %>% dplyr::group_by(Site_ID)  %>% 
  summarise(obs_sum = sum(Nr_Obs),
            n = n(),
            Longitude..DD. = unique(Longitude..DD.),
            Latitude..DD. = unique(Latitude..DD.),
            Country = unique(Country))
#rename n in "number of experiments"
#merge with dat
dat<-full_join(dat,obs)
obs_sum<-as.numeric(dat$obs_sum)

dat$Nr_Obs


# get map data
thismap = map_data("world")

# Defines the x axes required
x_lines <- seq(-120,180, by = 60)

ggplot() +
  geom_polygon(data = thismap, aes(x = long, y = lat, group = group), fill = "lightgrey", colour = "white") +
  
  # Convert to polar coordinates
  coord_map("ortho", orientation = c(90, 0, 0)) +
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  
  # Adds points
  geom_point(data=dat, aes(x=Longitude..DD., y=Latitude..DD.,  
                           group=paste(Site_ID,Country), 
                           size=n, fill=Nr_Obs),
             color="black", 
             pch=21, 
             alpha=0.5)+  
  scale_fill_viridis_c(name = "Number of observations", trans="log",option="magma",
                       breaks=c(50,500, 5000), labels=c(50,500, 5000), direction=-1)+ # add color gradient
  #scale_fill_gradient(name = "Number of years",trans="log",low = "blue", high = "red")+
  scale_size_continuous(name="Number of years",range=c(2,8),
                        breaks=c(2,4,6),
                        labels=c(4,8,12))+
  
  # geom_hline(yintercept = 66.33487, linetype = "solid", color="black", alpha=0.5)+
  # annotate("text", x=165, y = 70, color = "black", label = "ARCTIC CIRCLE", size=3.5)+
  
  
  # Removes Axes and labels
  scale_x_continuous(breaks = NULL) +
  xlab("") +
  ylab("") +
  
  
  # Adds labels
  geom_text(aes(x = 180, y = seq(45, 85, by = 20), hjust = -0.2, label = paste0(seq(45, 85, by = 20), "°N"))) +
  geom_text(aes(x = x_lines, y = 15, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
  # geom_hline(yintercept = 66.33487, linetype = "dashed", color="black", alpha=0.5)+
  # annotate("text", x=165, y = 70, color = "black", label = "ARCTIC CIRCLE", size=3.5)+
  
  # Adds axes
  # geom_hline(aes(yintercept = 10), size = 0.8) +  # make it round?
  geom_segment(aes(y = 10, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
  
  # Change theme to remove axes and ticks
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "darkgrey"),
        axis.ticks=element_blank(),
        legend.position = "top") 



# Australia

ggplot() + geom_sf(data = worldmap, colour="white", fill="lightgray") +
  coord_sf(xlim = c(110, 160), ylim = c(0, -50), expand = FALSE) +
  geom_point(aes(x=dat$Longitude, y=dat$Latitude,  group=paste(dat$Site_ID,dat$Country), fill="ffb07a80", size=n), colour = "black", pch=21, alpha=0.5)+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=20,color="black"),
        axis.title=element_text(size=15))+
  scale_x_continuous(breaks = c(120,140,160))+
  scale_y_continuous(breaks = c(-40,-20,0,  by = 10))+
  xlab("Longitude") + ylab("Latitude")

#### barplots for Figure 1 #####
ph<-dat %>% dplyr::count(pH) %>%
  ggplot(aes(x=pH, y=n, fill=pH))+
  ggtitle("pH")+
  xlab("")+ylab("")+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(0,20,40,60,80,100))+
  geom_col(fill='grey')+
  #geom_col(fill='#73AF48')+
  scale_x_discrete(limits=c("Low","Medium","High"))+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15, color="black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size = 15, face = "bold"))+
  geom_text(aes(label=n),position=position_dodge(width=0.9), vjust=-0.3)

veg<-dat %>% dplyr::count(Veg_Class) %>%
  ggplot(aes(x=Veg_Class, y=n, fill=Veg_Class))+
  ggtitle("Vegetation type")+
  xlab("")+ylab("")+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(0,20,40,60,80,100))+
  geom_col(fill='grey')+
  #geom_col(fill='#0F8554')+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15, color="black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size = 15, face = "bold"))+
  geom_text(aes(label=n),position=position_dodge(width=0.9), vjust=-0.3)
#scale_x_discrete(labels=c("B" ="Cryptogam", "G"="Graminoid", "P" ="Prostrate dwarf-shrub", "S" = "Erect dwarf-shrub", "W" ="Wetland"))

moist<- dat %>%  dplyr::count(Soil_Moist_CAT) %>%
  ggplot(aes(x=Soil_Moist_CAT, y=n, fill=Soil_Moist_CAT))+
  ggtitle("Soil moisture")+
  xlab("")+ylab("")+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(0,20,40,60,80,100))+
  geom_col(fill='grey')+
  #geom_col(fill='#1D6996')+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15, color="black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size = 15, face = "bold"))+
  geom_text(aes(label=n),position=position_dodge(width=0.9), vjust=-0.3)+
  scale_x_discrete(labels=c("dry" ="Dry", "mesic"="Mesic", "wet" ="Wet"))

zone<- dat %>%  dplyr::count(Zone) %>%
  ggplot(aes(x=Zone, y=n, fill=Zone))+
  ggtitle("Zone")+
  xlab("")+ylab("")+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(0,20,40,60,80,100))+
  geom_col(fill='grey')+
  #geom_col(fill='#1D6996')+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15,color="black"),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size = 15, face = "bold"))+
  geom_text(aes(label=n),position=position_dodge(width=0.9), vjust=-0.3)+
  scale_x_discrete(breaks=c("Alpine", "Low arctic/subarctic", "High arctic"),
                   limits=c("Alpine", "Low arctic/subarctic", "High arctic"),
                   labels=c("Alpine" ="Alpine", "High arctic"="High \nArctic", "Low arctic/subarctic" ="Low \nArctic"))

library(data.table)

duration = data.table(
  dur_YRS = c(70,28,15,5,15,3),
  dur_class = as.factor(c("[0-5)","[5-10)","[10-15)","[15-20)","[20-25)","[25-30)")))


dur<- duration %>%
  ggplot(aes(x=dur_class, y=dur_YRS, fill=dur_class))+
  ggtitle("Duration")+
  xlab("")+ylab("Number of datasets")+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(0,20,40,60,80,100))+
  scale_x_discrete(breaks = c("[0-5)","[5-10)","[10-15)","[15-20)","[20-25)","[25-30)"),
                   limits = c("[0-5)","[5-10)","[10-15)","[15-20)","[20-25)","[25-30)"))+
  geom_col(fill='grey')+
  #geom_col(fill='#1D6996')+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15,color="black"),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size = 15, face = "bold"))+
  geom_text(aes(label=dur_YRS),position=position_dodge(width=0.9), vjust=-0.3)

## arrange figures 1B

B <- ggarrange(zone, moist, ph, veg, 
               nrow = 1,
               ncol=4,
               labels=c("b"),
               align = c("hv"))

annotate_figure(B, left = text_grob("Number of datasets", rot= 90, size=15, vjust=1.8, hjust=0.4))