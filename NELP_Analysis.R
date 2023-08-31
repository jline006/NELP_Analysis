# NELP isotope and nutrient real data -Wet season- conduct biplots, boxplots, violin plots
# author - Joshua Linenfelser w/ help from Bryan hayden script
# created - 6/19/22

library(tidyverse)
library(scales)
library(viridis)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(plyr)
library(forcats)
library(ggforce)
library(readr)
library(lubridate)
library(plotly)
library(xlsx)
library(dplyr)
library(ggpubr)
library(gt)

# setwd('C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analysis/data')

setwd("~/R/NELP_Real_Analyses/data")

#############################################################
####Wet Season Data####
#############################################################

#### Prepare data for use ####

#Load your data
data<-read_csv("Master_NELP_Data.csv") 

data<-read_csv("Master_NELP_Data_WChickee.csv") 

# coords <- read_csv("Sites_Coordinates.csv")

#Merged data to add coordinates and exported for GIS
# 
# data<-merge(data,coords,by=c('Code'),all.x=T)
# 
# data

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh",
                            "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%  
  # mutate(System2= recode(System2, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(System= recode(System, "Garfield Bight"="Alligator Creek System", "Terrapin Bay" = "McCormick Creek System", "Taylor River Bridge"="Taylor Slough Marsh" , "Taylor Slough Marsh"= "Taylor Slough Marsh" ) ) %>%
   mutate(System = fct_relevel(System, "Taylor Slough Marsh", "Alligator Creek System", "McCormick Creek System", )) %>%
  # mutate(System2 = fct_relevel(System2, "Marsh","Alligator Creek System", "McCormick Creek System","Bay")) %>% 
  # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  # mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(System, "Alligator Creek System"="ECLS", "McCormick Creek System"="ECLS")) %>%
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  filter(Species != "Excrement")%>%
  # filter(Lake != "Shark Chickee") %>%
  # filter(Lake != "Cuthbert Rookery") 
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") %>%  
  # filter(System2 != "Alligator Creek System") %>% 
  # filter(System2 != "McCormick Creek System") %>% 
  # filter(System2 != "Bay") %>% 
  filter(System2 != "Chasar")


glimpse(new.data)
  
  
  # filter(System2 == "Marsh")

# write.csv(new.data.TS,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/new.data.systems.bays.csv", row.names = TRUE)

# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analysis/data/datawcoords.csv")
# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analyses/data/datawcoords2.csv", row.names = F)


new.data<- subset(data, Season == "Wet") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>%
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N), C.N = as.double(C/N))


#View the first 10 rows of data
head(data)
glimpse(new.data)


#Summarize data

data.sum<-ddply(new.data, c("System", "Season"), summarise,
                count = length(Code),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N),
                mS = mean(d34S, na.rm=TRUE), sdS = sd(d34S, na.rm=TRUE)) %>% 
  rename(
                  # Lake = Lake2,
                  n = count,
                  Mean.C13 = mC,
                  "SD C13"=sdC,
                  Mean.N15=mN,
                  SD.N15=sdN
                )
  

mean_table <- data.sum %>% gt()%>%
  tab_header(
    title = md("Wet '21 - C13 and N15 Summary"),
    subtitle = "Isotopic data taken from microalgae in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))
  # tab_style(
  #   style = list(
  #     # cell_fill(color = "lightcyan")
  #     # cell_text(weight = "bold")
  #   ),
  #   locations = cells_body(
  #     # columns = c(Species),
  #     # rows = num >= 5000
  #   ))

mean_table

data.sum<-ddply(data, c("Lake"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N))

sum = new.data %>% 
  group_by(System2) %>% 
  dplyr::summarize(count = length(Species), mN = mean(d15N), sdN = sd(d15N), mC = mean(d13C), sdC = sd(d13C)) 

write.csv(sum,"C:/Users/Joshua_Linenfelser/Documents/R/Ryan_MixingModels_practice/data/ave_source_data.csv", row.names = TRUE)


glimpse(data)

#Plotting data 
  

glimpse(new.data)

new.data %>% 
  with(.,table(System2))  







#### Exploratory Plots isotopes ####

# ggplot(new.data, aes(x="", y=d13C, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab(expression(paste(delta^{13}, "C (\u2030)")))+
#   xlab("")

system_comparisons <- list(c("Alligator Creek System", "McCormick Creek System"), c("Alligator Creek System", "Taylor Slough"), c("McCormick Creek System", "Taylor Slough"))
# lake_comparison <- list(c("West Lake", "Cuthbert Lake"), c("West Lake", "Long Lake"), c("West Lake", "The Lungs"), c("West Lake", "Garfield Bight"))


ggplot(new.data, aes(x=System2, y=d13C, colour=System2)) +                 
  geom_boxplot() +
  ylim(-40, -10)+
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)  +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_fill_manual(name="System")+
  labs(title = "Carbon Isotope Boxplots")+
  theme_gray()+
  # labs(fill="System")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=d15N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))+
#   xlab("")

ggplot(new.data, aes(x=System2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  ylim(1,6.5)+
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)  +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=d15N, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))+
#   xlab("")

ggplot(new.data, aes(x="", group = Species, y=d15N, colour=System2)) +
  geom_violin() +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
# scale_colour_discrete(name="Lakes")+
  labs(title = "Nitrogen Isotope Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x=Lake, y=d15N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   # facet_grid(. ~ Lake) +
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))

ggplot(new.data, aes(x=Lake2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ Lake) +
ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Systems")+
xlab("")+
  labs(title = "Nitrogen Isotope Boxplots")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
 

# ggplot(new.data, aes(x="", group = Species, y=d13C, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab(expression(paste(delta^{13}, "C (\u2030)")))+
#   xlab("")

ggplot(new.data, aes(x="", group = Species, y=d13C, colour=System2)) +
  geom_violin() +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  labs(title = "C13 Isotope Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x="", group = Species, y=d13C, colour=System2)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#N15 and C13 lakes in order

ggplot(new.data, aes(x=reorder(Lake,d13C), group = Lake, y=d13C, colour=System)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake,d15N), group = Lake, y=d15N, colour=System)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N15 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,d13C), group = Lake2, y=d13C, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,d15N), group = Lake2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N15 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



## Only ACS

new.data.ACS <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d13C, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d15N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#Add stats and only Lakes2 against zones

lake_comparisons <- list(c("West Lake", "Cuthbert Lake"), c("West Lake", "Long Lake"), c("West Lake", "The Lungs"), c("West Lake", "Garfield Bight"), c("Cuthbert Lake", "Long Lake"), c("Cuthbert Lake", "The Lungs"), c("Cuthbert Lake", "Garfield Bight"), c("Long Lake", "The Lungs"),c("Long Lake", "The Lungs"),c("Long Lake", "Garfield Bight"),c("The Lungs", "Garfield Bight"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d13C, colour=Zone)) +                 
  geom_boxplot() +
  ylim(-30,-15)+
  # stat_compare_means(comparisons = lake_comparisons)+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 50)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  labs(title = "ACS Carbon Isotope Boxplots")+
  scale_colour_viridis_d(option= "D")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d15N, colour=Zone)) +                 
  geom_boxplot() +
  ylim(2,5)+
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  # scale_colour_discrete(name"Lakes")+
  labs(title = "ACS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

####Wet ACS Stats####

#N15 stats

#Check assumptions
library(car)

leveneTest(d15N ~ Lake, data = new.data.ACS)


# Compute the analysis of variance
res.aov <- aov(d15N ~ Lake2, data = new.data.ACS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)


TukeyHSD(res.aov)


#C13 stats

#Check assumptions
library(car)

leveneTest(d13C ~ Lake, data = new.data.ACS)


# Compute the analysis of variance
res.aov <- aov(d13C ~ Lake, data = new.data.ACS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)



## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) 

ggplot(new.data.MCS, aes(x=reorder(Lake,d13C), group = Lake, y=d13C, colour=Lake)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  labs(title = "MCS Ordered Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(x=reorder(Lake,d15N), group = Lake, y=d15N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=d15N, colour=Lake)) +                 
  geom_boxplot() +
  ylim(2,5)+
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=d13C, colour=Lake)) +                 
  geom_boxplot() +
  ylim(-30,-15)
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
  
  
####Wet MCS Stats####  
  
  #N15 stats
  
  #Check assumptions
  library(car)
  
  leveneTest(d15N ~ Lake, data = new.data.MCS)
  
  
  # Compute the analysis of variance
  res.aov <- aov(d15N ~ Lake, data = new.data.MCS)
  
  plot(res.aov,1)
  
  plot(res.aov,2)
  
  
  # Summary of the analysis
  summary(res.aov)
  
  
  TukeyHSD(res.aov)  
  
  
#C13 stats
  
  #N15 stats
  
  #Check assumptions
  library(car)
  
  leveneTest(d13C ~ Lake, data = new.data.MCS)
  
  
  # Compute the analysis of variance
  res.aov <- aov(d13C ~ Lake, data = new.data.MCS)
  
  plot(res.aov,1)
  
  plot(res.aov,2)
  
  
  # Summary of the analysis
  summary(res.aov)
  
  TukeyHSD(res.aov) 
  


#Check by zones MCS and ACS

zone_comparisons <- list(c("Upstream", "Middle"), c("Upstream", "Downstream"), c("Upstream", "Bay"), c("Middle", "Downstream"), c("Middle", "Bay"), c("Downstream", "Bay"))

#Zone stats MCS

glimpse(new.data.MCS)

ggplot(new.data.MCS, aes(Zone, group = Zone, y=d13C, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # ylim(-30, 0)+
  stat_compare_means(comparisons = zone_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = -12 )  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Zone")+
  labs(title = "MCS Zone Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Zone, group = Zone, y=d15N, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  stat_compare_means(comparisons = zone_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 8)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylim(0,8)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Zone Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

##Zone stats at ACS

glimpse(new.data.ACS)

ggplot(new.data.ACS, aes(Zone, group = Zone, y=d13C, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # ylim(-30, 0)+
  stat_compare_means(comparisons = zone_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = -12)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Zone")+
  labs(title = "ACS Zone Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Zone, group = Zone, y=d15N, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  stat_compare_means(comparisons = zone_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 8)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylim(0,8)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Zone Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#ACS and MCS by lake and zone

#MCS and ACS by lake and zone

ggplot(new.data, aes(Lake2, y=d15N, colour=Zone2)) +                 
  geom_boxplot() +
  ylim(0,6.5)+
  scale_colour_viridis_d(option= "D")+
  xlab("")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ Lake2) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Wet '21 Zone Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

##### Biplots isotopes #### 

## All Biplot

biplot.ALL<-ggplot(new.data, aes(x = d13C, y = d15N, colour = System2)) +
  geom_point(alpha = 1.0, size=3.0) +
  # scale_colour_viridis_d(option= "D")+
  geom_mark_ellipse(alpha = 0.3, size=1.0) +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  facet_grid( ) +
  ylim(0,6)+
  xlim(-35,-15)+
  # xlim(0,-25)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Wet '21 System Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ALL


glimpse(new.data)


biplot.ALL.sep<-ggplot(new.data, aes(x = d13C, y = d15N, group=System2, colour=Lake2)) +
  geom_point(alpha = 1.0, size=3.0) +
  geom_mark_ellipse(alpha = 0.3, size=1.0)+
  scale_colour_viridis_d(option= "D")+
  # facet_grid(. ~ System2) +
  ylim(0,6)+
  xlim(-35,-15)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Wet '21 System and Lake Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ALL.sep



#ACS biplot

new.data.ACS <- subset(new.data, System == "Alligator Creek System")

biplot.ACS<-ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  ylim(0,6)+
  xlim(-27, -15)+
  # facet_grid(. ~ System) +
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Lakes Biplot")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ACS


# ?geom_ellipse

#ACS OnlyLakes - dont know how to subset multiple variables

biplot.ACS<-ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_point(alpha = 1.0, size=3.0) +
  scale_colour_viridis_d(option= "D")+
  ylim(0,6)+
  xlim(-27, -15)+
  # facet_grid(. ~ System) +
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake2),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake2,
  #                                color = Lake2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Wet '21 ACS Lakes Biplot", legend= "Lakes")+
  # scale_colour_discrete(name="Lakes")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ACS


#MCS biplot

new.data.MCS <- subset(new.data, System == "McCormick Creek System")

glimpse(new.data.MCS)

biplot.MCS<-ggplot(new.data.MCS, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_point(alpha = 0.7, size=3.5) +
  scale_colour_viridis_d(option= "D")+
  ylim(0,6)+
  xlim(-27, -15)+
  # facet_grid(. ~ Species) +
  # stat_ellipse() +
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Wet '21 MCS Lakes Biplot")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.MCS


#Looking between systems


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 1.0, size=3) +
  facet_grid(. ~ System2) +
  ylim(0,5.5)+
  xlim(-35,-10)+
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #                           aes(fill = Lake2),
  #                           alpha = 0.25)+
  ggforce::geom_mark_ellipse(aes(fill = Lake2,
                                 color = Lake2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Wet '21 Isotope Ellipse Biplot by System")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots

#

sum.biplot2<-ggplot(new.data, aes(x=d13C, y=d15N, colour=Lake2)) + 
  geom_point(size=3) + 
  scale_colour_viridis_d(option= "D")+
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ System2)+
  theme_dark()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot2

#Final summarize -- 

data.sum<-ddply(new.data, c("Lake2", "Species", "System2"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d13Cse=sd(d15N)/sqrt(length(d15N)))

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)



sum.biplot<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Lake2, shape=System2)) + 
  geom_point(size=3) + 
  geom_errorbar(Ylims, width=0.6) + 
  geom_errorbarh(Xlims, height=0.3) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "Wet '21 Coastal Lakes Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
  # theme_bw()
sum.biplot

?viridis

library(RColorBrewer)


#Visualize the difference --


#plotting data to see differences across species groups


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 0.9, size=3) +
  # facet_grid(. ~ Season) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "MCS and ACS Lakes Isotope Biplot")+
  scale_colour_discrete(name="Lakes")+
  theme_gray()+
  ggforce::geom_mark_ellipse(aes(fill = Lake2,
                                 color = Lake2)) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots


# ggsave("biplots.pdf")

#Final summary

data.sum<-ddply(new.data, c("Lake", "Season", "Species", "System", "System2", "Lake2", "Zone2"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d13Cse=sd(d15N)/sqrt(length(d15N)))

glimpse(data.sum)

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)


sum.biplot<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Zone2, shape=System2  )) + 
  geom_point(size=3) + 
  scale_colour_viridis_d(option= "D")+
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ Lake)+
  theme_dark()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot

#Cant get ACS to just show lakes because mean and SD of all lakes are seperate

sum.biplot3<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Lake)) + 
  geom_point(size=3) + 
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ System2)+
  theme_gray()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot3





#### doesn't work ####

#Making a boxplot using melt tool to show the rest, but dont know why I can'ts also see DC13 and C:N 
#Maybe have to fix the concotonate function

glimpse(new.data)

melt.data<-melt(new.data, id.vars = c(9:13))

?melt


melt.boxplot<-ggplot(melt.data, aes(x=System, group = "", y=value, colour=System, fill=Species)) +
  geom_boxplot(alpha=0.3, outlier.shape = NA) +
  # geom_point(position="jitter") +
  facet_grid(microalgae, scales = "free_y") +
  theme_bw()
melt.boxplot

#GGally doesnt work

library(GGally)
ggpairs(new.data[,c(1:6)])




#### Exploratory plots nutrients % ####

glimpse(new.data)

ggplot(new.data, aes(x=System2, y=N, colour=System2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 4.5)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Nitrogen Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=System2, y=C, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 48)  +
  # scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Carbon Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab("N%")+
#   xlab("")

#C/N lakes plot

ggplot(new.data, aes(x="", group = Species, y=C/N, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C/N")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C/N Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#N plots


ggplot(new.data, aes(x="", group = Species, y=N, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Nitrogen Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x=Lake, y=N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   # facet_grid(. ~ Lake) +
#   ylab("N%")


# ggplot(new.data, aes(x="", group = Species, y=d13C, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab("C%")+
#   xlab("")

ggplot(new.data, aes(x="", group = Species, y=C, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Carbon Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x="", group = Species, y=C, colour=System2)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C%")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#N% and C% lakes in order

ggplot(new.data, aes(x=reorder(Lake,C), group = Lake, y=C, colour=System)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake,N), group = Lake, y=N, colour=System)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,C), group = Lake2, y=C, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,N), group = Lake2, y=N, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



## Only ACS

new.data.ACS <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=C, colour=Zone)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=N, colour=Zone)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake2, group = Lake2, y=C, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake2, group = Lake2, y=N, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay"))

ggplot(new.data.MCS, aes(x=reorder(Lake,C), group = Lake, y=C, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(x=reorder(Lake,N), group = Lake, y=N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=N, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=C, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



#TRY mack nutrient plot-didnt work

glimpse(new.data)

data.sum<-ddply(new.data, c("Lake"), summarise,
                mC = mean(d13C), 
                mN = mean(d15N),
                mCN = mean(C/N))

p1 <- ggplot(new.data, aes(x=Lake2, y=C, group=Lake2, color=System2)) + 
  geom_line(aes(size = Lake2)) +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d(option= "D") +
  labs(title = "Spatiotemporal Variability in McCormick Creek Subestuary N:P Ratios", 
       x = "Lakes", y = "Mean C:N Ratio") +
  scale_x_discrete(expand = c(0,0.1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.25, 0.8))

p1




#### Statistical analyses ####

#Kruskal N15 vs lake

glimpse(new.data)

levels(new.data$Lake2)
new.data$d15N

library(dplyr)
group_by(new.data, Lake2) %>%
  summarise(
    count = n(),
    mean = mean(d15N, na.rm = TRUE),
    sd = sd(d15N, na.rm = TRUE),
    median = median(d15N, na.rm = TRUE),
    IQR = IQR(d15N, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "Lake2", y = "d15N", 
          color = "Lake2",
        
          ylab = "N15", xlab = "Lake")


library("ggpubr")
ggline(new.data, x = "Lake2", y = "d15N", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "N15", xlab = "Lake")

#Kruskal wallis test shows significance

kruskalN15.lake <- kruskal.test(d15N ~ Lake2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers?

wilcox.pairwiseN1.lake <- pairwise.wilcox.test(new.data$d15N, new.data$Lake2,
                                  p.adjust.method = "BH")



# Kruskal N15 vs system

library(dplyr)
group_by(new.data, System2) %>%
  summarise(
    count = n(),
    mean = mean(d15N, na.rm = TRUE),
    sd = sd(d15N, na.rm = TRUE),
    median = median(d15N, na.rm = TRUE),
    IQR = IQR(d15N, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "System2", y = "d15N", 
          color = "System2",
          
          ylab = "N15", xlab = "System")


library("ggpubr")
ggline(new.data, x = "System2", y = "d15N", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "N15", xlab = "System")

#Kruskal wallis test shows significance

kruskal.test(d15N ~ System2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d15N, new.data$System2,
                     p.adjust.method = "")


#Kruskal C13 vs lake

glimpse(new.data)

levels(new.data$Lake2)
new.data$d13C

library(dplyr)
group_by(new.data, Lake2) %>%
  summarise(
    count = n(),
    mean = mean(d13C, na.rm = TRUE),
    sd = sd(d13C, na.rm = TRUE),
    median = median(d13C, na.rm = TRUE),
    IQR = IQR(d13C, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "Lake2", y = "d13C", 
          color = "Lake2",
          
          ylab = "C13", xlab = "Lake")


library("ggpubr")
ggline(new.data, x = "Lake2", y = "d13C", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "C13", xlab = "Lake")

#Kruskal wallis test shows significance

kruskal.test(d13C ~ Lake2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d13C, new.data$Lake2,
                     p.adjust.method = "BH")


# Kruskal C13 vs system

library(dplyr)
group_by(new.data, System2) %>%
  summarise(
    count = n(),
    mean = mean(d13C, na.rm = TRUE),
    sd = sd(d13C, na.rm = TRUE),
    median = median(d13C, na.rm = TRUE),
    IQR = IQR(d13C, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "System2", y = "d13C", 
          color = "System2",
          
          ylab = "C13", xlab = "System")


library("ggpubr")
ggline(new.data, x = "System2", y = "d13C", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "C13", xlab = "System")

#Kruskal wallis test shows significance

kruskal.test(d13C ~ System2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d13C, new.data$System2,
                     p.adjust.method = "BH")








#############################################################
#####Dry Season Data####
#############################################################

#### Prepare data for use ####

#Load your data
data<-read_csv("Master_NELP_Data.csv")

# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analyses/master_wet_data.csv", row.names = TRUE)

# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analysis/data/datawcoords.csv")
# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analyses/data/datawcoords2.csv", row.names = F)


#View the first 10 rows of data
head(data)
glimpse(data)

new.data<-  subset(data, Season == "Dry") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
   mutate(d15N = as.double(d15N), N = as.double(N))

glimpse(new.data)

#Summarize data

data.sum<-ddply(new.data, c("Lake2", "Species"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N))%>% rename(
                  Lake = Lake2,
                  Count = count,
                  Mean.C13 = mC,
                  SD.C13=sdC,
                  Mean.N15=mN,
                  SD.N15=sdN
                )

mean_table <- data.sum %>% gt()%>%
  tab_header(
    title = md("Dry'22 - C13 and N15 Summary"),
    subtitle = "Isotopic data taken from microalgae in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))

mean_table

data.sum<-ddply(data, c("Lake"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N))


glimpse(data)

#Plotting data 

# new.data <- na.omit(new.data)

glimpse(new.data)

new.data %>% 
  with(.,table(System2))  



#### Exploratory Plots isotopes ####

# ggplot(new.data, aes(x="", y=d13C, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab(expression(paste(delta^{13}, "C (\u2030)")))+
#   xlab("")

system_comparisons <- list(c("Alligator Creek System", "McCormick Creek System"), c("Alligator Creek System", "Taylor Slough"), c("McCormick Creek System", "Taylor Slough"))
# lake_comparison <- list(c("West Lake", "Cuthbert Lake"), c("West Lake", "Long Lake"), c("West Lake", "The Lungs"), c("West Lake", "Garfield Bight"))


ggplot(new.data, aes(x=System2, y=d13C, colour=System2)) +                 
  geom_boxplot() +
  ylim(-40, -10)+
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = -10)  +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=d15N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))+
#   xlab("")


ggplot(new.data, aes(x=System2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  ylim(1,6.5)+
  stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 6)  +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=d15N, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))+
#   xlab("")

ggplot(new.data, aes(x="", group= Species, y=d15N, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_point(position="jitter")+
  # scale_colour_viridis_d(option= "D")+
  facet_grid(. ~ Lake2)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Nitrogen Isotope Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x=Lake, y=d15N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   # facet_grid(. ~ Lake) +
#   ylab(expression(paste(delta^{15}, "N (\u2030)")))

ggplot(new.data, aes(x=Lake2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ Lake) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Systems")+
  xlab("")+
  labs(title = "Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


# ggplot(new.data, aes(x="", group = Species, y=d13C, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab(expression(paste(delta^{13}, "C (\u2030)")))+
#   xlab("")

ggplot(new.data, aes(x="", group = Species, y=d13C, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Isotope Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x="", group = Species, y=d13C, colour=System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#N15 and C13 lakes in order

ggplot(new.data, aes(x=reorder(Lake,d13C), group = Lake, y=d13C, colour=System)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake,d15N), group = Lake, y=d15N, colour=System)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N15 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,d13C), group = Lake2, y=d13C, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C13 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,d15N), group = Lake2, y=d15N, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N15 Ordered Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



## Only ACS

new.data.ACS <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d13C, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d15N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d13C, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # stat_compare_means(comparisons = lake_comparisons)+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 0)  +
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=d15N, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


####Dry ACS stats####

levels(new.data.ACS$Lake2)

new.data.ACS$Lake

library(dplyr)
group_by(new.data.ACS, Lake) %>%
  summarise(
    count = n(),
    mean = mean(d15N, na.rm = TRUE),
    sd = sd(d15N, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data.ACS, x = "Lake", y = "d15N", 
          color = "Lake",
          # order = c("ctrl", "trt1", "trt2"),
          ylab = "N15", xlab = "Lake")


ggline(new.data.ACS, x = "Lake", y = "d15N", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "N15", xlab = "Lake")

#Check assumptions
library(car)

leveneTest(d15N ~ Lake, data = new.data.ACS)


# Compute the analysis of variance
res.aov <- aov(d15N ~ Lake, data = new.data.ACS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)


TukeyHSD(res.aov)

#C13 stats

#Check assumptions
library(car)

leveneTest(d13C ~ Lake, data = new.data.ACS)


# Compute the analysis of variance
res.aov <- aov(d13C ~ Lake, data = new.data.ACS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)


## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay"))

ggplot(new.data.MCS, aes(x=reorder(Lake,d13C), group = Lake, y=d13C, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(x=reorder(Lake,d15N), group = Lake, y=d15N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=d15N, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=d13C, colour=Zone)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  scale_colour_viridis_d(option= "D")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#Check by zones
new.data$Lake2

ggplot(new.data, aes(x="", group = Lake2, y=d13C, colour=Zone2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  facet_grid(. ~ System2) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  scale_colour_discrete(name="Lakes")+
  labs(title = "Zone Carbon Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(Lake2, y=d15N, colour=Zone2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  xlab("")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ Lake2) +
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Zone Nitrogen Isotope Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


####Dry MCS stats####

#Check assumptions
library(car)

leveneTest(d15N ~ Lake, data = new.data.MCS)


# Compute the analysis of variance
res.aov <- aov(d15N ~ Lake, data = new.data.MCS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)


TukeyHSD(res.aov)

#C13 stats

#Check assumptions
library(car)

leveneTest(d13C ~ Lake, data = new.data.MCS)


# Compute the analysis of variance
res.aov <- aov(d13C ~ Lake, data = new.data.MCS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

#C13 stats

#Check assumptions
library(car)

leveneTest(d13C ~ Lake, data = new.data.MCS)


# Compute the analysis of variance
res.aov <- aov(d13C ~ Lake, data = new.data.MCS)

plot(res.aov,1)

plot(res.aov,2)


# Summary of the analysis
summary(res.aov)

tukey.test<- TukeyHSD(res.aov)

plot(tukey.test)

##### Biplots isotopes #### 

## All Biplot

na.omit(new.data$d15N)

new.data

biplot.ALL<-ggplot(new.data, aes(x = d13C, y = d15N, colour = System2)) +
  geom_point(alpha = 1.0, size=3.0) +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_mark_ellipse(alpha = 0.3, size=1.0) +
  facet_grid( ) +
  ylim(0,7)+
  xlim(-35,-10)+
  # xlim(0,-25)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Dry '22 -System Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ALL


glimpse(new.data)


biplot.ALL.sep<-ggplot(new.data, aes(x = d13C, y = d15N, group=System2, colour=Lake2)) +
  geom_point(alpha = 1.0, size=3.0) +
  geom_mark_ellipse(alpha = 0.3, size=1.0) +
  scale_colour_viridis_d(option= "D")+
  # facet_grid(. ~ System2) +
  ylim(0,7)+
  xlim(-35,-10)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Dry '22 - System and Lake Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_grey()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ALL.sep



#ACS biplot

new.data.ACS <- subset(new.data, System == "Alligator Creek System")

biplot.ACS<-ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  geom_point(alpha = 1.0, size=3.0) +
  ylim(0,6)+
  xlim(-27, -15)+
  facet_grid(. ~ System) +
  theme_bw() +
  scale_colour_viridis_d(option= "D")+
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Lakes Biplot")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ACS


# ?geom_ellipse

#ACS OnlyLakes - dont know how to subset multiple variables

biplot.ACS<-ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 1.0, size=3.0) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  geom_point(alpha = 1.0, size=3.0) +
  ylim(0,6)+
  xlim(-27, -15)+
  scale_colour_viridis_d(option= "D")+
  # facet_grid(. ~ System) +
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake2),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake2,
  #                                color = Lake2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "ACS Lakes Biplot", legend= "Lakes")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ACS


#MCS biplot

new.data.MCS <- subset(data, System == "McCormick Creek System")

biplot.MCS<-ggplot(new.data.MCS, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_point(alpha = 0.7, size=3.5) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  ylim(0,8)+
  xlim(-27, -14)+
  scale_colour_viridis_d(option= "D")+
  # facet_grid(. ~ Species) +
  
  # stat_ellipse() +
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "MCS Lakes Biplot")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.MCS


#Looking between systems


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 1.0, size=3) +
  facet_grid(. ~ System2) +
  scale_colour_viridis_d(option= "D")+
  ylim(0,5.5)+
  xlim(-35,-10)+
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #                           aes(fill = Lake2),
  #                           alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake2,
  #                                color = Lake2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Isotope Ellipse Biplot by System")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots

#

sum.biplot2<-ggplot(new.data, aes(x=d13C, y=d15N, colour=Lake2)) + 
  geom_point(size=3) + 
  scale_colour_viridis_d(option= "D")+
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ System2)+
  theme_gray()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot2

#Final summarize -- 

data.sum<-ddply(new.data, c("Lake2", "Species", "System2"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d13Cse=sd(d15N)/sqrt(length(d15N)))

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)



sum.biplot<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Lake2, shape=System2)) + 
  geom_point(size=3) + 
  geom_errorbar(Ylims, width=0.6) + 
  geom_errorbarh(Xlims, height=0.3) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "Dry '22 - Coastal Lakes Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
# theme_bw()
sum.biplot

?viridis

library(RColorBrewer)


#Visualize the difference --


#plotting data to see differences across species groups


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 0.9, size=3) +
  scale_colour_viridis_d(option= "H")+
  # facet_grid(. ~ Season) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "MCS and ACS Lakes Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  # ggforce::geom_mark_ellipse(aes(fill = Lake2,
  #                                color = Lake2)) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots


# ggsave("biplots.pdf")

#Final summary

data.sum<-ddply(new.data, c( "Season", "System", "System2", "Lake"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d13Cse=sd(d15N)/sqrt(length(d15N)))

glimpse(data.sum)

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)


sum.biplot<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Lake )) + 
  geom_point(size=3) +
  scale_colour_viridis_d(option= "D")+
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ Lake)+
  theme_dark()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot

#Cant get ACS to just show lakes because mean and SD of all lakes are seperate

sum.biplot3<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Lake)) + 
  geom_point(size=3) + 
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ System2)+
  theme_gray()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
sum.biplot3





#### doesn't work ####

#Making a boxplot using melt tool to show the rest, but dont know why I can'ts also see DC13 and C:N 
#Maybe have to fix the concotonate function

glimpse(new.data)

melt.data<-melt(new.data, id.vars = c(9:13))

?melt


melt.boxplot<-ggplot(melt.data, aes(x=System, group = "", y=value, colour=System, fill=Species)) +
  geom_boxplot(alpha=0.3, outlier.shape = NA) +
  # geom_point(position="jitter") +
  facet_grid(microalgae, scales = "free_y") +
  theme_bw()
melt.boxplot

#GGally doesnt work

library(GGally)
ggpairs(new.data[,c(1:6)])




#### Exploratory plots nutrients % ####

glimpse(new.data)

ggplot(new.data, aes(x=System2, y=N, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Nitrogen Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=System2, y=C, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Carbon Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab("N%")+
#   xlab("")

#C/N lakes plot

ggplot(new.data, aes(x="", group = Species, y=C/N, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C/N")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C/N Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#N plots


ggplot(new.data, aes(x="", group = Species, y=N, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Nitrogen Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x=Lake, y=N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   # facet_grid(. ~ Lake) +
#   ylab("N%")


# ggplot(new.data, aes(x="", group = Species, y=d13C, colour=Lake)) +
#   geom_violin() +
#   geom_point(position="jitter")+
#   facet_grid(. ~ Lake)+
#   ylab("C%")+
#   xlab("")

ggplot(new.data, aes(x="", group = Species, y=C, colour=System2)) +
  geom_violin() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Carbon Violinplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x="", group = Species, y=C, colour=System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  facet_grid(. ~ Lake2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "C%")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#N% and C% lakes in order

ggplot(new.data, aes(x=reorder(Lake,C), group = Lake, y=C, colour=System)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake,N), group = Lake, y=N, colour=System)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,C), group = Lake2, y=C, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "C% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=reorder(Lake2,N), group = Lake2, y=N, colour=System2)) +                 
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "N% Ordered Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



## Only ACS

new.data.ACS <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee" = "Bay"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=C, colour=Zone)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake, group = Lake, y=N, colour=Zone)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake2, group = Lake2, y=C, colour=Zone2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "H")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.ACS, aes(Lake2, group = Lake2, y=N, colour=Zone2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "H")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay"))

ggplot(new.data.MCS, aes(x=reorder(Lake,C), group = Lake, y=C, colour=Zone)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(x=reorder(Lake,N), group = Lake, y=N, colour=Lake)) +
  geom_boxplot() +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  xlab("")+
  scale_colour_discrete(name="Lakes")+
  labs(title = "MCS Ordered N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=N, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("N%")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS N% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data.MCS, aes(Lake, group = Lake, y=C, colour=Zone)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System) +
  ylab("C%")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "MCS C% Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



ggplot(new.data, aes(x=Lake2, y=C, colour=Zone2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Dry '22 - Zone Carbon Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=Lake2, y=N, colour=Zone2)) +                 
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2) +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Dry '22 - Zone Nitrogen Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#TRY mack nutrient plot-didnt work

glimpse(new.data)

data.sum<-ddply(new.data, c("Lake"), summarise,
                mC = mean(d13C), 
                mN = mean(d15N),
                mCN = mean(C/N))

p1 <- ggplot(new.data, aes(x=Lake2, y=C, group=Lake2, color=System2)) + 
  geom_line(aes(size = Lake2)) +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d(option= "D") +
  labs(title = "Spatiotemporal Variability in McCormick Creek Subestuary N:P Ratios", 
       x = "Lakes", y = "Mean C:N Ratio") +
  scale_x_discrete(expand = c(0,0.1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.25, 0.8))

p1




#### Statistical analyses ####

#Kruskal N15 vs lake

glimpse(new.data)

levels(new.data$Lake2)
new.data$d15N

library(dplyr)
group_by(new.data, Lake2) %>%
  summarise(
    count = n(),
    mean = mean(d15N, na.rm = TRUE),
    sd = sd(d15N, na.rm = TRUE),
    median = median(d15N, na.rm = TRUE),
    IQR = IQR(d15N, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "Lake2", y = "d15N", 
          color = "Lake2",
          
          ylab = "N15", xlab = "Lake")


library("ggpubr")
ggline(new.data, x = "Lake2", y = "d15N", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "N15", xlab = "Lake")

#Kruskal wallis test shows significance

kruskal.test(d15N ~ Lake2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers?

pairwise.wilcox.test(new.data$d15N, new.data$Lake2,
                     p.adjust.method = "BH")


# Kruskal N15 vs system

library(dplyr)
group_by(new.data, System2) %>%
  summarise(
    count = n(),
    mean = mean(d15N, na.rm = TRUE),
    sd = sd(d15N, na.rm = TRUE),
    median = median(d15N, na.rm = TRUE),
    IQR = IQR(d15N, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "System2", y = "d15N", 
          color = "System2",
          
          ylab = "N15", xlab = "System")


library("ggpubr")
ggline(new.data, x = "System2", y = "d15N", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "N15", xlab = "System")

#Kruskal wallis test shows significance

kruskal.test(d15N ~ System2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d15N, new.data$System2,
                     p.adjust.method = "")


#Kruskal C13 vs lake

glimpse(new.data)

levels(new.data$Lake2)
new.data$d13C

library(dplyr)
group_by(new.data, Lake2) %>%
  summarise(
    count = n(),
    mean = mean(d13C, na.rm = TRUE),
    sd = sd(d13C, na.rm = TRUE),
    median = median(d13C, na.rm = TRUE),
    IQR = IQR(d13C, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "Lake2", y = "d13C", 
          color = "Lake2",
          
          ylab = "C13", xlab = "Lake")


library("ggpubr")
ggline(new.data, x = "Lake2", y = "d13C", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "C13", xlab = "Lake")

#Kruskal wallis test shows significance

kruskal.test(d13C ~ Lake2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d13C, new.data$Lake2,
                     p.adjust.method = "BH")


# Kruskal C13 vs system

library(dplyr)
group_by(new.data, System2) %>%
  summarise(
    count = n(),
    mean = mean(d13C, na.rm = TRUE),
    sd = sd(d13C, na.rm = TRUE),
    median = median(d13C, na.rm = TRUE),
    IQR = IQR(d13C, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(new.data, x = "System2", y = "d13C", 
          color = "System2",
          
          ylab = "C13", xlab = "System")


library("ggpubr")
ggline(new.data, x = "System2", y = "d13C", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "C13", xlab = "System")

#Kruskal wallis test shows significance

kruskal.test(d13C ~ System2, data = new.data)

#Mulitple Pairwise comparison shows no lake with significant difference - maybe because too much variability and outliers

pairwise.wilcox.test(new.data$d13C, new.data$System2,
                     p.adjust.method = "BH")




#################################################################################                        
##### Both seasons together####
################################################################################

######Prepare data #######

data<-read_csv("Master_NELP_Data.csv") 

data2<-read_csv("new.data.systems.bays.csv")

data<-read_csv("Master_NELP_Data_w_Exc.csv") 

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh",
                            "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%  
  # mutate(System2= recode(System2, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(System= recode(System, "Garfield Bight"="Alligator Creek System", "Terrapin Bay" = "McCormick Creek System", "Taylor River Bridge"="Taylor Slough Marsh" , "Taylor Slough Marsh"= "Taylor Slough Marsh" ) ) %>%
  mutate(System = fct_relevel(System, "Taylor Slough Marsh", "Alligator Creek System", "McCormick Creek System", )) %>%
  # mutate(System2 = fct_relevel(System2, "Marsh","Alligator Creek System", "McCormick Creek System","Bay")) %>% 
  # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  # mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # filter(Species != "Excrement")%>%
  filter(Lake != "Shark Chickee") %>%
  # filter(Lake != "Cuthbert Rookery") 
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") 
# filter(System2 != "Alligator Creek System") %>% 
# filter(System2 != "McCormick Creek System") %>% 
# filter(System2 != "Bay") %>% 
# filter(System2 != "Chasar")

# data<-read_csv("Merger_temp.csv")
# 
# sulfur<- read_csv("NELP S34 Data.csv")
# 
# data<-merge(data,sulfur,by=c('Site','Season'),all.x=T)
# 
# 
# data
# 
# 
# write.csv(data,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/data/datawwS34.csv", row.names = TRUE)

# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analysis/data/datawcoords.csv")
# write.csv(data,"C:/Users/Joshua Linenfelser/Documents/R/NELP_Real_Analyses/data/datawcoords2.csv", row.names = F)


#View the first 10 rows of data
head(data)
glimpse(data)

##S data test

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh",
                            "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%  
  mutate(System2= recode(System2, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
mutate(System2 = fct_relevel(System2, "Alligator Creek System", "McCormick Creek System", "Marsh", "Bay")) %>% 
  # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # filter(Species != "Excrement")%>% 
  filter(Lake != "Shark Chickee") %>% 
  filter(Lake != "Cuthbert Rookery") %>% 
  filter(Lake != "Cuthbert Groundwater") %>% 
  filter(Lake != "West Lake Bathroom")


#END S data test

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(System2= recode(System2, "Bay"="Florida Bay", "Garfield Bight" = "Florida Bay", "Terrapin Bay"= "Florida Bay", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  filter(Species != "Excrement")%>% 
  filter(Lake != "Shark Chickee")

new.data.ACS<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  filter(Lake %in% c("West Lake", "Cuthbert Lake", 
                                                                     "Long Lake", "The Lungs", "Garfield Bight"))

new.data.extra<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  filter(Lake %in% c("West Lake Bathroom", "Cuthbert Rookery", 
                     "Shark Chickee"))


new.data.MCS<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))%>% 
  filter(System2 %in% c("McCormick Creek System"))

new.data.TS <-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River Bridge", "Taylor Marsh"="Taylor Slough Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))%>% 
  filter(Lake2 %in% c("Taylor River Bridge", "Taylor Slough Marsh"))


# #Data with TS sites separate
# 
# new.data.TS<-  data %>% 
#   mutate(Lake = fct_relevel(Lake, 
#                             "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
#                             "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
#                             "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
#                             "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
#   mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
#   mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
#   mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
#   mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor River Bridge", "Taylor Slough Marsh"="Taylor Slough Marsh")) %>% 
#   mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
#   mutate(d15N = as.double(d15N), N = as.double(N))
# 
# glimpse(new.data)

#Summarize data

data.sum<-ddply(new.data, c( "System2"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N))%>% rename(
                  # Lake = Lake2,
                  Count = count,
                  "Mean C13" = mC,
                  "SD C13" =sdC,
                  "Mean N15" =mN,
                  "SD N15" =sdN
                )

write.csv(data.sum,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/C.N isotope seasonal.spatial summary w exc.csv", row.names = T)

data.sum.ACS<-ddply(new.data.ACS, c("Lake", "Species", "Season"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N))%>% rename(
                  # Lake = Lake2,
                  Count = count,
                  "Mean C13" = mC,
                  "SD C13" =sdC,
                  "Mean N15" =mN,
                  "SD N15" =sdN
                )

data.sum.extra<-ddply(new.data.extra, c("Lake", "Species", "Season"), summarise,
                    count = length(Species),
                    mC = mean(d13C), sdC = sd(d13C), 
                    mN = mean(d15N), sdN = sd(d15N))%>% rename(
                      # Lake = Lake2,
                      Count = count,
                      "Mean C13" = mC,
                      "SD C13" =sdC,
                      "Mean N15" =mN,
                      "SD N15" =sdN )

data.sum.MCS<-ddply(new.data.MCS, c("Lake", "Species", "Season"), summarise,
                    count = length(Species),
                    mC = mean(d13C), sdC = sd(d13C), 
                    mN = mean(d15N), sdN = sd(d15N))%>% rename(
                      # Lake = Lake2,
                      Count = count,
                      "Mean C13" = mC,
                      "SD C13" =sdC,
                      "Mean N15" =mN,
                      "SD N15" =sdN
                    )

data.sum.TS<-ddply(new.data.TS, c("Lake2", "Species", "Season"), summarise,
                    count = length(Species),
                    mC = mean(d13C), sdC = sd(d13C), 
                    mN = mean(d15N), sdN = sd(d15N))%>% rename(
                      # Lake = Lake2,
                      Count = count,
                      "Mean C13" = mC,
                      "SD C13" =sdC,
                      "Mean N15" =mN,
                      "SD N15" =sdN
                    )



####Tables####

#Load your data
data<-read_csv("Master_NELP_Data_WChickee.csv") 
# coords <- read_csv("Sites_Coordinates.csv")

#Merged data to add coordinates and exported for GIS
# 
# data<-merge(data,coords,by=c('Code'),all.x=T)
# 
# data

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh",
                            "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%  
  mutate(System2= recode(System2, "Florida Bay"="Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(Lake= recode(Lake, "Florida Bay"="Bay", "Taylor River"="Marsh" , "Taylor Marsh"= "Marsh" ) ) %>%
  mutate(System= recode(System, "Florida Bay"="Bay", "Garfield Bight"="Alligator Creek System", "Terrapin Bay" = "McCormick Creek System", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(System = fct_relevel(System, "Marsh", "Alligator Creek System", "McCormick Creek System", )) %>%
  mutate(System2 = fct_relevel(System2, "Marsh","Alligator Creek System", "McCormick Creek System","Bay")) %>% 
  # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # filter(Species != "Excrement")%>% 
  filter(Lake != "Shark Chickee") %>% 
  # filter(Lake != "Cuthbert Rookery") %>% 
  filter(Lake != "Cuthbert Groundwater") %>% 
  filter(Lake != "Chasar") %>%
  filter(Lake != "West Lake Bathroom")

##Only for excrement

new.data<-  data %>% 
  # mutate(Lake = fct_relevel(Lake, 
  #                           "Taylor River", "Taylor Marsh",
  #                           "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
  #                           "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%
  # mutate(System2= recode(System2, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  # mutate(Lake2= recode(Lake, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River"="Marsh" , "Taylor Marsh"= "Marsh" ) ) %>%
  # mutate(System= recode(System, "Garfield Bight"="Alligator Creek System", "Terrapin Bay" = "McCormick Creek System", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  # mutate(System = fct_relevel(System, "Marsh", "Alligator Creek System", "McCormick Creek System", )) %>%
  # mutate(System2 = fct_relevel(System2, "Marsh","Alligator Creek System", "McCormick Creek System","Bay")) %>% 
  # # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  # mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # filter(Species != "Excrement")%>% 
  filter(Lake != "Shark Chickee") %>%
  # # filter(Lake != "Cuthbert Rookery") %>% 
  # filter(Lake != "Cuthbert Groundwater") %>% 
  filter(Species == "Excrement")
# filter(Lake != "West Lake Bathroom")


glimpse(new.data)

data.sum<-ddply(new.data, c("Lake","System","Season"), summarise,
                count = length(Species),
                mC = mean(d13C), sdC = sd(d13C), 
                mN = mean(d15N), sdN = sd(d15N),
                mS = mean(d34S, na.rm = TRUE ), sdS = sd(d34S,  na.rm = TRUE))
# %>% rename(
#                   # Lake = Lake2,
#                   Count = count,
#                   "Mean C13" = mC,
#                   "SD C13" =sdC,
#                   "Mean N15" =mN,
#                   "SD N15" =sdN,
#                   "Mean S34" =mS,
#                   "SD S34" =sdS
#                 )


# Table 1 mean and SD for lakes across seasons
#This one adds the Low End and High End from new.data in parentheses

table <- tibble(Lake = data.sum$Lake, Count=data.sum$count, System= data.sum$System2, Season = data.sum$Season,
             "Mean d15N \u00B1 sd" = paste0(format(round(data.sum$mN, digits = 2),nsmall = 2)," ", 
                                                           '\u00B1'," ",format(round(data.sum$sdN, digits = 2),nsmall = 2)),
                                           "Mean d13C \u00B1 sd" = paste0(format(round(data.sum$mC, digits = 2),nsmall = 2)," ", 
                                                                         '\u00B1'," ",format(round(data.sum$sdC, digits = 2),nsmall = 2)),
             "Mean d34S \u00B1 sd" = paste0(format(round(data.sum$mS, digits = 2),nsmall = 2)," ", 
                                            '\u00B1'," ",format(round(data.sum$sdS, digits = 2),nsmall = 2)))

write.csv(table,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/CNS_table2.csv", row.names = TRUE)


# b2 <- a2 %>% pivot_wider(names_from=Count, 
#                        values_from = "Mean N15 \u00B1 sd" | "Mean C13 \u00B1 sd")

library(gt)

mean_table <- table %>% gt()%>%
  tab_header(
    title = md("Seasonal Isotopic Summary"),
    subtitle = "Mean d13C, d15N values and ±SD of all microalgae collected") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything())) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  )

mean_table

mean_table %>% gtsave(filename = "all_table.html", inline_css = TRUE)

# write.csv(mean_table,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/mean_table.csv", row.names = TRUE)


# write.csv(a2,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/data/C.N isotope summary all.csv", row.names = T)


#ACS table

table.acs <- tibble(Lake = data.sum.ACS$Lake, Count=data.sum.ACS$Count, Season = data.sum.ACS$Season,
             "Mean d15N \u00B1 sd" = paste0(format(round(data.sum.ACS$"Mean N15", digits = 2),nsmall = 2)," ", 
                                            '\u00B1'," ",format(round(data.sum.ACS$"SD N15", digits = 2),nsmall = 2)),
             "Mean d13C \u00B1 sd" = paste0(format(round(data.sum.ACS$"Mean C13", digits = 2),nsmall = 2)," ", 
                                            '\u00B1'," ",format(round(data.sum.ACS$"SD C13", digits = 2),nsmall = 2)))

# b2 <- a2 %>% pivot_wider(names_from=Count, 
#                        values_from = "Mean N15 \u00B1 sd" | "Mean C13 \u00B1 sd")

library(gt)

mean_table.acs <- table.acs %>% gt()%>%
  tab_header(
    title = md("Alligator - Seasonal Isotopic Summary"),
    subtitle = "Mean d13C, d15N values and ±SD of microalgae collected in Alligator Creek System") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything())) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  )

mean_table.acs

write.csv(mean_table.acs,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/mean_table.acs.csv", row.names = TRUE)



#Extra sites table

table.extra <- tibble(Lake = data.sum.extra$Lake, Count=data.sum.extra$Count, Season = data.sum.extra$Season,
                    "Mean d15N \u00B1 sd" = paste0(format(round(data.sum.extra$"Mean N15", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.extra$"SD N15", digits = 2),nsmall = 2)),
                    "Mean d13C \u00B1 sd" = paste0(format(round(data.sum.extra$"Mean C13", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.extra$"SD C13", digits = 2),nsmall = 2)))

# b2 <- a2 %>% pivot_wider(names_from=Count, 
#                        values_from = "Mean N15 \u00B1 sd" | "Mean C13 \u00B1 sd")

library(gt)

mean_table.extra <- table.extra %>% gt()%>%
  tab_header(
    title = md("Sites of Interest - Seasonal Isotopic Summary"),
    subtitle = "Mean d13C, d15N values and ±SD of microalgae collected at sites of interest") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything())) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  )

mean_table.extra

write.csv(mean_table.extra,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/mean_table.extra.csv", row.names = TRUE)



#MCS table

table.mcs <- tibble(Lake = data.sum.MCS$Lake, Count=data.sum.MCS$Count, Season = data.sum.MCS$Season,
                    "Mean d15N \u00B1 sd" = paste0(format(round(data.sum.MCS$"Mean N15", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.MCS$"SD N15", digits = 2),nsmall = 2)),
                    "Mean d13C \u00B1 sd" = paste0(format(round(data.sum.MCS$"Mean C13", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.MCS$"SD C13", digits = 2),nsmall = 2)))

# b2 <- a2 %>% pivot_wider(names_from=Count, 
#                        values_from = "Mean N15 \u00B1 sd" | "Mean C13 \u00B1 sd")

library(gt)

mean_table.mcs <- table.mcs %>% gt()%>%
  tab_header(
    title = md("McCormick - Seasonal Isotopic Summary"),
    subtitle = "Mean d13C, d15N values and ±SD of microalgae collected in McCormick Creek System") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything())) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  )

mean_table.mcs

write.csv(mean_table.mcs,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/mean_table.mcs.csv", row.names = TRUE)


#TS_Table

table.ts <- tibble(Lake = data.sum.TS$Lake, Count=data.sum.TS$Count, Season = data.sum.TS$Season,
                    "Mean d15N \u00B1 sd" = paste0(format(round(data.sum.TS$"Mean N15", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.TS$"SD N15", digits = 2),nsmall = 2)),
                    "Mean d13C \u00B1 sd" = paste0(format(round(data.sum.TS$"Mean C13", digits = 2),nsmall = 2)," ", 
                                                   '\u00B1'," ",format(round(data.sum.TS$"SD C13", digits = 2),nsmall = 2)))


table.ts
# b2 <- a2 %>% pivot_wider(names_from=Count, 
#                        values_from = "Mean N15 \u00B1 sd" | "Mean C13 \u00B1 sd")

library(gt)

mean_table.ts <- table.ts %>% gt()%>%
  tab_header(
    title = md("Taylor Slough - Seasonal Isotopic Summary"),
    subtitle = "Mean d13C, d15N values and ±SD of microalgae collected in Taylor Slough") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything())) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  )

mean_table.ts

write.csv(mean_table.ts,"C:/Users/Joshua_Linenfelser/Documents/R/NELP_Real_Analyses/tables/mean_table.ts.csv", row.names = TRUE)




#Plotting data 

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Shark Chickee", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))

# new.data <- na.omit(new.data)

glimpse(new.data)

####Exploratory boxplots ####

##S data test

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh",
                            "West Lake", "Cuthbert Lake", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>%  
  mutate(System2= recode(System2, "Garfield Bight"="Bay", "Terrapin Bay" = "Bay", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(System= recode(System, "Garfield Bight"="Alligator Creek System", "Terrapin Bay" = "McCormick Creek System", "Taylor River Bridge"="Marsh" , "Taylor Slough Marsh"= "Marsh" ) ) %>%
  mutate(System = fct_relevel(System, "Marsh", "Alligator Creek System", "McCormick Creek System", )) %>%
  mutate(System2 = fct_relevel(System2, "Marsh","Alligator Creek System", "McCormick Creek System","Bay")) %>% 
  # mutate(System2= fct_relevel(System2,"Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  # mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  # mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  # mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # filter(Species != "Excrement")%>% 
  filter(Lake != "Shark Chickee") %>% 
  filter(Lake != "Cuthbert Rookery") %>% 
  filter(Lake != "Cuthbert Groundwater") %>% 
  filter(Lake != "West Lake Bathroom")

#S34 differences

ggplot(new.data, aes(x=Lake, y=d34S, colour=Season)) +
  geom_boxplot() +
  # scale_colour_viridis_d(option= "D")+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  # "McCormick Creek System"="blue",
  # "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  facet_wrap(. ~ System, scales = "free_x")+
  # facet_grid(. ~ Lake2, drop=TRUE)+
  ylab(expression(paste(delta^{34}, "S (\u2030)")))+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Sulfur Isotope Boxplots")+
  theme_grey()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#N15 system wide differences across season

ggplot(new.data, aes(x=Lake2, y=d15N, colour=Season)) +
  geom_boxplot() +
  # scale_colour_viridis_d(option= "D")+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                # "McCormick Creek System"="blue",
                                # "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_wrap(. ~ System2, drop = TRUE)+
  # facet_grid(. ~ Lake2, drop=TRUE)+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab("")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Nitrogen Isotope Boxplots")+
  theme_grey()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#C13 system wide differences across season

ggplot(new.data, aes(x=System2, y=d13C, colour=Season)) +
  geom_boxplot() +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  # "McCormick Creek System"="blue",
  # "Taylor Slough"="darkkhaki")) +
  # geom_point(position="jitter")+
  # facet_wrap(. ~ System2, drop = TRUE)+
  # facet_grid(. ~ Lake2, drop=TRUE)+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab("System")+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "Carbon Isotope Boxplots")+
  theme_grey()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



# Difference between systems across zones

ggplot(new.data, aes(x = Zone2, y = d15N, colour= System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0,10)+
  facet_wrap(~Season)+
  # facet_wrap(~System2)+
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS N15 values", x = "Zone") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  theme_dark()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

#C13

ggplot(new.data, aes(x = Zone2, y = d13C, colour= System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # ylim(0,10)+
  facet_wrap(~Season)+
  # facet_wrap(~System2)+
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS C13 values", x = "Zone") +
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_dark()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.9,0.2")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.83, 0.2))


# Difference between lakes and seaons

#Wet season plot


#With bays included in systems- wet
new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"="Florida Bay", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Florida Bay", "McCormick Creek System" , "Alligator Creek System", "Taylor Slough Marsh")) %>% 
  filter(Season %in% c("Wet")) %>%
  filter(Lake2 != "Shark Chickee") %>%
  filter(System2 != "Shark Chickee") %>%
  # filter(System2 != "Florida Bay") %>%
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") %>% 
  na.omit()

#With bays included in systems- dry
new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"="Florida Bay", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Florida Bay", "McCormick Creek System" , "Alligator Creek System", "Taylor Slough Marsh")) %>% 
  filter(Season %in% c("Dry")) %>%
  filter(Lake2 != "Shark Chickee") %>%
  filter(System2 != "Shark Chickee") %>%
  # filter(System2 != "Florida Bay") %>%
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") %>% 
  na.omit()

glimpse(new.data.systems)

#Bays included and only systems and excrement
new.data.onlysystems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Taylor Slough Marsh", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  filter(Season %in% c("Wet")) %>% 
  filter(System2 != "Taylor Slough Marsh")
na.omit()

#With bays not included in systems
new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Bay"="Florida Bay", "Garfield Bight" = "Florida Bay", "Terrapin Bay"= "Florida Bay", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  filter(Season %in% c("Wet")) %>% 
  na.omit()

sample_size = new.data.systems %>% group_by(System2) %>% dplyr::summarize(num=n()) 
#   mutate(num = as.factor(num)) %>% 
#   # mutate(num= recode(num, "16"="15") ) %>% 
#   mutate(num= fct_relevel(num,"9", "13",  "8" ,"16", "37") )
# 
glimpse(sample_size)

## I want to have the n in my plot but it wont let me order it. I took out shark chickee algae values from dataset to make bays and mccormick not
##equal but now i think i should of kept it in the plot because it didnt make a difference in ordering - maybe put chickee sites back in as bay

#####Only N15 plot####

# errbar_lims <- new.data.systems %>%  group_by(System2) %>% 
#   dplyr::summarize(d15N=mean(d15N), se=sd(d15N)/sqrt(n()), 
#             upper=d15N+(2*se), lower=d15N-(2*se))
# 
# p <- ggplot(data=new.data.systems, aes(x=System2, y=d15N))+
# geom_violin() +
#   # geom_point()+
#   # geom_boxplot(width=0.1, color="black", alpha=0.2) +
# geom_errorbar(data=errbar_lims, aes(ymin = lower, ymax = upper) )
# p
# 
# p + geom_violin(aes(x=System2, "")) +    
#   geom_jitter(height = 0, width = 0.1, alpha = 0.05) +
#   geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, 
#                 fatten=2, width=.5) 
  

#With bays included in systems- wet

new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"="Florida Bay", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Florida Bay", "McCormick Creek System" , "Alligator Creek System", "Taylor Slough Marsh")) %>% 
  filter(Season %in% c("Wet")) %>%
  filter(Lake2 != "Shark Chickee") %>%
  filter(System2 != "Shark Chickee") %>%
  # filter(System2 != "Florida Bay") %>%
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") %>% 
  na.omit()

Stats <- new.data.systems %>% mutate(System2= fct_relevel(System2,"Bird Excrement", "Florida Bay", "McCormick Creek System","Alligator Creek System", "Taylor Slough Marsh")) %>% 
  group_by(System2, Species) %>%  
  dplyr::summarize(mean = mean(d15N), SD = sd(d15N), se=sd(d15N)/sqrt(n()),
                                            se_L = mean - (2*se),
                                            se_U = mean + (2*se))

ggplot(new.data.systems, aes(x=System2, y = d15N, fill=Species))+ 
  geom_violin( mapping = aes(System2, d15N), scale="width")+
  geom_point(mapping = aes(System2, mean), size=2, data = Stats)+
  geom_errorbar(mapping = aes(System2, mean, ymin = se_L, ymax = se_U), 
                              data = Stats, width = 0.2)+
  # geom_boxplot(width=0.1, color="black", alpha=0.2) +
  coord_flip()+
  ylim(-5,20)+
  # ylim(-5,20)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("Microalgae" = "lightgreen",
                                "Excrement"="Brown"))+
  # scale_fill_manual(values = c("Alligator Creek System" = "#299408",
  #                               "McCormick Creek System"="#827e1e",
  #                               "Taylor Slough Marsh"="#ffbe7a",
  #                               "Florida Bay"= "blue",
  #                               "Bird Excrement"= "brown")) +
  # scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  # scale_colour_viridis_d()+
  labs(title = "2021 Wet Season N15 Values", x = "") +
  theme_bw()+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) 
  # theme(legend.position = "0.2,0.5")+
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  # theme(legend.position = c(0.7, 0.8))

## Dry season plot

#With bays included in systems- dry

new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"="Florida Bay", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Florida Bay", "McCormick Creek System" , "Alligator Creek System", "Taylor Slough Marsh")) %>% 
  filter(Season %in% c("Dry")) %>%
  filter(Lake2 != "Shark Chickee") %>%
  filter(System2 != "Shark Chickee") %>%
  # filter(System2 != "Florida Bay") %>%
  filter(Lake != "Cuthbert Groundwater") %>%
  filter(Lake != "West Lake Bathroom") %>% 
  na.omit()

Stats <- new.data.systems %>% mutate(System2= fct_relevel(System2,"Bird Excrement", "Florida Bay", "McCormick Creek System","Alligator Creek System", "Taylor Slough Marsh")) %>% 
  group_by(System2, Species) %>%  
  dplyr::summarize(mean = mean(d15N), SD = sd(d15N),se=sd(d15N)/sqrt(n()),
                   se_L = mean - (2*se),
                   se_U = mean + (2*se))

ggplot(new.data.systems, aes(x=System2, y = d15N, fill=Species))+ 
  geom_violin( mapping = aes(System2, d15N), scale="width")+
  geom_point(mapping = aes(System2, mean), size=2, data = Stats)+
  geom_errorbar(mapping = aes(System2, mean, ymin = se_L, ymax = se_U), 
                data = Stats, width = 0.2)+
  # geom_boxplot(width=0.1, color="black", alpha=0.2) +
  coord_flip()+
  ylim(-5,20)+
  # ylim(-5,20)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("Microalgae" = "lightgreen",
                               # "McCormick Creek System"="blue",
                               # "Taylor Slough Marsh"="darkkhaki",
                               # "Florida Bay"= "lightblue",
                               "Excrement"= "brown")) +
  # scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  # scale_colour_viridis_d()+
  labs(title = "2022 Dry Season N15 Values", x = "") +
  theme_bw()+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) 
# theme(legend.position = "0.2,0.5")+
# theme(legend.title = element_blank()) +
# theme(legend.text = element_text(size=10, face="bold", color = "black")) +
# theme(legend.position = c(0.7, 0.8))
  
  
  ggplot(new.data.systems, aes(x=System2, y = d15N, colour=System2)) +
  geom_violin() +
  geom_point()+
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  coord_flip()+
  ylim(-5,20)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                               
                                "Bird Excrement"= "brown")) +
  # scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  # scale_colour_viridis_d()+
  labs(title = "2021 Wet Season N15 Values", x = "") +
  theme_grey()+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black"))
  # theme(legend.position = "0.2,0.5")+
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  # theme(legend.position = c(0.7, 0.8))


##Dry season plot

#Bays included and only systems and excrement
new.data.onlysystems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"= "Alligator Creek System", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Taylor Slough Marsh", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  filter(Season %in% c("Dry")) %>% 
  filter(System2 != "Taylor Slough Marsh")
  na.omit()
  

#With bays included in systems
new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
    mutate(System2= recode(System2, "Garfield Bight" = "Alligator Creek System", "Bay"= "Alligator Creek System", "Terrapin Bay"= "McCormick Creek System", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
    mutate(System2= fct_relevel(System2,"Bird Excrement","Taylor Slough Marsh", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
    filter(Season %in% c("Dry")) %>% 
  na.omit()

#With bays not included in systems
new.data.systems<-read_csv("new.data.systems.bays.csv") %>%
  mutate(System2= recode(System2, "Bay"="Florida Bay", "Garfield Bight" = "Florida Bay", "Terrapin Bay"= "Florida Bay", "Shark Chickee" = "Bird Excrement", "Cuthbert Rookery"= "Bird Excrement", "Taylor River Bridge"= "Taylor Slough Marsh") ) %>% 
  mutate(System2= fct_relevel(System2,"Bird Excrement","Taylor Slough Marsh", "Florida Bay", "McCormick Creek System" ,"Alligator Creek System" )) %>% 
  filter(Season %in% c("Dry")) %>% 
  na.omit()


sample_size = new.data.systems %>% group_by(System2) %>% dplyr::summarize(num=n()) 
#   mutate(num = as.factor(num)) %>% 
#   # mutate(num= recode(num, "16"="15") ) %>% 
#   mutate(num= fct_relevel(num,"9", "13",  "8" ,"16", "37") )
# 
glimpse(sample_size)

## I want to have the n in my plot but it wont let me order it. I took out shark chickee algae values from dataset to make bays and mccormick not
##equal but now i think i should of kept it in the plot because it didnt make a difference in ordering - maybe put chickee sites back in as bay


ggplot(new.data.systems, aes(x=System2, y = d15N, colour=System2)) +
  geom_violin() +
  geom_point()+
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  coord_flip()+
  ylim(-5,20)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough Marsh"="darkkhaki",
                                "Florida Bay"= "lightblue",
                                "Bird Excrement"= "brown")) +
  # scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  # scale_colour_viridis_d()+
  labs(title = "2022 Dry Season N15 Values", x = "") +
  theme_grey()+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) 
  # theme(legend.position = "0.2,0.5")+
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  # theme(legend.position = c(0.7, 0.8))


ggplot(new.data.onlysystems, aes(x=System2, y = d15N, colour=System2)) +
  geom_violin() +
  geom_point()+
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  coord_flip()+
  ylim(-5,20)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Bird Excrement"= "brown")) +
  # scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  # scale_colour_viridis_d()+
  labs(title = "2022 Dry Season N15 Values", x = "") +
  theme_grey()+
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) 
# theme(legend.position = "0.2,0.5")+
# theme(legend.title = element_blank()) +
# theme(legend.text = element_text(size=10, face="bold", color = "black")) +
# theme(legend.position = c(0.7, 0.8))


?geom_bar#C13

ggplot(new.data, aes(x = Lake, y = d13C, fill= Season)) +
  geom_boxplot() +
  # ylim(0,10)+
  # facet_wrap(~System2)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  labs(title = "ACS and MCS Lakes across Seasons - C13 Values", x = "Lake") +
  theme_grey()+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#Only ACS seasonal differences


new.data.ACS <- subset(new.data, System2 == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Shark Chickee"))

#ACS zones by season

ggplot(new.data.ACS, aes(x = Zone2, y = d15N, colour= Season)) +
  geom_boxplot() +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS N15 values", x = "Zone") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

ggplot(new.data.ACS, aes(x = Zone2, y = d13C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS C13 values", x = "Zone") +
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#ACS lakes by season

ggplot(new.data.ACS, aes(x = Lake, y = d15N, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS N15 values", x = "Zone") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

ggplot(new.data.ACS, aes(x = Lake, y = d13C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS C13 values", x = "Zone") +
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay"))


#MCS lakes by zone and season

ggplot(new.data.MCS, aes(x = Lake, y = d15N, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  ylim(0,10)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in MCS N15 values", x = "Zone") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


ggplot(new.data.MCS, aes(x = Lake, y = d13C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,10)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in MCS C13 values", x = "Zone") +
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.3, 0.8))



##### Biplots isotopes - both seasons ####

## All Biplot against season

ggplot(new.data, aes(x = d13C, y = d15N, colour = System2)) +
  geom_point(alpha = 1.0, size=3.0) +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_colour_viridis_d(option= "D")+
  geom_mark_ellipse(alpha = 0.3, size=1.0) +
  facet_grid( ) +
  ylim(-1,7)+
  xlim(-35,-12)+
  # xlim(0,-25)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Both Seasons - Systems Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))






ggplot(new.data, aes(x = d13C, y = d15N, colour = Season)) +
  geom_point(alpha = 1.0, size=3.0) +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_colour_viridis_d(option= "D")+
  geom_mark_ellipse(alpha = 0.3, size=1.0) +
  facet_grid( ) +
  ylim(-1,7)+
  xlim(-35,-12)+
  # xlim(0,-25)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Both Seasons Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


##Lakes plot seperate


ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_point(alpha = 1.0, size=3.0) +
  # scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  scale_colour_viridis_d(option= "D")+
  # geom_mark_ellipse(alpha = 0.3, size=1.0) +
  facet_grid( ) +
  ylim(-1,7)+
  xlim(-35,-12)+
  # xlim(0,-25)+
  theme_gray() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = System2,
  #                                colour = System2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Both Seasons Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

#ACS biplot

new.data.ACS <- subset(new.data, System == "Alligator Creek System")

ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Season)) +
  geom_point(alpha = 1.0, size=3.0) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  scale_color_manual(values = c("Wet" = "cadetblue",
                                                             "Dry"="darkgoldenrod")) +
  ylim(-0.5,8)+
  xlim(-28, -13)+
  # facet_grid(. ~ System) +
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  # scale_colour_discrete(name="Lakes")+
  labs(title = "ACS Lakes Seasonal Biplot")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))



# ?geom_ellipse

#ACS OnlyLakes - dont know how to subset multiple variables

biplot.ACS<-ggplot(new.data.ACS, aes(x = d13C, y = d15N, colour = Season)) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  geom_point(alpha = 1.0, size=3.0) +
  ylim(0,6)+
  xlim(-27, -15)+
  # facet_grid(. ~ System) +
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #              aes(fill = Lake2),
  #              alpha = 0.25)+
  # ggforce::geom_mark_ellipse(aes(fill = Lake2,
  #                                color = Lake2)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "ACS Lakes Biplot", legend= "Lakes")+
  # scale_colour_discrete(name="Lakes")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplot.ACS


#MCS biplot

new.data.MCS <- subset(new.data, System == "McCormick Creek System")

glimpse(new.data.MCS)

ggplot(new.data.MCS, aes(x = d13C, y = d15N, colour = Season)) +
  geom_mark_ellipse(alpha = 0.2, size=1) +
  geom_point(alpha = 0.7, size=3.5) +
  scale_color_manual(values = c("Wet" = "cadetblue",
                                "Dry"="darkgoldenrod")) +
  ylim(-0.5,8)+
  xlim(-28, -13)+
  # facet_grid(. ~ Species) +
  # stat_ellipse() +
  # ggforce::geom_mark_ellipse(aes(fill = Lake,
  #                                color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "MCS Lakes Seasonal Biplot")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


#Looking between systems


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake)) +
  geom_point(alpha = 1.0, size=3) +
  facet_grid(. ~ System2) +
  # ylim(0,5.5)+
  # xlim(-35,-10)+
  theme_bw() +
  # stat_ellipse(geom = "polygon",
  #                           aes(fill = Lake2),
  #                           alpha = 0.25)+
  ggforce::geom_mark_ellipse(aes(fill = Lake,
                                 color = Lake)) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "Isotope Ellipse Biplot by System")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots

# check by systems

ggplot(new.data, aes(x=d13C, y=d15N, colour=Lake, shape=Season)) + 
  geom_point(size=3) + 
  # scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  scale_colour_viridis_d(option= "D")+
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ System2)+
  theme_gray()+
  labs(title = "Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


####Final summarize####

# new.data.systems<-read_csv("new.data.systems.bays.csv") %>% 
#   mutate(System2= fct_relevel(System2,"Taylor River Bridge" , "Taylor Slough Marsh", "Alligator Creek System", "McCormick Creek System", "Garfield Bight", "Terrapin Bay", "Cuthbert Rookery", "Shark Chickee")) 

new.data<- new.data %>% 
  mutate(System= recode(System2, "Garfield Bight"="Florida Bay", "Terrapin Bay" = "Florida Bay", "Taylor River Bridge"="Taylor Slough Marsh"))

data.sum<-ddply(new.data, c( "Season", "System"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d15Nse=sd(d15N)/sqrt(length(d15N)),
                d34Smn=mean(d34S,  na.rm = TRUE),
                d34Ssd=sd(d34S,  na.rm = TRUE),
                d34Sse=sd(d34S,  na.rm = TRUE)/sqrt(length(d15N)))
                
#C&S
# Ylims <- aes(ymax = d15Nmn + d15Nse, ymin=d15Nmn - d15Nse)
Ylims <- aes(ymax = d34Smn + d34Sse, ymin=d34Smn - d34Sse)
Xlims <- aes(xmax = d13Cmn + d13Cse, xmin=d13Cmn - d13Cse)


#C & N
Ylims <- aes(ymax = d15Nmn + d15Nse, ymin=d15Nmn - d15Nse)
Xlims <- aes(xmax = d13Cmn + d13Cse, xmin=d13Cmn - d13Cse)

# glimpse(new.data)


# check<- new.data %>% 
#   ungroup() %>% 
#   group_by(System2, Lake2) %>% 
#   summarise(d13Cmn=mean(d13C), #mean
#              d13Csd=sd(d13C),#standard deviation
#              d13Cse=sd(d13C)/sqrt(length(d13C)),#standard error
#              d15Nmn=mean(d15N),
#              d15Nsd=sd(d15N),
#              d13Cse=sd(d15N)/sqrt(length(d15N)),
#             rangeN=range(d15N),
#             rangeC=range(d13C))



#Biplot across systems

ggplot(data.sum, aes(x=d13Cmn, y=d34Smn, shape=Season, colour=System)) + 
  geom_point(size=4) + 
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                             "McCormick Creek System"="blue",
  #                                                             "Taylor River Bridge"="darkkhaki",
  #                                                                "Marsh"="darkgoldenrod3",
  #                                                                "Bay"="darkslategray3")) +
  geom_errorbar(Ylims, width=0.5) + 
  geom_errorbarh(Xlims, height=0.1) +
  ylab(expression(delta^{34}~S)) +
  xlab(expression(delta^{13}~C)) +
  # facet_wrap("System2")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                              "McCormick Creek System"="blue",
  #                                                              "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "C & S Isotope Biplot")+
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="#c1bc2c",
                                # "Taylor River Bridge"="darkkhaki",
                                "Taylor Slough Marsh"="#ffbe7a",
                                "Florida Bay"="blue"))+
  # "Terrapin Bay"="darkslategray3",
  # "Garfield Bight"="lightgreen",
  # "Shark Chickee"= "coral3",
  # "Cuthbert Rookery"="brown")) +
  # scale_colour_discrete(name="Lakes")+
  theme_grey()+
  theme(
    # axis.title = element_text(size = 14),
        # axis.text = element_text(size = 1, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=14, hjust=0.5, face="bold"))


#Biplot across Systems

ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, shape=Season, colour=System)) + 
  geom_point(size=3) + 
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="#c1bc2c",
                                # "Taylor River Bridge"="darkkhaki",
                                "Taylor Slough Marsh"="#ffbe7a",
                                "Florida Bay"="blue"))+
                                # "Terrapin Bay"="darkslategray3",
                                # "Garfield Bight"="lightgreen",
                                # "Shark Chickee"= "coral3",
                                # "Cuthbert Rookery"="brown")) +
  # scale_alpha_manual(values = c("Wet"=0, "Dry"=1))+
  geom_errorbar(Ylims, width=0.4) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  # facet_wrap("System2")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                              "McCormick Creek System"="blue",
  #                                                              "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "C & N Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_grey()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, shape=Season, colour=Lake2)) + 
  geom_point(size=3) + 
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                             "McCormick Creek System"="blue",
  #                                                             "Taylor Slough"="darkkhaki")) +
  geom_errorbar(Ylims, width=0.6) + 
  geom_errorbarh(Xlims, height=0.3) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "Summarized Lakes Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


ggplot(data.sum, aes(x=Lake2, y=Ylims, colour=Lake2)) + 
  geom_bar(size=3) +
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                             "McCormick Creek System"="blue",
  #                                                             "Taylor Slough"="darkkhaki")) +
  geom_errorbar(Ylims, width=0.6) + 
  geom_errorbarh(Xlims, height=0.3) +
  ylab(expression(delta^{15}~N)) +
  # xlab(expression(delta^{13}~C)) +
  scale_colour_viridis_d(option= "D")+
  # scale_colour_brewer(palette = "Spectral" )+
  # facet_grid(. ~ Species)+
  labs(title = "MCS Summarized Lakes Isotope Biplot")+
  # scale_colour_discrete(name="Lakes")+
  theme_dark()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# theme_bw()

?viridis

library(RColorBrewer)


#Visualize the difference --


#plotting data to see differences across species groups


biplots<-ggplot(new.data, aes(x = d13C, y = d15N, colour = Lake2)) +
  geom_point(alpha = 0.9, size=3) +
  # facet_grid(. ~ Season) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  labs(title = "MCS and ACS Lakes Isotope Biplot")+
  scale_colour_discrete(name="Lakes")+
  theme_gray()+
  ggforce::geom_mark_ellipse(aes(fill = Lake2,
                                 color = Lake2)) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))
biplots


# ggsave("biplots.pdf")

#Final summary

data.sum<-ddply(new.data, c("Season", "Species", "System2", "Lake", "Season", "Zone2"), summarise,
                d13Cmn=mean(d13C), #mean
                d13Csd=sd(d13C),#standard deviation
                d13Cse=sd(d13C)/sqrt(length(d13C)), #standard error
                d15Nmn=mean(d15N),
                d15Nsd=sd(d15N),
                d13Cse=sd(d15N)/sqrt(length(d15N)))

glimpse(data.sum)

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)


ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Zone2, shape= Season )) + 
  geom_point(size=3) + 
  scale_colour_viridis_d(option= "D")+
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  facet_wrap(. ~ Lake)+
  theme_dark()+
  labs(title = "Seasonal All Coastal Lakes Isotope Biplots")+
  # scale_colour_discrete(name="Lakes")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


##### Exploratory plots nutrients #######

glimpse(new.data)

system_comparisons <- list(c("Alligator Creek System", "McCormick Creek System"), c("Alligator Creek System", "Taylor Slough"), c("McCormick Creek System", "Taylor Slough"))

ggplot(new.data, aes(x=System2, y=N, colour=Season)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                                                                                            "McCormick Creek System"="blue",
  #                                                                                            "Taylor Slough"="darkkhaki")) +
  # scale_colour_viridis_d(option= "D")+
  # stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 4.5)  +
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("N%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Nitrogen Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

ggplot(new.data, aes(x=System2, y=C, colour=Season)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # stat_compare_means(comparisons = system_comparisons)+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 48)  +
  # scale_colour_viridis_d(option= "D")+
  # geom_point(position="jitter")+
  # facet_grid(. ~ System2)+
  ylab("C%")+
  xlab("")+
  # scale_colour_discrete(name="Systems")+
  labs(title = "Carbon Boxplots")+
  theme_gray()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))

# ggplot(new.data, aes(x="", group = Species, y=N, colour=System)) +                 
#   geom_boxplot() +
#   # geom_point(position="jitter")+
#   facet_grid(. ~ System)+
#   ylab("N%")+
#   xlab("")


# Difference between systems across zones

ggplot(new.data, aes(x = Zone2, y = N, colour= System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # ylim(0,5)+
  facet_wrap(~Season)+
  # facet_wrap(~System2)+
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS N values", x = "Zone") +
  ylab("N%") +
  theme_dark()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.4, 0.8))

#C13

ggplot(new.data, aes(x = Zone2, y = C, colour= System2)) +
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # ylim(0,10)+
  facet_wrap(~Season)+
  # facet_wrap(~System2)+
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS C values", x = "Zone") +
  ylab("C%") +
  theme_dark()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.9,0.2")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.4, 0.8))


# Difference between lakes and seaons

ggplot(new.data, aes(x = Lake, y = N, fill= Season)) +
  geom_boxplot() +
  # ylim(0,10)+
  # facet_wrap(~System2)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  labs(title = "ACS and MCS Lakes across Seasons - N Values", x = "Lake") +
  theme_grey()+
  ylab("N%")+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

#C13

ggplot(new.data, aes(x = Lake2, y = C, fill= Season)) +
  geom_boxplot() +
  # ylim(0,10)+
  # facet_wrap(~System2)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  labs(title = "ACS and MCS Lakes across Seasons - C Values", x = "Lake") +
  theme_grey()+
  ylab("C%")+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#Only ACS seasonal differences


new.data.ACS <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight"))

#ACS zones by season

ggplot(new.data.ACS, aes(x = Zone2, y = N, colour= Season)) +
  geom_boxplot() +
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS N values", x = "Zone") +
  ylab("N%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

ggplot(new.data.ACS, aes(x = Zone2, y = C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS C values", x = "Zone") +
  ylab("C%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#ACS lakes by season

ggplot(new.data.ACS, aes(x = Lake, y = N, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS N values", x = "Zone") +
  ylab("N%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

ggplot(new.data.ACS, aes(x = Lake, y = C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,6)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS C13 values", x = "Zone") +
  ylab("C%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


## Only MCS

new.data.MCS <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay"))


#MCS lakes by zone and season

ggplot(new.data.MCS, aes(x = Zone, y = N, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,10)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in MCS N values", x = "Zone") +
  ylab("N%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.4, 0.8))


ggplot(new.data.MCS, aes(x = Zone, y = C, colour= Season)) +
  geom_boxplot() +
  scale_colour_viridis_d(option= "D")+
  # scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
  #                               "McCormick Creek System"="blue",
  #                               "Taylor Slough"="darkkhaki")) +
  # ylim(0,10)+
  # facet_wrap(~Season)+
  # facet_wrap(~System2)+
  scale_color_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in MCS C values", x = "Zone") +
  ylab("C%") +
  theme_grey()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.2, 0.87))




####Seasonal, Lake, system stats####

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>%
  mutate(d15N = as.double(d15N), N = as.double(N))


OnlyLakes <-new.data %>% 
  filter(System2!= "Taylor Slough") %>% 
  droplevels()

summary(OnlyLakes)
OnlyLakes %>% 
  with(.,table(Lake2))

#N15 stats

res.aov <- aov(d15N ~ System2 + Season + Lake2, data = OnlyLakes)

summary(res.aov)


TukeyHSD(res.aov)



#C13 stats

res.aov <- aov(d13C ~ System2 + Season + Lake2, data = OnlyLakes)

summary(res.aov)


TukeyHSD(res.aov)



##########################################################
###### Biomass ######
#########################################################


new.data %>%
  rename("Biomass" <- "Biomass (g)")

data.sum<-ddply(new.data, c("Biomass", "Lake"), summarise,
                count = length(Lake),
                mC = mean(Biomass), sdC = sd(Biomass))

glimpse(new.data)

## Wet season biomass

wet_data<-  subset(data, Season == "Wet") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))

#Ordered plot

ggplot(wet_data, aes(x=reorder(Lake,Biomass), group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Wet '21 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())

#Not ordered plot

ggplot(wet_data, aes(x=Lake, group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Wet '21 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())


#Trying to figure out how to do p-value comparisons in box plot

my_comparisons <- list( c("West Lake", "WL Bathroom"), c("West Lake", "Cuthbert"), c("WL Bathroom", "") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50) 

summary(new.data)

wilcox.test(Biomass ~ Lake, new.data=new.data)



##### Separate Wet and Dry season biomass####

dry_data<-  subset(data, Season == "Dry") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Shark Chickee", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))

# dry_data<-read_csv("Biomass_Dry_lakes.csv")

dry_data
str(dry_data)
head(dry_data)
glimpse(dry_data)

# dry_data %>%
#   rename(Biomass <- Biomass (g))

dry_data.sum<-ddply(dry_data, c("Lake", "Species", "Season"), summarise,
                    count = length(Lake),
                    mBio = mean(Biomass), sdBio = sd(Biomass))%>% rename(
                      Count = count,
                      Mean.Biomass = mBio,
                      SD.Biomass=sdBio,
                    )


mean_table_Lake <- dry_data.sum %>% gt()%>%
  tab_header(
    title = md("Wet '21 & Dry'22 - Biomass Summary"),
    subtitle = "Biomass taken from microalgae grown over a 3-week period in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))

mean_table_Lake

##

dry_data.sum<-ddply(dry_data, c("System", "Species", "Season"), summarise,
                    count = length(Lake),
                    mBio = mean(Biomass), sdBio = sd(Biomass))%>% rename(
  Count = count,
  Mean.Biomass = mBio,
  SD.Biomass=sdBio,
)

mean_table_system <- dry_data.sum %>% gt()%>%
  tab_header(
    title = md("Wet '21 & Dry'22 - Biomass Summary"),
    subtitle = "Biomass taken from microalgae grown over a 3-week period in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))

mean_table_system

#ordered plot

ggplot(dry_data, aes(x=reorder(Lake,Biomass), group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Dry '22 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())

#not ordered plot

ggplot(dry_data, aes(x= Lake, group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Dry '22 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())


#### Wet season ####

wet_data<-  subset(data, Season == "Wet") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Shark Chickee", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))

# wet_data<-read_csv("Biomass_wet_lakes.csv")

wet_data
str(wet_data)
head(wet_data)
glimpse(wet_data)

# wet_data %>%
#   rename(Biomass <- Biomass (g))

wet_data.sum<-ddply(wet_data, c("Lake", "Species", "Season"), summarise,
                    count = length(Lake),
                    mBio = mean(Biomass), sdBio = sd(Biomass))%>% rename(
                      Count = count,
                      Mean.Biomass = mBio,
                      SD.Biomass=sdBio,
                    )


mean_table_Lake <- wet_data.sum %>% gt()%>%
  tab_header(
    title = md("Wet '21 & Dry'22 - Biomass Summary"),
    subtitle = "Biomass taken from microalgae grown over a 3-week period in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))

mean_table_Lake

##

wet_data.sum<-ddply(wet_data, c("System", "Species", "Season"), summarise,
                    count = length(Lake),
                    mBio = mean(Biomass), sdBio = sd(Biomass))%>% rename(
                      Count = count,
                      Mean.Biomass = mBio,
                      SD.Biomass=sdBio,
                    )

mean_table_system <- wet_data.sum %>% gt()%>%
  tab_header(
    title = md("Wet '21 & wet'22 - Biomass Summary"),
    subtitle = "Biomass taken from microalgae grown over a 3-week period in the FCE Coastal Lakes") %>% 
  # tab_source_note(md("More information is available at `?exibble`."))
  tab_style(style = cell_borders(weight = px(1.5), style = 'solid',
                                 sides = c('top','bottom')),
            locations = cells_body(
              columns = everything(),
              rows = everything()))

mean_table_system

#ordered plot

ggplot(wet_data, aes(x=reorder(Lake,Biomass), group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Wet '21 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())

#not ordered plot

ggplot(wet_data, aes(x= Lake, group = Lake, y=Biomass, colour=System2)) +                 
  geom_boxplot() +
  scale_color_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0, 10)+
  geom_point(position="jitter")+
  labs(title = "Wet '21 Biomass", x = 'Lake', y = 'Algae Biomass (g)')+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank())

#Trying to figure out how to do p-value comparisons in box plot

my_comparisons <- list( c("West Lake", "WL Bathroom"), c("West Lake", "Cuthbert"), c("WL Bathroom", "") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50) 

summary(wet_data)

wilcox.test(Biomass ~ Lake, data=data)




##### Wet vs Dry Biomass and between systems and zone ####

# biomass_data<-read_csv("Biomass_Wet_Dry_lakes.csv") 
# 
# glimpse(biomass_data)

new.data<-  data %>% 
  mutate(Lake = fct_relevel(Lake,
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom",
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery",
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>%
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="Taylor Slough", "Taylor Slough Marsh"="Taylor Slough")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N))

#System wide differences

ggplot(new.data, aes(x = System2, y = Biomass, fill= System2)) +
  geom_boxplot() +
  ylim(0,10)+
  scale_fill_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  # scale_fill_manual(values = c("cadetblue","darkgoldenrod" , "cadetblue","darkgoldenrod" )) +
  labs(title = "Variability in ACS and MCS Algae Biomass (2021-2022)", x = "Season", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))



#Seasonal system wide differences

ggplot(new.data, aes(x = System2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Season)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("cadetblue","darkgoldenrod" , "cadetblue","darkgoldenrod" )) +
  labs(title = "Seasonal Variability in ACS and MCS Algae Biomass (2021-2022)", x = "Season", 
       y = "Biomass (g)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

# Difference between zones

new.zones<-  data %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Taylor River", "Taylor Marsh", "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight", "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) %>% 
  mutate(Season = fct_relevel(Season, "Wet", "Dry")) %>% 
  mutate(Zone=fct_relevel(Zone, "Taylor River Bridge", "Taylor Slough Marsh", "Upstream", "Middle", "Downstream", "Shark Chickee")) %>% 
  mutate(Lake2= recode(Lake, "Cuthbert Rookery" = "Cuthbert Lake", "Cuthbert Groundwater"= "Cuthbert Lake", "West Lake Bathroom" = "West Lake", "Shark Chickee"= "Garfield Bight", "Taylor River"= "Taylor River", "Taylor Marsh"="Taylor Marsh") ) %>% 
  mutate(System2= recode(System, "Shark Chickee"="Alligator Creek System", "Taylor River Bridge"="", "Taylor Slough Marsh"="")) %>% 
  mutate(Zone2= recode(Zone, "Shark Chickee"="Bay", "Taylor River Bridge"="", "Taylor Slough Marsh"="")) %>% 
  mutate(d15N = as.double(d15N), N = as.double(N)) %>% 
  # subset(Zone2, select = -c("Taylor Slough"))

# subset(data, Season == "Wet")


ggplot(new.data, aes(x = Zone2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  facet_wrap(~System2)+
  scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


# Difference between zones and systems

ggplot(new.data, aes(x = Zone2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  facet_wrap(~System2)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

#Seasonal differences between lakes

ggplot(new.data, aes(x = Lake, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~System2)+
  # facet_wrap(~System)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod" )) +
  # scale_color_manual(values = c("wet" = "cadetblue",
  #                               "dry"="darkgoldenrod")) +
  labs(title = "ACS and MCS Lakes across Seasons - Biomass (g)", x = "Lake") +
  theme_grey()+
  ylab("Biomass (g)")+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=14,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#Switched and better

ggplot(new.data, aes(x = Zone2, y = Biomass, fill= System2)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Alligator Creek System" = "chartreuse3",
                                "McCormick Creek System"="blue",
                                "Taylor Slough"="darkkhaki")) +
  ylim(0,10)+
  facet_wrap(~Season)+
  # facet_wrap(~System)+
  # scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in ACS and MCS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


## Only ACS seasonal biomass differences

new.data.AC <- subset(new.data, System == "Alligator Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "West Lake Bathroom", 
                            "West Lake", "Cuthbert Lake", "Cuthbert Rookery", 
                            "Cuthbert Groundwater", "Long Lake", "The Lungs", "Garfield Bight"))

#Zone across season

ggplot(new.data.AC, aes(x = Zone2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod")) +
  labs(title = "Spatiotemporal Variability in ACS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

#Zone across lake

ggplot(new.data.AC, aes(x = Lake2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod")) +
  labs(title = "Spatiotemporal Variability in ACS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#Only MCS seasonal biomass changes

new.data.MC <- subset(new.data, System == "McCormick Creek System") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) 


#Zone across season

ggplot(new.data.MC, aes(x = Zone2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod")) +
  labs(title = "Spatiotemporal Variability in MCS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

#Zone across lake

ggplot(new.data.MC, aes(x = Lake2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("cadetblue", "darkgoldenrod", "cadetblue", "darkgoldenrod")) +
  labs(title = "Spatiotemporal Variability in MCS Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))


#Only Taylor slough biomass differences

new.data.TS <- subset(new.data, System2 == "Taylor Slough") %>% 
  mutate(Lake = fct_relevel(Lake, 
                            "Seven Palm",
                            "Middle Lake", "Monroe Lake", "Terrapin Bay")) 

# #Zone across season
# 
# ggplot(new.data.TS, aes(x = Zone2, y = Biomass, fill= Season)) +
#   geom_boxplot() +
#   ylim(0,10)+
#   # facet_wrap(~Zone2)+
#   # facet_wrap(~System2)+
#   scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
#   labs(title = "Spatiotemporal Variability in ACS Algae Biomass (2021-2022)", x = "Zone", 
#        y = "Biomass (g)") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=14, face="bold", color = "black")) +
#   theme(axis.text = element_text(size=12,face="bold", color = "black")) +
#   theme(axis.title = element_text(size=12,face="bold", color = "black")) +
#   theme(legend.position = "0.5,0.5")+
#   theme(legend.title = element_blank()) +
#   theme(legend.text = element_text(size=10, face="bold", color = "black")) +
#   theme(legend.position = c(0.5, 0.8))

#Zone across lake

ggplot(new.data.TS, aes(x = Lake2, y = Biomass, fill= Season)) +
  geom_boxplot() +
  ylim(0,10)+
  # facet_wrap(~Zone2)+
  # facet_wrap(~System2)+
  scale_fill_manual(values = c("darkgoldenrod", "cadetblue", "darkgoldenrod", "cadetblue")) +
  labs(title = "Spatiotemporal Variability in Taylor Slough Algae Biomass (2021-2022)", x = "Zone", 
       y = "Biomass (g)") +
  theme_gray()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=14, face="bold", color = "black")) +
  theme(axis.text = element_text(size=12,face="bold", color = "black")) +
  theme(axis.title = element_text(size=12,face="bold", color = "black")) +
  theme(legend.position = "0.5,0.5")+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10, face="bold", color = "black")) +
  theme(legend.position = c(0.5, 0.8))

