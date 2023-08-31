####Linear regression with C13 and salinity###
##Joshua Linenfelser
##


install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)


setwd("C:/Users/Joshua_Linenfelser/Documents/R/Ryan_MixingModels_practice/data")

# load the wet mix data

mix.file.wet= read.csv("mix.sum.wet.wbay.wsalinity.csv") #Filtered mix.sum for wet season
glimpse(mix.file.wet)


summary(mix.file.wet)

hist(mix.file.wet$Salinity)

hist(mix.file.wet$d13C)

plot(d13C ~ Salinity, data = mix.file.wet)


salinity.d13C.lm <- lm(d13C ~ Salinity, data = mix.file.wet)

salinity.d13C.lm

summary(salinity.d13C.lm)


salinity.d13c.graph<-ggplot(salinity.d13C.lm, aes(x=Salinity, y=d13C))+
  geom_point() +
  geom_smooth(method="lm", col="black")+
  theme_bw() +
  labs(title = "ECLS Salinity vs. d13C - Wet Season",
       x = "Salinity",
       y = "d13C")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 9, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


salinity.d13c.graph


# load the dry mix data

mix.file.dry= read_csv("mix.sum.dry.wbay.wsalinity.csv")
glimpse(mix.file.dry)


summary(mix.file.dry)

hist(mix.file.dry$Salinity)

hist(mix.file.dry$d13C)

plot(d13C ~ Salinity, data = mix.file.dry)


salinity.d13C.lm <- lm(d13C ~ Salinity, data = mix.file.dry)

salinity.d13C.lm

summary(salinity.d13C.lm)


salinity.d13c.graph<-ggplot(salinity.d13C.lm, aes(x=Salinity, y=d13C))+
  geom_point() +
  geom_smooth(method="lm", col="black")+
  theme_bw() +
  labs(title = "ECLS Salinity vs. d13C - Dry Season",
       x = "Salinity",
       y = "d13C")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 9, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'right',
        legend.title = element_text(size=12, hjust=0.5, face="bold"))


salinity.d13c.graph


