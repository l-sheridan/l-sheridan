
# Setting up
install.packages("devtools")
install.packages("cli")
install.packages("readxl")
install.packages("readxl")
extrafont::loadfonts(device="win")

install.packages("extrafontdb")
extrafont::loadfonts(device="win")

library(readxl)
library(cli)
library(devtools)
library(urbnmapr)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(extrafontdb)
library(RColorBrewer)



devtools::install_github("UrbanInstitute/urbnmapr")

# Set directory and read in data 
setwd("C:/Users/leigh/OneDrive/Desktop/EPI 520 Infectious Disease")
oregon_tab2 <- read_excel("oregon_tab2.xlsx")
oregon_tab7_prev <- read_excel("oregon_tab7_prev.xlsx")
oregon_tab7 <- read_excel("oregon_tab7.xlsx")


# ------------------------- Oregon Table 7 ----------------------------------------


# --------------------------- Overall population map -----------------------
# rename oregon accordingly 
colnames(oregon_tab7)[1] ="county_name"

# Read in, clean and merge data 
territories_counties <- get_urbn_map(map = "territories_counties", sf = TRUE)
states <- get_urbn_labels(map = "states", sf = TRUE)



# filter to just oregon
oregon_df <- get_urbn_map(map='counties', sf=TRUE) %>%
  filter(state_name == 'Oregon')


# Join two datasets together w/o losing geometry 

# Visualize the data

oregon_df %>%
  left_join(oregon_tab7, by='county_name') %>%
  ggplot() +  theme_minimal(base_family = "Calibri") + scale_fill_gradient(low='white', high='blue') +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 4), "cm")) +
  geom_sf(aes(fill=Pop))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       title = "Population Map of Oregon by County 1990",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       Pop = Population")

# ------------------------ Prevalence map --------------------------------------------

# change name in excel file 
colnames(oregon_tab7_prev)[1] ="county_name"

# ------------------------
oregon_df %>%
  left_join(oregon_tab7_prev, by='county_name') %>%
  ggplot() +  theme_minimal(base_family = "Calibri") + scale_fill_gradient(low='white', high='blue') +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 4), "cm")) +
  geom_sf(aes(fill=Prevalence))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       title = "Proportion of Oregon Population by County, 1990",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       Prevalence = Proportion of Population in Oregon by County")

# -------------------------- Oregon Table 2 -------------------------------------
# remove total row
oregon_tab2 = oregon_tab2[-23,]


# rename oregon_tab2 for merge
colnames(oregon_tab2)[1] ="county_name"

# ---------------------------- Total count map

oregon_df %>%
  left_join(oregon_tab2, by='county_name') %>%
  ggplot()  + scale_fill_distiller(palette ="Oranges", direction = 1) +  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 200), "cm")) +
  geom_sf(aes(fill=TOTAL))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       title = "Total Count of Oregon E.Coli Cases, 1990-1992 by County",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       
       TOTAL = Case count by county over 3 years
       **Grey = no data available")


# Read in oregon tab 3 
oregon_tab3 <- read_excel("oregon_tab3.xlsx")

# remove total row
oregon_tab3 = oregon_tab3[-23,]


# rename oregon_tab2 for merge
colnames(oregon_tab3)[1] ="county_name"

# --------------------------- Map 1990  ------------------------------------------
oregon_df %>%
  left_join(oregon_tab3, by='county_name') %>%
  ggplot()  + scale_fill_distiller(palette ="Blues", direction = 1) +  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 200), "cm")) +
  geom_sf(aes(fill=P_1990))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       fill = "Proportion",
       title = "Proportion of 1990 E.Coli Cases by Oregon County",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       **Grey = no data available")

# --------------------------- Map 1991  ------------------------------------------
oregon_df %>%
  left_join(oregon_tab3, by='county_name') %>%
  ggplot()  + scale_fill_distiller(palette ="Blues", direction = 1) +  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 200), "cm")) +
  geom_sf(aes(fill=P_1991))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       fill = "Proportion",
       title = "Proportion of 1991 E.Coli Cases by Oregon County",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       **Grey = no data available")

# --------------------------- Map 1992  ------------------------------------------
oregon_df %>%
  left_join(oregon_tab3, by='county_name') %>%
  ggplot()  + scale_fill_distiller(palette ="Blues", direction = 1) +  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TT Times New Roman"))+
  theme(plot.margin=unit(rep(0.5, 200), "cm")) +
  geom_sf(aes(fill=P_1992))  +  theme(plot.title=element_text(family="Times New Roman", margin=margin(b=15)))+
  theme(plot.subtitle=element_text(family="TTimes New Roman")) + 
  coord_sf(crs = '+proj=utm +zone=11 +ellps=WGS84 +towgs84=0,0,0') + 
  labs(x = "",
       y = "",
       fill = "Proportion",
       title = "Proportion of 1992 E.Coli Cases by Oregon County",
       caption = "Author: Leigh Sheridan 
       Data: Oregon Health Authority
       **Grey = no data available")
