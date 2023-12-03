#our script for Homework1
#Created by: Atiyeh Hasanian
#Created on: November 29,2023

#rm(list=ls())

library(tidycensus)
library(tidyverse)
library(ggplot2)

var<- load_variables(2021,"acs5",cache=TRUE)
temp<- var%>%
  filter(grepl("MEDIAN", concept))%>%
  filter(grepl("GROSS  Contract RENT", concept))
#WE filtered the filter

vars<-c("B06011_001","B25058_001")

acs<- get_acs(geography = "county", #define geography level of data
              variables=vars,  #specific the data we want
              state= c(37,24,51,54),       #denotes the specific states
              year=2021,       #denotes the year
              geometry=TRUE)   #downloads the TIGER shapefile data

# code head shows the 5 lines of dataset
#state code for state of illinois is 17.
#data comes in long and wide format. our data is in long format, cause we have one row for each individual dataset. wide has one row for eeach county
#mutate is a command to create a column
core <- acs %>%
  select(-moe) %>%
  mutate(variable= case_when(variable=="B06011_001" ~ "Med_Inc",
                             variable == "B25058_001" ~ "Med_Rent",
                             TRUE ~ variable)) %>%
  pivot_wider(id_cols = c("GEOID", "NAME","geometry"), names_from = "variable", values_from = "estimate") %>%
  mutate(med_inc2=Med_Inc/12,
         Afford=Med_Rent/med_inc2,
         Affordable=0.33-Afford)


#Affordability: the ratio of  med rentto med income should not be bigger than 33% of income
# benefits of summerizing each part:  1-helps us me to be sure there is no erro
#code to creat a map and visualize it by using different colors for each county, it layers the program, acs stands for statics

ggplot(core)+
  geom_sf(aes(fill=Afford))

ggplot(core) +
  geom_sf(aes(fill = Affordable))+
  scale_fill_gradient2()+ 
  theme_bw()

ggplot(core)+
  geom_sf(aes(fill=Med_Inc+Med_Rent))



