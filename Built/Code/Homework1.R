#our script for Homework1
#Created by: Atiyeh Hasanian
#Created on: November 29,2023

rm(list=ls())

library(tidycensus)
library(tidyverse)
library(ggplot2)

#Part two: Generating data frame and transforming long data frame to wide one

var<- load_variables(2021,"acs5",cache=TRUE)
temp<- var%>%
  filter(grepl("MEDIAN", concept))%>%
  filter(grepl("GROSS  Contract RENT", concept))


vars<-c("B06011_001","B25058_001")

acs<- get_acs(geography = "county", 
              variables=vars,  
              state= c(37,24,51,54),       
              year=2021,       
              geometry=TRUE)   


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


#Part 3, first one: a map of states

ggplot(core)+
  geom_sf(aes(fill=Affordable))+
    scale_fill_gradient2()+ 
        theme_bw()

# Part 3, second map 

ggplot(core)+
  geom_sf(aes(fill=Med_Inc))+
    theme_bw()


#Part 3, third map

ggplot(core)+
 geom_sf(aes(fill=Med_Rent))+
  theme_bw()
    
  


