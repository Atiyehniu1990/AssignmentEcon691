#Scripts for Homework2
#Created by: Atiyeh,Hasanian Ph.D.
#Created on: December 3, 2023

rm(list=ls())


library(tidycensus)
library(tidyverse)
library(sf)
library(stargazer)
library(plm)


vras<- load_variables(2021, "acs5", cache = TRUE)
data<-vars

#Access data from ACS 5-Year API
#Part1
vars<-c("B01001_001", "B01001_003", "B01001_004", "B01001_005", "B01001_006",
        "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
        "B01001_027", "B01001_028", "B01001_029", "B01001_030",
        "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
        "B02001_002", "B02001_003", "B02001_005",
        "B25087_001", "B25087_002", "B25088_001",
        "B99084_001", "B99084_005",
        "B06011_001", "B25058_001")

#Part2

years <- c(2018, 2019, 2020, 2021)
states<- c(52,21,19,22)


for(i in years){
  acs <- get_acs(geography = "county",	 
                 variables = vars,	 
                 state = c(37,24,51,54),	         
                 year = i,	        
                 geometry = TRUE)	 
   
  
  core <- acs %>%
    mutate(variable = case_when(variable=="B01001_001" ~ "Population",
                                variable=="B06011_001" ~ "Med_Inc",
                                variable=="B25058_001" ~ "Med_Rent",
                                variable=="B25088_001" ~ "Med_Cost",
                                TRUE ~ variable)) %>%
    select(-"moe") %>%
    pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), names_from = "variable", values_from = "estimate") 
  core <- core %>%
    group_by(GEOID) %>%
    mutate(per_und18 = (sum(c_across(B01001_003:B01001_006))+sum(c_across(B01001_027:B01001_030)))/Population,
           per_ovr64 = (sum(c_across(B01001_020:B01001_025))+sum(c_across(B01001_044:B01001_049)))/Population,
           per_blk   = (B02001_003)/Population,
           per_wht   = (B02001_002)/Population,
           per_asn   = (B02001_005)/Population,
           per_oth   = 1 - per_wht - per_blk - per_asn,
           per_mort  = (B25087_002)/B25087_001,
           per_wfh   = (B99084_005)/B99084_001,
           m_monthly = Med_Inc/12,
           Rent_Share = Med_Rent/m_monthly,
           Affordable = .33 - Rent_Share,
           Population = Population/10000000,
           Med_Cost = Med_Cost/10000,
           Year = i)
  ifelse(i==years[1], CORE <- core, CORE <- rbind(CORE, core))
}


#Part3, Regressing the models

mod1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost,
           data = core)
mod2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost,
           data = core)
mod3.1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost +
              factor(Year),
            data = CORE)
mod3.2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost + 
              factor(Year),
            data = CORE)
mod4.1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost +
              factor(Year)+factor(GEOID),
            data = CORE)
mod4.2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost + 
              factor(Year)+factor(GEOID),
            data = CORE)

#Part4
#Table of results in (html)

model_list <- list(mod1, mod2, mod3.1, mod3.2,mod4.1, mod4.2)

html_table <- stargazer(model_list, title = "Regression Models", align = TRUE, type = "html")

cat(html_table, sep = "\n")
stargazer(core,CORE,type="html", title="Table 1 = summary statistics", out="table.html")

writeLines(html_table,"regression_models_table.html")

browseURL("regression_models_table.html")

# Table of results in (latex)
          
model_list <- list(mod1, mod2, mod3.1, mod3.2,mod4.1, mod4.2)

latex_table <- stargazer(model_list, title = "Regression Models", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")
stargazer(core,CORE,type="latex", title="Table 1 = summary statistics", out="table.latex")

writeLines(latex_table,"regression_models_table.latex")

browseURL("regression_models_table.html")          
          
          














  
    
   



  