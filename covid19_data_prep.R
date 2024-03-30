#---
# title: "covid_data_prep"
# author: "Jingyang(Judy) Zhang"
# date: "7/13/2020"
#---
  
# LOAD PACKAGES

library(tidyverse)
library(readr)
library(janitor)
library(wbstats)

# LOAD SOURCE FILE

source("map_covid.R")

# LOAD DATA

## Boarding country data ##

borders_url <- url("https://raw.githubusercontent.com/geodatasource/country-borders/master/GEODATASOURCE-COUNTRY-BORDERS.CSV")

borders <- read_csv(borders_url) %>% clean_names()


## COVID19 DATA ##

date_str <- "01-01-2021"

covid19 <- getDat(date_str) %>% clean_coviddat()



# DATA CLEANING AND MANIPULATION

## Match country names in borders to country names in covid19

borders_countries <- tolower(unique(borders$country_name))

covid19_countries <- covid19$country_region

sort(setdiff(borders_countries, covid19_countries))

## NOTES ##

# aland islands: not in covid19
# american samoa: not in covid19
# anguilla: not in covid19
# antarctica: not in covid19
# aruba: not in covid19
# bermuda: not in covid19
# bolivia (plurinational state of): bolivia in covid19

borders$country_name[tolower(borders$country_name) == "bolivia (plurinational state of)"] <- "bolivia"

borders$country_border_name[tolower(borders$country_border_name) == "bolivia (plurinational state of)"] <- "bolivia"

# bonaire, sint eustatius and saba: not in covid19
# bouvet island: not in covid19
# british indian ocean territory: not in covid19
# brunei darussalam: brunei in covid19
borders$country_name[tolower(borders$country_name) == "brunei darussalam"] <- "brunei"
borders$country_border_name[tolower(borders$country_border_name) == "brunei darussalam"] <- "brunei"

# cayman islands: not in covid19
# christmas island: not in covid19
# cocos (keeling) islands: not in covid19
# congo: congo (brazzaville) in covid19
borders$country_name[tolower(borders$country_name) == "congo"] <- "congo (brazzaville)"

borders$country_border_name[tolower(borders$country_border_name) == "congo"] <- "congo (brazzaville)"

# congo (the democratic republic of the): congo (kinshasa) in covid19
borders$country_name[tolower(borders$country_name) == "congo (the democratic republic of the)"] <- "congo (kinshasa)"

borders$country_border_name[tolower(borders$country_border_name) == "congo (the democratic republic of the)"] <- "congo (kinshasa)"
# cook islands: not in covid19
# cote d’ivoire: cote d'ivoire in covid19
borders$country_name[tolower(borders$country_name) == "cote d’ivoire"] <- "cote d'ivoire"

borders$country_border_name[tolower(borders$country_border_name) == "cote d’ivoire"] <- "cote d'ivoire"

# curacao: not in covid19
# falkland islands (malvinas): not in covid19
# faroe islands: not in covid19
# french guiana: not in covid19
# french polynesia: not in covid19
# french southern territories: not in covid19
# gambia (the): gambia in covid19
borders$country_name[tolower(borders$country_name) == "gambia (the)"] <- "gambia"

borders$country_border_name[tolower(borders$country_border_name) == "gambia (the)"] <- "gambia"
# gibraltar: not in covid19
# greenland: not in covid19
# guadeloupe: not in covid19
# guam: not in covid19
# guernsey: not in covid19
# heard island and mcdonald islands: not in covid19
# hong kong: not in covid19
# iran (islamic republic of): iran in covid19
borders$country_name[tolower(borders$country_name) == "iran (islamic republic of)"] <- "iran"

borders$country_border_name[tolower(borders$country_border_name) == "iran (islamic republic of)"] <- "iran"
# isle of man: not in covid19
# jersey: not in covid19
# kiribati: not in covid19
# korea (democratic people's republic of): korea, south in covid19
borders$country_name[tolower(borders$country_name) == "korea (democratic people's republic of)"] <- "korea, south"

borders$country_border_name[tolower(borders$country_border_name) == "korea (democratic people's republic of)"] <- "korea, south"
# korea (the republic of): not in covid19
# lao people's democratic republic: laos in covid19
borders$country_name[tolower(borders$country_name) == "lao people's democratic republic"] <- "laos"

borders$country_border_name[tolower(borders$country_border_name) == "lao people's democratic republic"] <- "laos"
# macao: not in covid19
# marshall islands: not in covid19
# martinique: not in covid19
# mayotte: not in covid19
# micronesia (federated states of): not in covid19
# moldova (the republic of): moldova in covid19
borders$country_name[tolower(borders$country_name) == "moldova (the republic of)"] <- "moldova"

borders$country_border_name[tolower(borders$country_border_name) == "moldova (the republic of)"] <- "moldova"
# montserrat: not in covid19
# myanmar: burma in covid19
borders$country_name[tolower(borders$country_name) == "myanmar"] <- "burma"

borders$country_border_name[tolower(borders$country_border_name) == "myanmar"] <- "burma"
# nauru: not in covid19
# new caledonia: not in covid19
# niue: not in covid19
# norfolk island: not in covid19
# northern mariana islands: not in covid19
# palau: not in covid19
# palestine, state of: not in covid19
# pitcairn: not in covid19
# puerto rico: not in covid19
# reunion: not in covid19
# russian federation: russia in covid19
borders$country_name[tolower(borders$country_name) == "russian federation"] <- "russia"

borders$country_border_name[tolower(borders$country_border_name) == "russian federation"] <- "russia"
# saint barthelemy: not in covid19
# saint helena,\"ascension and tristan da cunha: not in covid19
# saint martin (french part): not in covid19
# saint pierre and miquelon: not in covid19
# samoa:
# sint maarten (dutch part): not in covid19
# solomon islands: not in covid19
# south georgia and the south sandwich islands: not in covid19
# svalbard and jan mayen: not in covid19
# syrian arab republic: syria in covid19
borders$country_name[tolower(borders$country_name) == "syrian arab republic"] <- "syria"

borders$country_border_name[tolower(borders$country_border_name) == "syrian arab republic"] <- "syria"
# taiwan (province of china): not in covid19
# tanzania (the united republic of): tanzania in covid19
borders$country_name[tolower(borders$country_name) == "tanzania (the united republic of)"] <- "tanzania"

borders$country_border_name[tolower(borders$country_border_name) == "tanzania (the united republic of)"] <- "tanzania"
# tokelau: not in covid19
# tonga: not in covid19
# turkmenistan: not in covid19
# turks and caicos islands: not in covid19
# tuvalu: not in covid19
# united kingdom of great britain and northern ireland: not in covid19
# united states minor outlying islands: not in covid19
# united states of america: us in covid19
borders$country_name[tolower(borders$country_name) == "united states of america"] <- "us"

borders$country_border_name[tolower(borders$country_border_name) == "united states of america"] <- "us"
# vanuatu: not in covid19
# venezuela (bolivarian republic of): venezuela in covid19
borders$country_name[tolower(borders$country_name) == "venezuela (bolivarian republic of)"] <- "venezuela"

borders$country_border_name[tolower(borders$country_border_name) == "venezuela (bolivarian republic of)"] <- "venezuela"
# viet nam: vietname in covid19
borders$country_name[tolower(borders$country_name) == "viet nam"] <- "vietnam"

borders$country_border_name[tolower(borders$country_border_name) == "viet nam"] <- "vietnam"
# virgin islands (british): not in covid19
# virgin islands (u.s.): not in covid19
# wallis and futuna: not in covid19




# CREATE CONNECTIVITY MATRIX (WEIGHT MATRIX)
## Use binary connectivity matrix (i.e. adjacency weights) with w_ij = 1 if country i and country j share any portion of their borders with another.
## We will build the matrix using the borders dataset. 


## Function: get_borders - gets border countries of a given country ##

## Args: 
## borders: dataset containing border information
## country: country of interest

## Return: list of countries that share border with the given country

get_borders <- function(borders, country){
  border_countries <- borders %>% filter(tolower(country_name) == tolower(country))
  
  border_countries <- tolower(border_countries$country_border_name)
  
  return(border_countries)
}






## Function: get_connect - creates a connect matrix using information from the borders dataset ##

## Arg: 
## borders: dataset containing border information

## Return: a connectivity matrix 


get_connect <- function(borders){
  # Get list of unique countries 
  countries <- tolower(unique(borders$country_name))
  
  # Create an empty connect matrix
  connect_matrx <- matrix(data = NA, nrow = length(countries), ncol = length(countries))
  colnames(connect_matrx) <- countries
  rownames(connect_matrx) <- countries
  
  # Get border information of every country in the countries list
  for(i in 1 : length(countries)){
    country <- countries[i]
    border_countries <- get_borders(borders, country)
    
    if(is.na(border_countries)){
      next
    }
    else{
      
      if(length(border_countries) > 1){
        for(j in 1 : length(border_countries)){
          
          connect_matrx[border_countries[j], country] <- 1
        }
      }
      else{
        
        connect_matrx[border_countries, country] <- 1
      }
      
    }
    
  }
  
  # Replace all NA with 0s
  connect_matrx[is.na(connect_matrx)] <- 0
  
  return(connect_matrx)
  
}



## Get connectivity matrix
connect_matrx <- get_connect(borders)





## Function: value_lookup - look up value of confirmed/recovered/deaths cases of a given country ##

## Args: 
## country: country of interest
## covid19: covid19 dataset
## type: which type of value to draw, confirmed, recovered, deaths

## Return: a number of value of confirmed/recovered/deaths cases of the given county

value_lookup <- function(covid19, country, type){
  country_value <- covid19 %>% 
    filter(tolower(country_region) == tolower(country))
  
  case_value <- case_when(
    type == "confirmed" ~ country_value$total_confirmed,
    type == "recovered" ~ country_value$total_recovered,
    type == "deaths" ~ country_value$total_deaths
  )
  
  if(length(case_value) == 0){
    case_value <- 0
  }
  
  return(case_value)
  
}

