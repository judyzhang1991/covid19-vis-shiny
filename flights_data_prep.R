#---
# title: "flights_data_prep"
# author: "Jingyang(Judy) Zhang"
# date: "5/20/2020"
#---





# LOAD SOURCE FILES
source("network_flights.R")


# LOAD DATA

## TRAVEL RESTRICTION DATA ##

travel_rest_dat <- read_csv("data/c8ev_internationaltravel.csv") 

## Restriction Code
### Source: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/interpretation_guide.md  (C8-International Travel Controls)
#### 0 - no policy.
#### 1 - screening for everyone. 
#### 2 - mandatory quarantine.
#### 3 - prohibits non-vaccinated entry from high-risk countries.
#### 4 - prohibits non-vaccinated entry from any country.
 
colnames(travel_rest_dat)[1] <- "country"


travel_rest_dat <- travel_rest_dat %>%
  pivot_longer(cols = !c(country, country_code, country_name, region_code, region_name, jurisdiction),
               names_to = "date",
               values_to = "restriction")


## HIGH RISK REGIONS DATA ##
## Source: https://www.cdc.gov/coronavirus/2019-ncov/travelers/from-other-countries.html

high_risk <- c("China", "Iran", "Austria", "Belgium", "Czech Republic", "Denmark", "Estonia", "Finland",
               "France", "Germany", "Greece", "Hungary", "Iceland", "Italy", "Latvia", "Liechtenstein", "Lithuania", 
               "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", 
               "Sweden", "Switzerland", "Monaco", "San Marino", "Vatican City",
               "United Kingdom", "Ireland", "Brazil")


## FLIGHT ROUTES DATA ##
## Source: https://www.kaggle.com/open-flights/flight-route-database
routes <- read_csv("data/routes.csv") %>%
  clean_names() 

names(routes)[names(routes) == "destination_apirport"] <- "destination_airport"


# Get direct routes
direct_routes <- routes %>% filter(
  stops == 0
)

## Source: https://www.kaggle.com/divyanshrai/openflights-airports-database-2017
airports <- read_csv("data/airports.csv") %>%
  clean_names() %>%
  filter(
    iata != "\\N"
  ) %>% as.data.frame()

rownames(airports) <- airports$iata

# Create a lookup hash table for looking up country of an airport
airports_hash <- hash()

for(i in 1:nrow(airports)){
  airports_hash[[airports$iata[i]]] <- airports$country[i]
}




# Reformat date column in travel restriction data
for(i in 1:nrow(travel_rest_dat)){
  print(i)
  travel_rest_dat$date[i] <- format_date_str(travel_rest_dat$date[i])
}




# Create a lookup hash table for looking up country latitude
airport_lat_hash <- hash()

for(i in 1:nrow(airports)){
  airport_lat_hash[[airports$iata[i]]] <- airports$latitude[i]
}



# Create a lookup hash table for looking up country longtitude
airport_long_hash <- hash()

for(i in 1:nrow(airports)){
  airport_long_hash[[airports$iata[i]]] <- airports$longitude[i]
}



# Add source country and destination country to direct_routes dataframe
## Initialize an empty column

# Add source country geo code and destination country geocode to direct_routes dataframe
## Initialize an empty column

direct_routes <- direct_routes %>% 
  
  mutate(
    source_country = NA,
    
    dest_country = NA,
    
    source_airport_lat = NA,
    
    source_airport_long = NA,
    
    dest_airport_lat = NA,
    
    dest_airport_long = NA
  )




for(i in 1:nrow(direct_routes)){
  
  source = country_lookup(airports_hash, direct_routes$source_airport[i])
  
  if(!is.null(source)){
    
    direct_routes$source_country[i] = source
    
    source_lat = geo_lookup(airport_lat_hash, NULL, direct_routes$source_airport[i])
    
    source_long = geo_lookup(NULL, airport_long_hash, direct_routes$source_airport[i])
    
    if(!is.null(source_lat) & !is.null(source_long)){
      
      direct_routes$source_airport_lat[i] = source_lat
      
      direct_routes$source_airport_long[i] = source_long
    }
    
  }
  
  dest = country_lookup(airports_hash, direct_routes$destination_airport[i])
  
  if(!is.null(dest)){
    
    direct_routes$dest_country[i] = dest
    
    dest_lat = geo_lookup(airport_lat_hash, NULL, direct_routes$destination_airport[i])
    
    dest_long = geo_lookup(NULL, airport_long_hash, direct_routes$destination_airport[i])
    
    if(!is.null(dest_lat) & !is.null(dest_long)){
      
      direct_routes$dest_airport_lat[i] = dest_lat
      
      direct_routes$dest_airport_long[i] = dest_long
    }
  }
  
}


# Remove demostic flights
direct_routes <- direct_routes %>%
  filter(source_country != dest_country)




# Assign geo code for each source country and each destination country
direct_routes <- direct_routes %>%
  mutate(
    source_country_lat = NA,
    source_country_long = NA,
    dest_country_lat = NA,
    dest_country_long = NA
  )





for(i in 1 : nrow(direct_routes)){
  print(i)
  source_country_geo = assign_country_geo(direct_routes, direct_routes$source_country[i])
  
  direct_routes$source_country_lat[i] = source_country_geo[1]
  direct_routes$source_country_long[i] = source_country_geo[2]
  
  dest_country_geo = assign_country_geo(direct_routes, direct_routes$dest_country[i])
  
  direct_routes$dest_country_lat[i] = dest_country_geo[1]
  direct_routes$dest_country_long[i] = dest_country_geo[2]
}


# Create a dataframe of distinct countries and its calculated geo code
countries_geo <- data.frame(
  country = unique(c(direct_routes$source_country, direct_routes$dest_country))
) %>%
  mutate(
    country_lat = NA,
    country_long = NA
  )

# Add calculated country geo code

for(i in 1:nrow(countries_geo)){
  
  countries_geo$country_lat[i] = assign_country_geo(direct_routes, countries_geo$country[i])[1]
  countries_geo$country_long[i] = assign_country_geo(direct_routes, countries_geo$country[i])[2]
  
}

rownames(countries_geo) <- countries_geo$country
 


## SAVE DATA ##


write.csv(airports, "data/airports.csv")

write.csv(countries_geo, "data/countries_geo.csv")

write.csv(direct_routes, "data/direct_routes.csv")
  
write.csv(routes, "data/routes.csv")
write.csv(travel_rest_dat, "data/travel_rest_dat.csv")











