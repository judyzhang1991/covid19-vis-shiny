#---
# title: "network_flights"
# author: "Jingyang(Judy) Zhang"
# date: "5/20/2020"
#---



## Helper Function: convert date from the form 01jan2020 to 01-01-2020 ##

## Args:
## date: string of date in the form of 01jan2020

## Return:
## formatted_date: string of date in the form of 01-01-2020

format_date_str <- function(date){
  
  str <- toString(dmy(date))
  
  splits <- strsplit(str, "-")
  
  yr <- unlist(splits[[1]][1])
  
  month <- unlist(splits[[1]][2])
  
  day <- unlist(splits[[1]][3])
  
  formated_date <- paste(paste(month, day, sep = "-"), yr, sep = "-")
  
  return(formated_date)
}




## Helper Function: check international travel restriction status ##

## Args:
## travel_rest_dat: travel restriction dataset
## ctry: the country of interest
## date_str: the date of interest, format 01-22-2020
## Return:

## travel_rest_status: an indicator (double) of travel restriction status:
## 0: no measures
## 1: screening
## 2: quarantine arrivals from high-risk regions
## 3: ban on high-risk regions
## 4: total border closure
## No data: blank

check_travel_rest <- function(travel_rest_dat, ctry, date_str){
  rest_status <- travel_rest_dat %>% filter(country_name == ctry, date == date_str)
  
  travel_rest_status <- as.double(rest_status$restriction)
  ## Travel_rest_status contains data for every state of the U.S.
  ## Only want country level data - U.S. country restriction = restriction of most states
  if(length(travel_rest_status) > 1){
    us_rest <- data.frame(rest = travel_rest_status)
    us_rest <- us_rest %>% group_by(rest) %>% summarize(count = n()) %>% arrange(desc(count))
    travel_rest_status <- us_rest[1, ]$rest
  }
  
  return(travel_rest_status)
}


## Helper Function: update flight routes information according to the travel restriction status ##
## Args:
## direct_routes_dat: direct flight routes data,
## travel_rest_dat: travel restriction dataset
## ctry: country of interest
## date_str: date of interest

## Return
## updated_flight_dat: updated flight routes data

update_flight <- function(direct_routes_dat, travel_rest_dat, ctry, date_str){
  
  travel_rest_status <- check_travel_rest(travel_rest_dat, ctry, date_str)
  
  
  if(!is.na(travel_rest_status)){
    
    # If travel restriction = 3: ban on high risk regions, then remove all flights into/out of high risk countries
    if(travel_rest_status == 3){
      for(i in 1 : length(high_risk)){
        direct_routes_dat <- direct_routes_dat[!(direct_routes_dat$dest_country == high_risk[i] | direct_routes_dat$source_country == high_risk[i]),]
      }
      
    }
    else{
      
      if(travel_rest_status == 4){
        
        
        # If travel restriction = 4: total border closure, then remove all flights into/out of  the given country
        direct_routes_dat <- direct_routes_dat[!(direct_routes_dat$dest_country == ctry | direct_routes_dat$source_country == ctry),]
      }
      
    }
    
  }
  
  #print(nrow(direct_routes_dat))
  
  return(direct_routes_dat)
  
}


## Helper Function: find country for a given IATA code ##
## Args:
## airports_hash: hastable of key = IATA and value = country name
## IATA: IATA code string

## Return:
## country: country name of the given IATA code


country_lookup <- function(airports_hash, iata){
  
  return(airports_hash[[iata]])
}




## Helper Function: find country for a given IATA code ##
## Args:
## airports_hash: hastable of key = IATA and value = country name
## IATA: IATA code string

## Return:
## country: country name of the given IATA code


country_lookup <- function(airports_hash, iata){
  
  return(airports_hash[[iata]])
}


## Helper Function: find latitude or longitude of a given airport ##
## Args:
## airport_lat_hash: hastable of key = airport iata, value = latitude
## airport_long_hash: hashtable of key = airport iata, value = longitude
## iata: iata of the airport

## Return:
## lat or long: latitude or longitude of the given airport

geo_lookup <- function(airport_lat_hash = NULL, airport_long_hash = NULL, iata){
  if(!is.null(airport_lat_hash)){
    
    return(airport_lat_hash[[iata]])
  }
  
  if(!is.null(airport_long_hash)){
    
    return(airport_long_hash[[iata]])
  }
  
  
}





## Helper Function: assign geo code to a country using average of latitude and longitude of all airports in the country ##

## Args:
## ctry: name of the country
## direct_routes: dataframe of all international direct routes

## Return:
## country_geo: calculated geo code of the country


assign_country_geo <- function(direct_routes, ctry){
  all_airports <- direct_routes %>%
    filter(source_country == ctry | dest_country == ctry)
  
  source_airports_dat <- all_airports %>% 
    filter(source_country == ctry) %>%
    select(source_airport, source_airport_lat, source_airport_long) %>%
    rename(c( "airport" = "source_airport", 
             "lat" = "source_airport_lat" ,
             "long" = "source_airport_long"))
  
  dest_airports_dat <- all_airports %>% 
    filter(dest_country == ctry) %>%
    select(destination_airport, dest_airport_lat, dest_airport_long) %>%
    rename(c( "airport" = "destination_airport", 
              "lat" = "dest_airport_lat" ,
              "long" = "dest_airport_long"))
  
  airports_distinct <- rbind(source_airports_dat, dest_airports_dat) %>%
    distinct()
  
  
  country_lat <- mean(airports_distinct$lat)
  country_long <- mean(airports_distinct$long)
  
  return(c(country_lat, country_long))
}


## Plot Function: plot flight routes to and from a given country ##
## Args:
## direct_routes: dataframe of all international direct routes
## ctry: name of the country
## world_map: world map ggplot object

plot_routes <- function(direct_routes, travel_rest_dat, ctry, world_map, InOut, date_str){
  
  if(InOut == "Out"){
    routes <- direct_routes %>% filter(source_country == ctry)
    
    curve_color = "#16a085"
    
    dot_color = "#7303fc"
  }else{
    routes <- direct_routes %>% filter(dest_country == ctry)
    
    curve_color = "#490e60"
    
    dot_color = "#7303fc"
  }
  
  
  travel_rest_status <- check_travel_rest(travel_rest_dat, ctry, date_str)
  

  
  if(!is.na(travel_rest_status)){
    if(travel_rest_status == 0){
      line_type = "solid"
    }else{
      if(travel_rest_status == 1){
        line_type = "longdash"
      }else{
        if(travel_rest_status == 2){
          line_type = "twodash"
        }else{
          if(travel_rest_status == 3){
            line_type = "dotted"
          }
          else{line_type = "blank"}
        }
      }
    }
  }
  
  
  #print(line_type)
  
  
  # Update direct routes data with travel policy
  routes <- update_flight(routes, travel_rest_dat, ctry, date_str)
  
  if(nrow(routes) == 0){
    world_map
  }else{
  flights <- data.frame(
    origin = routes$source_country,
    destination = routes$dest_country
  ) 
  
  # Remove rows origin = destination
  flights <- flights %>% filter(origin != destination) %>% unique()
  
  # Convert to network
  flights <- network(flights, direct = TRUE)
  
  
  # Add geographic coordinates
  flights %v% "lat" <- countries_geo[network.vertex.names(flights), "country_lat" ]
  
  flights %v% "long" <- countries_geo[network.vertex.names(flights), "country_long" ]
  
  
  # Get country geo code
  plotcord = data.frame(
    long = as.numeric(flights %v% "long"),
    lat = as.numeric(flights %v% "lat")
  )
  
  
  # Create edges
  edges <- network::as.matrix.network.edgelist(flights)
  
  # Create edges coordinates
  edges_mat <- data.frame(
    x = plotcord$long[edges[,1]],
    xend = plotcord$long[edges[,2]],
    y = plotcord$lat[edges[,1]],
    yend = plotcord$lat[edges[,2]]
  )
  
  #plotcord$labels <- NA
  #for(i in 1:length(plotcord$long)){
  #  label = paste(plotcord$lat[i], plotcord$long[i], sep = ",")
  #  plotcord$labels[i] = label
  #}
  
  world_map + 
    geom_point(data = plotcord, mapping = aes(x = long, y = lat), 
               size = 0.008,
               color = dot_color) +
    #geom_text(data = plotcord, mapping = aes(label = labels), hjust = 0, vjust = 0)
  
    geom_curve(
      data = edges_mat, 
      mapping = aes(x = x, xend = xend, y = y, yend = yend), 
      arrow = arrow(length = unit(0.02, "npc")),
      color = curve_color,
      linetype = line_type
    ) + 
    theme_minimal() + 
    theme(
      
      
      ### Panel ###
      panel.background = element_rect(fill = "#ffffff", 
                                      color = NA),
      
      panel.grid = element_blank(),
      
      ### Axis ###
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      
      
      ### Legend ###
      legend.position = "bottom",
      legend.background = element_rect(fill = "#ffffff", 
                                       color = NA),
      
      ### Text ###
      text = element_text(color = "#22211d")
    
    )
  
  }
}










