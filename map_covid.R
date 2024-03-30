#---
# title: "map_covid"
# author: "Jingyang(Judy) Zhang"
# date: "5/20/2020"
#---





## Helper Function: get dataset of the day ##

## Args:
## date: date of dataset to download in the format of "01-22-2020" (start date)
## date ranges from "01-22-2020" to the day before current date

## Return: dataset of the given date

getDat <- function(date){
  
  date_url <- paste(paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date, sep = ""), ".csv", sep = "")

  date_dat <- read_csv(url(date_url))
  
  return(date_dat)
  
}





## Helper Function: clean covid data, group by country region and make it tidy ##

## Args:
## dat: dataset (confirmed cases, deaths cases, recovered cases)

## Return: cleaned covid data set

clean_coviddat <- function(dat){
  dat <- dat %>% 
    clean_names() %>% 
    select(country_region, confirmed, recovered, deaths) %>%
    mutate(
      country_region = case_when(
        tolower(country_region) == "hong kong" ~ "china",
        tolower(country_region) == "macau" ~ "china",
        tolower(country_region) == "mainland china" ~ "china",
        TRUE ~ tolower(country_region)
      )
    ) %>%
    group_by(country_region) %>% 
    summarise(
      total_confirmed = sum(confirmed, na.rm = TRUE),
      total_recovered = sum(recovered, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE)
    )
}


## Helper Function: join covid 19 data and map data ##

## Args:
## covid_dat: dataset of covid 19
## map_dat: dataset of map

## Return:
## dat: joined dataset

joinDat <- function(covid_dat, map_dat){
  
  # Compare country names in the COVID19 datasets and the map dataset
  #diff <- data.frame(unique(covid_dat[!(country_names$country_region %in% map_dat$country_region),]$country_region))
  
  #map_country <- data.frame(map_dat$country_region)
  ## Notes: 20 unmatched countries in COVID19 dataset that are NOT in the map dataset
  
  
  ### CHANGE NAMES IN MAP DATA DATASET ###
  
  #1. COVID 19: antigua and barbuda   
  ## Map: antigua; barbuda
  map_dat$country_region[tolower(map_dat$country_region) == "antigua"] <- "antigua and barbuda"
  map_dat$country_region[tolower(map_dat$country_region) == "barbuda"] <- "antigua and barbuda"
  
  #2. COVID19: burma
  ## Map: myanmar
  map_dat$country_region[tolower(map_dat$country_region) == "myanmar"] <- "burma"
  
  
  #3. COVID19: cabo verde  
  ## Map: cape verde
  map_dat$country_region[tolower(map_dat$country_region) == "cape verde"] <- "cabo verde"
  
  
  
  #4. COVID19: congo (brazzaville)  
  ## Map: republic of congo
  map_dat$country_region[tolower(map_dat$country_region) == "republic of congo"] <- "congo (brazzaville)"
  
  
  #5. COVID19: congo (kinshasa)   
  ## Map: democratic republic of congo
  
  map_dat$country_region[tolower(map_dat$country_region) == "democratic republic of the congo"] <- "congo (kinshasa)"
  
  
  #6. COVID19: cote d'ivoire
  ## Map: ivory coast
  map_dat$country_region[tolower(map_dat$country_region) == "ivory coast"] <- "cote d'ivoire"
  
  
  
  #7. COVID19: czechia
  # Map: czech republic
  map_dat$country_region[tolower(map_dat$country_region) == "czech republic"] <- "czechia"
  
  
  #8. COVID19: diamond princess
  # Map: NONE
  # Not going to be mapped as it is not a country
  
  #9. COVID19: eswatini
  # Map: swaziland
  map_dat$country_region[tolower(map_dat$country_region) == "swaziland"] <- "eswatini"
  
  #10. COVID19: holy see
  # Map: No Match
  
  
  #11. COVID19: korea, south
  ## Map: south korea
  map_dat$country_region[tolower(map_dat$country_region) == "south korea"] <- "korea, south"
  
  
  #12. COVID19: ms zaandam
  ## Map: NONE
  # Not going to be mapped as it is not a country
  
  #13. COVID19: north macedonia
  ## Map: macedonia
  map_dat$country_region[tolower(map_dat$country_region) == "macedonia"] <- "north macedonia"
  
  #14. COVID19: saint kitts and nevis
  ## Map: saint kitts; nevis
  
  map_dat$country_region[tolower(map_dat$country_region) == "saint kitts"] <- "saint kitts and nevis"
  map_dat$country_region[tolower(map_dat$country_region) == "nevis"] <- "saint kitts and nevis"
  
  
  #15. COVID19: saint vincent and the grenadines
  ## Map: saint vincent; grenadines
  
  map_dat$country_region[tolower(map_dat$country_region) == "saint vincent"] <- "saint vincent and the grenadines"
  map_dat$country_region[tolower(map_dat$country_region) == "grenadines"] <- "saint vincent and the grenadines"
  
  
  #16. COVID19: taiwan*
  ## Map: taiwan
  
  map_dat$country_region[tolower(map_dat$country_region) == "taiwan"] <- "taiwan*"
  
  
  
  #17. COVID19: trinidad and tobago
  ## Map: trinidad; tobago
  
  map_dat$country_region[tolower(map_dat$country_region) == "trinidad"] <- "trinidad and tobago"
  
  map_dat$country_region[tolower(map_dat$country_region) == "tobago"] <- "trinidad and tobago"
  
  
  #18. COVID19: united kingdom
  ## Map: uk
  
  map_dat$country_region[tolower(map_dat$country_region) == "uk"] <- "united kingdom"
  
  #19. COVID19: us
  ## Map: usa
  
  map_dat$country_region[tolower(map_dat$country_region) == "usa"] <- "us"
  
  #20. COVID19: west bank and gaza (no palestine in the dataset)
  ## Map: palestine
  
  map_dat$country_region[tolower(map_dat$country_region) == "palestine"] <- "west bank and gaza"
  
  
  
  ## Joing COVID19 dataset and map dataset
  
  covid_geo <- left_join(map_dat, covid_dat, by = "country_region")
  
  
  return(covid_geo)
  
}





## Helper Function: assign value category to number of cases ##

## Args:
## dat: dataset (confirmed cases, deaths cases, recovered cases)
## case: which case to be assign value category to (i.e. confirmed, deaths, recovered)

## Return:
## dat: a dataset with the value category added to the given case


assign_categ <- function(dat, case){
  if(case == "Confirmed Cases"){
    
    dat <- dat %>%
      mutate(
        categ = case_when(
          total_confirmed >= 0 & total_confirmed <= 199 ~ "0-199",
          total_confirmed >= 200 & total_confirmed <= 999 ~ "200-999",
          total_confirmed >= 1000 & total_confirmed <= 9999 ~ "1,000-9,999",
          total_confirmed >= 10000 & total_confirmed <=499999 ~ "10,000-499,999",
          total_confirmed >= 500000 ~ "500,000+"
          
        )
      )
    
  }
  
  else{
    if(case == "Recovered Cases"){
      dat <- dat %>%
        mutate(
          categ = case_when(
            total_recovered >= 0 & total_recovered <= 199 ~ "0-199",
            total_recovered >= 200 & total_recovered <= 999 ~ "200-999",
            total_recovered >= 1000 & total_recovered <= 9999 ~ "1,000-9,999",
            total_recovered >= 10000 & total_recovered <=499999 ~ "10,000-499,999",
            total_recovered >= 500000 ~ "500,000+"
          )
        )
      
    }
    else{
      dat <- dat %>%
        mutate(
          categ = case_when(
            total_deaths >= 0 & total_deaths <= 199 ~ "0-199",
            total_deaths >= 200 & total_deaths <= 999 ~ "200-999",
            total_deaths >= 1000 & total_deaths <= 9999 ~ "1,000-9,999",
            total_deaths >= 10000 & total_deaths <=499999 ~ "10,000-499,999",
            total_deaths >= 500000 ~ "500,000+"
          )
        )
      
    }
  }
  
  return(dat)
}




# Helper Function: map covid 19 data ##
## Args:
## dat: dataset to map (confirmed cases, deaths cases, recovered cases)
## legend_title: title of the legend
## color: which set of colors to use for fill

covid_map <- function(dat, legend_title, color) {
      
        
      plot_dat = dat
      
      plot_dat$categ = factor(plot_dat$categ, 
                              levels = c("0-199",
                                         "200-999",
                                         "1,000-9,999",
                                         "10,000-499,999",
                                         "500,000+"))
      

      covid_map <- ggplot(plot_dat, aes(x = long, y = lat)) +
        
        geom_polygon(aes(group = group, fill = categ)) + 
        
        scale_fill_manual(name = legend_title,
                          values = color,
                          na.value = "#EEEEEE") +
        theme_minimal() + 
        
        labs(caption = "Data Repository provided by Johns Hopkins CSSE.") +
        
        theme(
          ### Plot ###
          plot.background = element_rect(fill = "#ffffff", 
                                         color = NA),
          
          ### Panel ###
          panel.background = element_rect(fill = "#ffffff", 
                                          color = NA),
          
          panel.grid = element_blank(),
          
          ### Axis ###
          axis.text = element_blank(),
          axis.title = element_blank(),
          
          
          ### Legend ###
          legend.position = "bottom",
          
          legend.background = element_rect(fill = "#ffffff", 
                                           color = NA),
          
          ### Text ###
          text = element_text(color = "#22211d")
           
        )
      
      covid_map
      
      return(covid_map)
        
}














