# Escape Project

# Web Scraping and Data Munging

# Food Data

#' @param city A string representing the city. 
#' @return A dataframe holding all the food data for the input city. 
scrape_food <- function(city) {

library(rvest)
library(RCurl)
library(dplyr)
library(XML)
library(stringr)
library(outliers)
library(RJSONIO)
library(plyr)
library(ggmap)
  
#---------------------------------------------------
# generate urls 
city <- gsub(" ", "", city)
url.template <- paste("https://www.zomato.com/", city, sep = "")
breakfast.url <- paste(url.template, "/breakfast", sep = "")
lunch.url <- paste(url.template, "/lunch", sep = "")
dinner.url <- paste(url.template, "/dinner", sep = "")

#generate tables for each meal
breakfast <- get_meal_data(breakfast.url, "breakfast")
lunch <- get_meal_data(lunch.url, "lunch")
dinner <- get_meal_data(dinner.url, "dinner")

#generate full table 
food <- rbind(breakfast, lunch, dinner)

return(food)
}

get_meal_data <- function(url, meal) {
   # column vectors to be updated
   zomato.names <- c()
   zomato.popularity <- c()
   zomato.cuisine <- c()
   zomato.hours <- c()
   zomato.costFor2 <- c()
   zomato.address <- c()
  
  for (k in 1:2) {
    
    
   zomato <- read_html(paste(url, "?page=",k, sep = ""))
    
    # get number of entries by looking for the first empty string as we loop through the names
    
    j = 0
    while(length(is.na(zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", j+1, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                       html_attr("title"))) == 1) {
      j = j + 1
    }
    
    for (i in 1:j) {
      zomato.names <- c(zomato.names, 
                                  zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                                    html_text()
      )
      
      
      zomato.popularity <- c(zomato.popularity,
                                       zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.ta-right.floating.search_result_rating.col-s-4.clearfix > span", sep = "")) %>% html_text()
      )
      
      zomato.cuisine <- c(zomato.cuisine,
                                    (zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div:nth-child(1) > span.col-s-11.col-m-12.nowrap.pl0 > a", sep = "")) %>% html_text())[1]
      )
    
      zomato.hours <- c(zomato.hours,
                                  zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-timing.clearfix", sep = "")) %>% html_attr("title")
      )
      
      zomato.costFor2 <- c(zomato.costFor2,
                                     gsub(zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-cost.clearfix > span.col-s-11.col-m-12.pl0", sep = "")) %>% html_text(), pattern = "ZAR", replacement = "")
      )
      
      zomato.address <- c(zomato.address, 
                                    zomato %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(2) > div", sep = "")) %>% html_text()
      ) 
    }
  }
    
    zomato.names <- zomato.names[1:50]
    zomato.popularity <- zomato.popularity[1:50]
    zomato.cuisine <- zomato.cuisine[1:50]
    zomato.hours <- zomato.hours[1:50]
    zomato.costFor2 <- zomato.costFor2[1:50]
    zomato.address <- zomato.address[1:50]
    
  
    
    #--------------------------------------------------------------
    
    # Creating the dataframe
    # we shall extract 50 values per subtype (breakfast, lunch, dinner)
    
    food <- data.frame(
      eventname = zomato.names,
      subtype = rep(meal, length(zomato.names)), 
      cuisine = zomato.cuisine,
      price = zomato.costFor2,
      openWeek = rep(NA, length(zomato.names)),
      closeWeek = rep(NA, length(zomato.names)),
      openSat = rep(NA, length(zomato.names)),
      closeSat = rep(NA, length(zomato.names)),
      openSun = rep(NA, length(zomato.names)),
      closeSun = rep(NA, length(zomato.names)),
      eventlength = rep(NA, length(zomato.names)),
      popularity = zomato.popularity, 
      latitude = rep(NA, length(zomato.names))
    )
    
    #--------------------------------------------
    
    # Cleaning data 
    
    # Hours
    
    tempHours <- zomato.hours
    tempHours <- str_split(tempHours, pattern = ",")
    
    # Week
    
    hoursWeek <- sapply(1:length(zomato.names), function(n) { tempHours[[n]][1] })
    
    clean <- str_locate(hoursWeek, "\\(")[, 1]
    hoursWeek <- sapply(1:length(zomato.names), function(n) { if (!is.na(clean[n])) { substring(hoursWeek[n], 1, clean[n] - 1)} else { hoursWeek[n]}})
    hoursWeek <- str_split(hoursWeek, pattern = " to ")
    
    barbarianToCivilised <- function(hours) {
      if (str_detect(hours, "Midnight")) {
        return(24)
      }
      else if (str_detect(hours, "Noon")) {
        return(12)
      } else if (str_detect(hours, "AM")) {
        if (str_locate(hours, pattern = "[[:space:]]")[1] == 2) return(as.numeric(substring(hours, 1, 1)))
        else return(substring(hours, 1, 2))
      } else if (str_detect(hours, "PM")) {
        if (str_locate(hours, pattern = "[[:space:]]")[1] == 2) return(as.numeric(substring(hours, 1, 1)) + 12)
        else return(as.numeric(substring(hours, 1, 2)) + 12)
      }
    }
    
    sapply(1:length(zomato.names), function(n) { hoursWeek[[n]][2] <- str_trim(hoursWeek[[n]][2]) })
    
    food$openWeek <- gsub(sapply(1:length(zomato.names), function(n) { barbarianToCivilised(hoursWeek[[n]][1]) }), pattern = ":", replacement = ".5")
    
    food$closeWeek <- gsub(sapply(1:length(zomato.names), function(n) { if(!is.na(hoursWeek[[n]][2])) { barbarianToCivilised(hoursWeek[[n]][2]) } 
      else { hoursWeek[[n]][2] } }), 
      pattern = ":", replacement = ".5")
    
    food$openSat <- food$openWeek
    food$closeSat <- food$closeWeek
    food$openSun <- food$openWeek
    food$closeSun <- food$closeWeek
    
    food$openWeek[which(sapply(food$openWeek, function(x) { x == "NA"}))] <- NA
    food$closeWeek[which(sapply(food$closeWeek, function(x) { x == "NA"}))] <- NA
    food$openWeek[which(sapply(food$openWeek, function(x) { x == "NULL"}))] <- NA
    food$closeWeek[which(sapply(food$closeWeek, function(x) { x == "NULL"}))] <- NA
    
    # Note: For now, we shall use the week hours for any itinerary generation. Further work will be done to fill up the weekend columns and 
    # customize itinerary depending on which day it is 
    
    # Prices
    food$price <- as.character(food$price)
    food$price <- as.numeric(food$price) / 2
    
    # li: number of elements in the ith price quantile (3 in total)
    l1 <- length(which(food$price <= quantile(food$price, seq(0, 1, 1/3))[2]))
    l2 <- length(which(food$price < quantile(food$price, seq(0, 1, 1/3))[3] & food$price > quantile(food$price, seq(0, 1, 1/3))[2]))
    l3 <- length(which(food$price >= quantile(food$price, seq(0, 1, 1/3))[3]))
    
    food$price[which(food$price <= quantile(food$price, seq(0, 1, 1/3))[2])] <- rep(1, l1)
    food$price[which(food$price < quantile(food$price, seq(0, 1, 1/3))[3] & food$price > quantile(food$price, seq(0, 1, 1/3))[2])] <- rep(2, l2)
    food$price[which(food$price >= quantile(food$price, seq(0, 1, 1/3))[3])] <- rep(3, l3)
    
    # eventlength
    food$eventlength[which(food$price == 1)] <- 1
    food$eventlength[which(food$price == 2)] <- 1.5
    food$eventlength[which(food$price == 3)] <- 2
    food$eventlength[which(scores(food$price, prob = 0.95))] <- 3   # outliers are fancy restaurants
    
    # Event Names
    food$eventname <- as.vector(sapply(food$eventname, function(x) { gsub(x, pattern = "\\\n", replacement = "")}))
    food$eventname <- str_trim(food$eventname)
    
    # Popularity: varies from 1 (lowest) to 3 (highest)
    
    food$popularity <- as.vector(sapply(zomato.popularity, function(x) { gsub(x, pattern = "votes", replacement = "") }))
    food$popularity <- as.numeric(food$popularity)
    
    food$popularity[which(food$popularity < quantile(food$popularity, seq(0, 1, 1/3))[2])] <- rep(1, 50)
    food$popularity[which(food$popularity < quantile(food$popularity, seq(0, 1, 1/3))[3] & food$popularity > quantile(food$popularity, seq(0, 1, 1/3))[2])] <- rep(2, 50)
    food$popularity[which(food$popularity >= quantile(food$popularity, seq(0, 1, 1/3))[3])] <- rep(3, 50)
    
    # cleaning ids (only odd numbers)
    #food$eventid <- seq(1, nrow(food) * 2, by = 2)
    
    #-------------------------------------------
    
    # Getting geolocations through ggmap
    
    addresses <- zomato.address[1:50]
    addresses <- str_trim(addresses)
    food$address <- addresses
    
    geolocations <- sapply(addresses, function(x) { geocode(x) })
    
    food$longitude <- as.vector(sapply(seq(1, length(zomato.names), 2), function(k) { geolocations[k] }))
    food$latitude <- as.vector(sapply(seq(2, length(zomato.names), 2), function(k) { geolocations[k] }))
    
    class(food$longitude) <- "numeric"
    class(food$latitude) <- "numeric"
    
    return(food)
  
}
