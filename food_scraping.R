# Escape Project

# Web Scraping and Data Munging

# Food Data

library(rvest)
library(RCurl)
library(dplyr)
library(XML)
library(stringr)
library(outliers)
library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)

#---------------------------------------------------

# scrape in breakfast data 

# loop through the 1st and 2nd page by modifying the url

# column vectors to be updated
zomato.breakfast.names <- c()
zomato.breakfast.popularity <- c()
zomato.breakfast.cuisine <- c()
zomato.breakfast.hours <- c()
zomato.breakfast.costFor2 <- c()
zomato.breakfast.address <- c()

for (k in 1:2) {

  
  zomato.breakfast <- read_html(paste("https://www.zomato.com/capetown/breakfast?page=", k, sep = ""))

  # get number of entries by looking for the first empty string as we loop through the names

  j = 0
  while(length(is.na(zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", j+1, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                     html_attr("title"))) == 1) {
    j = j + 1
  }


  z %>% html_nodes(css = "#orig-search-list > div:nth-child(1) > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0") %>% html_text()

  for (i in 1:j) {
    zomato.breakfast.names <- c(zomato.breakfast.names, 
                      zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                                 html_text()
                               )


    zomato.breakfast.popularity <- c(zomato.breakfast.popularity,
                                     zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.ta-right.floating.search_result_rating.col-s-4.clearfix > span", sep = "")) %>% html_text()
                                    )
  
    zomato.breakfast.cuisine <- c(zomato.breakfast.cuisine,
                                  (zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div:nth-child(1) > span.col-s-11.col-m-12.nowrap.pl0 > a", sep = "")) %>% html_text())[1]
                                 )
  
    zomato.breakfast.hours <- c(zomato.breakfast.hours,
                                zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-timing.clearfix", sep = "")) %>% html_attr("title")
                               )
  
    zomato.breakfast.costFor2 <- c(zomato.breakfast.costFor2,
                                   gsub(zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-cost.clearfix > span.col-s-11.col-m-12.pl0", sep = "")) %>% html_text(), pattern = "ZAR", replacement = "")
                                  )
  
    zomato.breakfast.address <- c(zomato.breakfast.address, 
                                  zomato.breakfast %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(2) > div", sep = "")) %>% html_text()
                                  ) 
  }

}

  #-----------------------------------------------

# scrape in lunch data

# same process

zomato.lunch.names <- c()
zomato.lunch.popularity <- c()
zomato.lunch.cuisine <- c()
zomato.lunch.hours <- c()
zomato.lunch.costFor2 <- c()
zomato.lunch.address <- c()

for (k in 1:2) {
  
  zomato.lunch <- read_html(paste("https://www.zomato.com/capetown/lunch?page=", k, sep = ""))
  
  # get number of entries by looking for the first empty string as we loop through the names
  
  j = 0
  while(length(is.na(zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", j+1, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                     html_attr("title"))) == 1) {
    j = j + 1
  }
  
  
  for (i in 1:j) {
    zomato.lunch.names <- c(zomato.lunch.names, 
                                zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                              html_text()
    )
    
    zomato.lunch.popularity <- c(zomato.lunch.popularity,
                                     zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.ta-right.floating.search_result_rating.col-s-4.clearfix > span", sep = "")) %>% html_text()
    )
    
    zomato.lunch.cuisine <- c(zomato.lunch.cuisine,
                                  (zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div:nth-child(1) > span.col-s-11.col-m-12.nowrap.pl0 > a", sep = "")) %>% html_text())[1]
    )
    
    zomato.lunch.hours <- c(zomato.lunch.hours,
                                zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-timing.clearfix", sep = "")) %>% html_attr("title")
    )
    
    zomato.lunch.costFor2 <- c(zomato.lunch.costFor2,
                                   gsub(zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-cost.clearfix > span.col-s-11.col-m-12.pl0", sep = "")) %>% html_text(), pattern = "ZAR", replacement = "")
    )
    
    zomato.lunch.address <- c(zomato.lunch.address, 
                                  zomato.lunch %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(2) > div", sep = "")) %>% html_text()
    )
  }
  
}

#-----------------------------------------------

# scrape in dinner data

# same process

zomato.dinner.names <- c()
zomato.dinner.popularity <- c()
zomato.dinner.cuisine <- c()
zomato.dinner.hours <- c()
zomato.dinner.costFor2 <- c()
zomato.dinner.address <- c()

for (k in 1:2) {
  
  zomato.dinner <- read_html(paste("https://www.zomato.com/capetown/dinner?page=", k, sep = ""))
  
  # get number of entries by looking for the first empty string as we loop through the names
  
  j = 0
  while(length(is.na(zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", j+1, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                     html_attr("title"))) == 1) {
    j = j + 1
  }
  
  
  for (i in 1:j) {
    zomato.dinner.names <- c(zomato.dinner.names, 
                            zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.result-title.hover_feedback.zred.bold.ln24.fontsize0", sep = "")) %>% 
                              html_text()
    )
    
    zomato.dinner.popularity <- c(zomato.dinner.popularity,
                                 zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.ta-right.floating.search_result_rating.col-s-4.clearfix > span", sep = "")) %>% html_text()
    )
    
    zomato.dinner.cuisine <- c(zomato.dinner.cuisine,
                              (zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div:nth-child(1) > span.col-s-11.col-m-12.nowrap.pl0 > a", sep = "")) %>% html_text())[1]
    )
    
    zomato.dinner.hours <- c(zomato.dinner.hours,
                            zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-timing.clearfix", sep = "")) %>% html_attr("title")
    )
    
    zomato.dinner.costFor2 <- c(zomato.dinner.costFor2,
                               gsub(zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.search-page-text.clearfix.row > div.res-cost.clearfix > span.col-s-11.col-m-12.pl0", sep = "")) %>% html_text(), pattern = "ZAR", replacement = "")
    )
    
    zomato.dinner.address <- c(zomato.dinner.address, 
                                  zomato.dinner %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(2) > div", sep = "")) %>% html_text()
    )
  }
  
}

#--------------------------------------------------------------

# Creating the dataframe
# we shall extract 50 values per subtype (breakfast, lunch, dinner)

food <- data.frame(
  eventid = 1:150,
  eventname = c(zomato.breakfast.names[1:50], zomato.lunch.names[1:50], zomato.dinner.names[1:50]),
  subtype = c(rep("breakfast", 50), rep("lunch", 50), rep("dinner", 50)),
  cuisine = c(zomato.breakfast.cuisine[1:50], zomato.lunch.cuisine[1:50], zomato.dinner.cuisine[1:50]),
  price = c(zomato.breakfast.costFor2[1:50], zomato.lunch.costFor2[1:50], zomato.dinner.costFor2[1:50]),
  openWeek = rep(NA, 150),
  closeWeek = rep(NA, 150),
  openSat = rep(NA, 150),
  closeSat = rep(NA, 150),
  openSun = rep(NA, 150),
  closeSun = rep(NA, 150),
  eventlength = rep(NA, 150),
  popularity = c(zomato.breakfast.popularity[1:50], zomato.lunch.popularity[1:50], zomato.dinner.popularity[1:50]),
  longitude = rep(NA, 150),
  latitude = rep(NA, 150)
)

#--------------------------------------------

# Cleaning data 

# Hours

tempHours <- c(zomato.breakfast.hours[1:50], zomato.lunch.hours[1:50], zomato.dinner.hours[1:50])
tempHours <- str_split(tempHours, pattern = ",")

# Week

hoursWeek <- sapply(1:150, function(n) { tempHours[[n]][1] })

clean <- str_locate(hoursWeek, "\\(")[, 1]
hoursWeek <- sapply(1:150, function(n) { if (!is.na(clean[n])) { substring(hoursWeek[n], 1, clean[n] - 1)} else { hoursWeek[n]}})
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

sapply(1:150, function(n) { hoursWeek[[n]][2] <- str_trim(hoursWeek[[n]][2]) })

food$openWeek <- gsub(sapply(1:150, function(n) { barbarianToCivilised(hoursWeek[[n]][1]) }), pattern = ":", replacement = ".5")

food$closeWeek <- gsub(sapply(1:150, function(n) { if(!is.na(hoursWeek[[n]][2])) { barbarianToCivilised(hoursWeek[[n]][2]) } 
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

food$price[which(food$price < quantile(food$price, seq(0, 1, 1/3))[2])] <- rep(1, 50)
food$price[which(food$price < quantile(food$price, seq(0, 1, 1/3))[3] & food$price > quantile(food$price, seq(0, 1, 1/3))[2])] <- rep(2, 49)
food$price[which(food$price >= quantile(food$price, seq(0, 1, 1/3))[3])] <- rep(3, 51)

# eventlength
food$eventlength[which(food$price == 1)] <- 1
food$eventlength[which(food$price == 2)] <- 1.5
food$eventlength[which(food$price == 3)] <- 2
food$eventlength[which(scores(food$price, prob = 0.95))] <- 3   # outliers are fancy restaurants

# Event Names
food$eventname <- as.vector(sapply(food$eventname, function(x) { gsub(x, pattern = "\\\n", replacement = "")}))
food$eventname <- str_trim(food$eventname)

# Popularity: varies from 1 (lowest) to 3 (highest)
food$popularity <- as.vector(sapply(c(zomato.breakfast.popularity[1:50], zomato.lunch.popularity[1:50], zomato.dinner.popularity[1:50]), function(x) { gsub(x, pattern = "votes", replacement = "") }))
food$popularity <- as.numeric(food$popularity)

food$popularity[which(food$popularity < quantile(food$popularity, seq(0, 1, 1/3))[2])] <- rep(1, 50)
food$popularity[which(food$popularity < quantile(food$popularity, seq(0, 1, 1/3))[3] & food$popularity > quantile(food$popularity, seq(0, 1, 1/3))[2])] <- rep(2, 50)
food$popularity[which(food$popularity >= quantile(food$popularity, seq(0, 1, 1/3))[3])] <- rep(3, 50)

# cleaning ids (only odd numbers)
food$eventid <- seq(1, nrow(food) * 2, by = 2)

#-------------------------------------------

# Getting geolocations through ggmap

addresses <- c(zomato.breakfast.address[1:50], zomato.lunch.address[1:50], zomato.dinner.address[1:50])
addresses <- str_trim(addresses)

geolocations <- sapply(addresses, function(x) { geocode(x) })

food$longitude <- as.vector(sapply(seq(1, 150, 2), function(k) { geolocations[k] }))
food$latitude <- as.vector(sapply(seq(2, 150, 2), function(k) { geolocations[k] }))

class(food$longitude) <- "numeric"
class(food$latitude) <- "numeric"

#-----------------------------------------
# Writing the file

write.csv(food, "D:/iXperience/Projects/escape/food.csv", row.names = F)   

