# load libraries
library(xml2)  
library(rvest)
library(stringr)
library(lubridate)

library(ggplot2)

library(RMySQL)


# get the prices 
# two functions - 1. IPhone, 2.AirPods 


getIPhone <- function(){
  
  getSite_iphone <- read_html("https://www.apple.com/de/shop/buy-iphone/iphone-12-pro/6,1%22-display-256gb-graphit")
  
  getPrice <- getSite_iphone %>%
    html_nodes("span.as-price-currentprice") %>%  #span = tag name, as-price-currentprice = class name
    html_text()
  getPrice <- str_remove_all(getPrice, "[\n]") 
  getPrice <- gsub(" ","", getPrice)
  getPrice <- gsub(".","",getPrice, fixed=TRUE) #fixed=TRUE so that only the "." is replaced
  getPrice <- str_replace_all(getPrice, ",\\w+", "") #remove all character after "," included, so that also the "€" sign is removed
  getPrice <- substr(getPrice, 1,nchar(getPrice)-1)
  getPrice_iphone <- as.integer(getPrice)
  
  
  
  return(getPrice_iphone)

}



getAirPod <- function(){
  
  getSite_airpod <- read_html("https://www.apple.com/de/shop/product/MV7N2ZM/A/airpods-mit-ladecase")
  
  getPrice <- getSite_airpod %>%
    html_nodes("span.current_price") %>%
    html_text()
  
  
  getPrice <- str_remove_all(getPrice, "[\n]") 
  getPrice <- gsub(" ","", getPrice)
  getPrice <- str_replace_all(getPrice, ",\\w+", "") #remove all character after "," included
  getPrice <- substr(getPrice, 1,nchar(getPrice)-1)
  getPrice_airpod <- as.integer(getPrice)
  
  return(getPrice_airpod)
  
}

# the function only creates a data.frame that stores the current date, as well as the year
# later these two values are combined with product name and the price of the product into a data.frame that is finally store in the MySQL database
getTime <- function(){
  
  
  YearMonthDate <- Sys.Date()
  Year <- year(YearMonthDate)
  inputtable <- as.data.frame(cbind(Year,YearMonthDate))
  inputtable$YearMonthDate <-  as.numeric(inputtable$YearMonthDate)
  inputtable$YearMonthDate <- format(as.Date(inputtable$YearMonthDate, origin="1970-01-01"), "%Y:%m:%d")
  
  return(inputtable)
  
  
}


# Database access (save to database and load from database)

table <- "PriceTracker" #name of the table in the database 



saveData <- function(data){
  
  #connect to database
  
  db <- dbConnect(MySQL(), dbname = Sys.getenv("databaseName"),  
                  host = Sys.getenv("host"),  
                  port = as.integer(Sys.getenv("port")), 
                  user = Sys.getenv("user"),  
                  password = Sys.getenv("password"))
  
  #######################################################################################
  # The database credentials are stored in a .Renviron file that was also deployed to 
  # the shinyapps.io server. 
  # When testing locally the .Renviron file was project specific -> usethis::edit_r_environ("project") 
  ##################################
  
  
  # construct update query by looping over the data fields
  
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')", 
    
    table, # 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")    
    
  )
  
  # Submit the update query and disconnect
  
  dbGetQuery(db, query)
  
  dbDisconnect(db)
  
}


loadData <- function(){
  
  # connect to the database
  db <- dbConnect(MySQL(), dbname = Sys.getenv("databaseName"), 
                  host = Sys.getenv("host"),  
                  port = as.integer(Sys.getenv("port")), 
                  user =  Sys.getenv("user"), 
                  password = Sys.getenv("password")) 
  
  # construct the fetching query 
  
  query <- sprintf("SELECT * FROM %s", table)
  
  # Submit the fetch query and disconnect
  
  data <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  
  return(data)
  
}



# the plot functions - these are later executed once the user in the app hits the "plot button"
# the data input for the plot functions is the output of the loadData function / the data from the MySQL database 

plotAirPods <- function(input){
  
  
  input1 <- input[which(input[,"ProductName"] == "AirPods mit Ladecase"),]
  #prepare data
  ggplot(data = input1, aes(x = YearMonthDate, y = Price, group = 1)) + geom_line()
  
  
}

plotIPhone <- function(input){
  
  input1 <- input[which(input[,"ProductName"] == "Apple IPhone 12 Pro 256 GB"),]
  
  ggplot(data = input1, aes(x = YearMonthDate, y = Price, group = 1)) + geom_line()
  
}



