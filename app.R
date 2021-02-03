# r-shiny project
library(shiny) 

source("./global.R") #relative path as otherwise the file cannot be found when deploying to shinyapps.io


# Define UI / Layout 
# fluidpage -> function
ui <- fluidPage(
  
  titlePanel(""),
  

  
  sidebarLayout(
    
    sidebarPanel("Choose the product you wish to get the price for: ", 
                 
                 br(),
                 br(),
                 selectInput("productID","Choose product: ", c("Apple IPhone 12 Pro 256 GB", "AirPods mit Ladecase")),
                 actionButton("getNewPrice", "Load current Price"),
                 br(),
                 br(),
                 actionButton("plotData", "Plot Data"),
                 br(), 
                 h1(textOutput("currentPrice"), align = "center")), 
                 
               
    
    mainPanel(p("Prices (€) over Time", align = "center"), tabsetPanel(type = "tabs", 
                          tabPanel("IPhone 12 Pro - 256 GB", plotOutput("plotIPhone")),
                          tabPanel("AirPods mit Ladecase", plotOutput("plotAirPods"))),
              
              position = "left" #sidebar position
              
         
        
  ))
  
)


# Define server logic required 
server <- function(input, output) {
  
  tableInput <- loadData() #as to my knowledge loading the data at this point, loads the data everytime a user joins the app
                          #I got to be honest, I don't know if it is better to place it in the global.R app, that is supposed to only run once the app launches (as I have no seperate ui.R and server.R files)
  
  # if the user pushes the "Get newest Price" Button - newest price is extracted 
  # and written to remote database + current price is displayed (however without € sing. 
  observeEvent(input$plotData, {
    
    
    
    
    if(input$productID == "Apple IPhone 12 Pro 256 GB"){
      
      output$plotIPhone <- renderPlot(plotIPhone(tableInput))
      
    }
    else
    {
    
     output$plotAirPods <- renderPlot(plotAirPods(tableInput))
    }
    
   # output$datas <- renderTable(tableInput)
    
    
  })
  
  
  
  observeEvent(input$getNewPrice, {
    
    
    # first we scrape the price from the homepage
    # thereafter we get the current date and combine these two inputs
    # the data frame that is then written to the remote database
    
    if(input$productID == "Apple IPhone 12 Pro 256 GB"){
      
      Price <- getIPhone() # scrape IPhone Price
      
    }else{
      
      Price <- getAirPod()
    }
    
    times <- getTime() 
 
    ProductName <- input$productID
    all_frame <- as.data.frame(cbind(ProductName, Price, times))
    
    output$currentPrice <- renderText({
      
      print(Price)
      
    })
    
    saveData(all_frame)
    
    
  })
  
  
  
 
  
}


# run the app 
shinyApp(ui = ui, server = server)

