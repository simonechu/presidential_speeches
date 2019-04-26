#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# Importing plots that I've made!

i1993_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/i1993_topten.rds")
i2001_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/i2001_topten.rds")
i2009_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/i2009_topten.rds")
i2017_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/i2017_topten.rds")
press_plot <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/press_plot.rds")
pressobama_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/pressobama_topten.rds")
presstrump_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/presstrump_topten.rds")
sotu2014_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/sotu2014_topten.rds")
sotu2015_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/sotu2015_topten.rds")
sotu2016_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/sotu2016_topten.rds")
sotu2018_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/sotu2018_topten.rds")
sotu2019_topten <- read_rds("~/Desktop/presidential_speeches/lets-get-presidential/sotu2019_topten.rds")

# Define UI for application
ui <- fluidPage(theme = shinytheme("lumen"),
   
   # Application title
   tabsetPanel(
     
     tabPanel("Inaugural Addresses",
              
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "ia",
                                         label = "Select an Inaugural Address",
                                         choices = c("Trump, 2017", "Obama, 2009", 
                                                     "Bush, 2001", "Clinton, 1993"),
                                         multiple = FALSE, 
                                         selected = "Trump, 2017")), 
                mainPanel(plotOutput("i_topten")))),
     
     tabPanel("State of the Union",
              
              mainPanel(
                h1("What makes a State of the Union speech?",
                   imageOutput("img"),
                   align = "center"
                ),
                h5("Simone Chu and Igor Morzan",
                   align = "center"))),
     
     tabPanel("Press Relations",
              
              mainPanel(
                h1("An Analysis of Presidential Press Relations",
                   imageOutput("img"),
                   align = "center"
                ),
                h5("Simone Chu and Igor Morzan",
                   align = "center"))),
     
     tabPanel("About", textOutput("message"))
   )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$i_topten <- renderPlot({
    if (input$ia == "Trump, 2017") {
      i2017_topten
      }
    
    else if (input$ia == "Obama, 2009") {
      i2009_topten
    }
    
    else if (input$ia == "Bush, 2001") {
      i2001_topten
    }
    
    else if (input$ia == "Clinton, 1993") {
      i1993_topten
    }
  }) 
  
  output$message <- renderUI({
    str0 <- paste(" ")
    str1 <- paste("About")
    str2 <- paste("This app, which compares presidential speeches to each other, was made for the Gov. 1005: Data course at Harvard.")
    str3 <- paste("The transcripts for each speech are from the UC Santa Barbara American Presidency Project, as it other data.")
    str4 <- paste("The code for this project can be found at https://github.com/simonechu/presidential_speeches")
    
    HTML(paste(tags$ul(str0, h3(str1, align = "center"), p(str2), p(str3), p(str4))))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

