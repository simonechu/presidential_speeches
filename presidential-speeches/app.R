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

# Importing plots that I've made (of which there are many)! 

i1993_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i1993_topten.rds")
i2001_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2001_topten.rds")
i2009_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2009_topten.rds")
i2017_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2017_topten.rds")
press_plot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/press_plot.rds")
pressobama_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/pressobama_topten.rds")
presstrump_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/presstrump_topten.rds")
sotu2014_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2014_topten.rds")
sotu2015_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2015_topten.rds")
sotu2016_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2016_topten.rds")
sotu2018_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2018_topten.rds")
sotu2019_topten <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2019_topten.rds")
i1993_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i1993_ot.rds")
i2001_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2001_ot.rds")
i2009_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2009_ot.rds")
i2017_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/i2017_ot.rds")
pressobama_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/pressobama_ot.rds")
presstrump_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/presstrump_ot.rds")
sotu2014_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2014_ot.rds")
sotu2015_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2015_ot.rds")
sotu2016_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2016_ot.rds")
sotu2018_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2018_ot.rds")
sotu2019_ot <- read_rds("~/Desktop/presidential_speeches/presidential.speeches/sotu2019_ot.rds")

# Define UI for application
ui <- fluidPage(theme = shinytheme("lumen"),
   
   # Application title
   tabsetPanel(
     
     tabPanel("Inaugural Addresses",
              
              h1("What makes an Inaugural Address?"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "ia",
                                         label = "Select an Inaugural Address",
                                         choices = c("Trump, 2017", "Obama, 2009", 
                                                     "Bush, 2001", "Clinton, 1993"),
                                         multiple = FALSE, 
                                         selected = "Trump, 2017"),
                             h4("Notes"),
                             textOutput("i_analysis")),
                mainPanel(plotOutput("i_topten"), plotOutput("i_ot")))),
     
     tabPanel("State of the Union",
              
              h1("What makes a State of the Union speech?"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "sotu",
                                         label = "Select a Year",
                                         choices = c("Trump, 2019", "Trump, 2018", 
                                                     "Obama, 2016", "Obama, 2015",
                                                     "Obama, 2014"),
                                         multiple = FALSE, 
                                         selected = "Trump, 2019")), 
                mainPanel(plotOutput("sotu_topten"), plotOutput("sotu_ot")))),
     
     tabPanel("Press Relations",
              
              h1("An Analysis of Presidential Press Relations"),
              mainPanel(
                h3("President Obama's Press Conference - Jan. 18, 2017"),
                plotOutput("ot_bo"),
                h3("President Trump's Press Conference - Feb. 16, 2017"), 
                plotOutput("ot_dt"), 
                h4("Trump hosted fewer press conferences than Obama did during their respective first years in office. However, Trump is by no means the most difficult-to-access president that the press has seen..."),
                plotOutput("pressconfs"),
                   align = "center")),
     
     tabPanel("About", htmlOutput("message"))
   )

)

# Define server logic
server <- function(input, output) {
  
  output$i_analysis <- renderText({
    if (input$ia == "Trump, 2017") {
      "'Great' was the most frequently-appearing word in Trump's Inaugural Address, and a part of his campaign motto to 'Make America Great Again'. This Inaugural Address was 1433 words long."
    }
    
    else if (input$ia == "Obama, 2009") {
      "Obama first took office during the Great Recession, which may be why the word 'crisis' appears so often in his Inaugural Address -- as well as his emphasis on 'hard work'. This Inaugural Address was 2395 words long."
    }
    
    else if (input$ia == "Bush, 2001") {
      "The ten most frequently used words in Bush's Inaugural Address were all coded as positive. This Inaugural Address was 1592 words long."
    }
    
    else if (input$ia == "Clinton, 1993") {
      "The language used over the duration of Clinton's Inaugural Address was overwhelmingly positive. This Inaugural Address was 1598 words long."
    }
  })
   
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
  
  output$i_ot <- renderPlot({
    if (input$ia == "Trump, 2017") {
      i2017_ot
    }
    
    else if (input$ia == "Obama, 2009") {
      i2009_ot
    }
    
    else if (input$ia == "Bush, 2001") {
      i2001_ot
    }
    
    else if (input$ia == "Clinton, 1993") {
      i1993_ot
    }
  })
  
  output$sotu_topten <- renderPlot({
    if (input$sotu == "Trump, 2019") {
      sotu2019_topten
    }
    
    else if (input$sotu == "Trump, 2018") {
      sotu2018_topten
    }
    
    else if (input$sotu == "Obama, 2016") {
      sotu2016_topten
    }
    
    else if (input$sotu == "Obama, 2015") {
      sotu2015_topten
    }
    
    else if (input$sotu == "Obama, 2014") {
      sotu2014_topten
    }
  })
  
  output$sotu_ot <- renderPlot({
    if (input$sotu == "Trump, 2019") {
      sotu2019_ot
    }
    
    else if (input$sotu == "Trump, 2018") {
      sotu2018_ot
    }
    
    else if (input$sotu == "Obama, 2016") {
      sotu2016_ot
    }
    
    else if (input$sotu == "Obama, 2015") {
      sotu2015_ot
    }
    
    else if (input$sotu == "Obama, 2014") {
      sotu2014_ot
    }
  })
  
  output$pressconfs <- renderPlot({
    
    press_plot %>%
    ggplot(aes(x = president, y = number, fill = party)) + 
      geom_col() +
      scale_fill_manual(values=colors) + 
      xlab("President") + 
      ylab("Number of Press Conferences in First Year") + 
      labs(title = "Number of Press Conferences Held in First Year of Presidency",
           fill = "Party") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ot_bo <- renderPlot({
    
    pressobama_ot
    
  })
  
  output$ot_dt <- renderPlot({
    
    presstrump_ot
    
  })
  
  output$message <- renderUI({
    str0 <- paste(" ")
    str1 <- paste("About")
    str2 <- paste("This app, which compares presidential speeches to each other, was made for the Gov. 1005: Data course at Harvard.")
    str3 <- paste("The transcripts for each speech are from the UC Santa Barbara American Presidency Project, as is other data on press conferences and speech duration.")
    str4 <- paste("The code for this project can be found at https://github.com/simonechu/presidential_speeches")
    
    HTML(paste(tags$ul(str0, h3(str1, align = "center"), p(str2), p(str3), p(str4))))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

