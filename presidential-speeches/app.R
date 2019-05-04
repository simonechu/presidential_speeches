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
library(gt)
library(tidyverse)

# Importing plots that I've made (of which there are many)! 

i1993_topten <- read_rds("./i1993_topten.rds")
i1997_topten <- read_rds("./i1997_topten.rds")
i2001_topten <- read_rds("./i2001_topten.rds")
i2005_topten <- read_rds("./i2005_topten.rds")
i2009_topten <- read_rds("./i2009_topten.rds")
i2013_topten <- read_rds("./i2013_topten.rds")
i2017_topten <- read_rds("./i2017_topten.rds")
press_plot <- read_rds("./press_plot.rds")
pressobama_topten <- read_rds("./pressobama_topten.rds")
presstrump_topten <- read_rds("./presstrump_topten.rds")
sotu2014_topten <- read_rds("./sotu2014_topten.rds")
sotu2015_topten <- read_rds("./sotu2015_topten.rds")
sotu2016_topten <- read_rds("./sotu2016_topten.rds")
sotu2018_topten <- read_rds("./sotu2018_topten.rds")
sotu2019_topten <- read_rds("./sotu2019_topten.rds")
i1993_ot <- read_rds("./i1993_ot.rds")
i1997_ot <- read_rds("./i1997_ot.rds")
i2001_ot <- read_rds("./i2001_ot.rds")
i2005_ot <- read_rds("./i2005_ot.rds")
i2009_ot <- read_rds("./i2009_ot.rds")
i2013_ot <- read_rds("./i2013_ot.rds")
i2017_ot <- read_rds("./i2017_ot.rds")
pressobama_ot <- read_rds("./pressobama_ot.rds")
presstrump_ot <- read_rds("./presstrump_ot.rds")
sotu2014_ot <- read_rds("./sotu2014_ot.rds")
sotu2015_ot <- read_rds("./sotu2015_ot.rds")
sotu2016_ot <- read_rds("./sotu2016_ot.rds")
sotu2018_ot <- read_rds("./sotu2018_ot.rds")
sotu2019_ot <- read_rds("./sotu2019_ot.rds")
fwbush_topten <- read_rds("./fwbush_topten.rds")
fwclinton_topten <- read_rds("./fwclinton_topten.rds")
fwobama_topten <- read_rds("./fwobama_topten.rds")
fwbush_ot <- read_rds("./fwbush_ot.rds")
fwclinton_ot <- read_rds("./fwclinton_ot.rds")
fwobama_ot <- read_rds("./fwobama_ot.rds")
obama_approval <- read_rds("./obama_approval.rds")
bush_approval <- read_rds("./bush_approval.rds")
clinton_approval <- read_rds("./clinton_approval.rds")

# Define UI for application
# I picked this theme because it looked nice! Yay themes! 
ui <- fluidPage(theme = shinytheme("lumen"),
   
   # Application title
   tabsetPanel(
     
     tabPanel("Inaugural Addresses",
              
              h1("What Makes an Inaugural Address?"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "ia",
                                         label = "Select an Inaugural Address",
                                         choices = c("Trump, 2017", "Obama, 2013", "Obama, 2009", 
                                                     "Bush, 2005", "Bush, 2001", 
                                                     "Clinton, 1997", "Clinton, 1993"),
                                         multiple = FALSE, 
                                         selected = "Trump, 2017"),
                             h4("Notes"),
                             textOutput("i_analysis")),
                
                # I had the selector appear in the sidebar, because that made the most sense. 
                # I also thought it would be cool to include a note or two in the sidebar with
                # each plot, just to add a bit of context. 
                
                mainPanel(plotOutput("i_topten"), plotOutput("i_ot"), 
                          h3("Takeaways"), textOutput("ia_takeaways")))),
     
                # These are the graphs I wanted to include. 
     
     tabPanel("State of the Union",
              
              h1("What Makes a State of the Union Speech?"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "sotu",
                                         label = "Select a Year",
                                         choices = c("Trump, 2019", "Trump, 2018", 
                                                     "Obama, 2016", "Obama, 2015",
                                                     "Obama, 2014"),
                                         multiple = FALSE, 
                                         selected = "Trump, 2019")), 
                mainPanel(plotOutput("sotu_topten"), plotOutput("sotu_ot"),
                          h5("SOTU speeches were markedly less positive than Inaugural Addresses overall.")))),
          
     tabPanel("Press Relations",
              
              h1("A Brief Analysis of Presidential Press Relations"),
              mainPanel(
                h3("President Obama's Press Conference - Jan. 18, 2017"),
                plotOutput("ot_bo"),
                h5(" "),
                h3("President Trump's Press Conference - Feb. 16, 2017"), 
                plotOutput("ot_dt"), 
                h5(" "),
                h4("Trump participated in fewer news conferences than Obama did during their respective first years in office. However, Trump is by no means the most difficult-to-access president that the press has seen..."),
                plotOutput("pressconfs"),
                h5(" "),
                h4("...that particular honor goes to his predecessors Gerald Ford and Ronald Reagan."),
                   align = "center")),
     
     tabPanel("Closing Words in Office", 
              
              h1("Closing Words in Office"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = "farewell",
                                         label = "Select a President",
                                         choices = c("Barack Obama", "George W. Bush",
                                                     "Bill Clinton"),
                                         multiple = FALSE, 
                                         selected = "Barack Obama")), 
                mainPanel(h3("Sentiment Analysis of Farewell Address"),
                  plotOutput("fw_topten"), 
                  plotOutput("fw_ot"),
                  h3("Approval Ratings at Start and End of Presidency"),
                  tableOutput("approv"),
                  h1(" "),
                  h3("Closing Words of Farewell Address"),
                  h1(" "),
                  h5(textOutput("closing"))))),
     
     tabPanel("About",
              h1("About"), tags$p("This app, which compares presidential speeches to each other, was made for the", tags$a("Gov. 1005: Data", href = "https://www.davidkane.info/files/gov_1005_spring_2019.html"), "course at Harvard. The transcripts for each speech are from the", tags$a("UC Santa Barbara American Presidency Project,", href = "https://www.presidency.ucsb.edu/"), "as are other data on press conferences and speech duration. The code for this project can be found", tags$a("here.", href="https://github.com/simonechu/presidential_speeches"), "Thanks for reading! - Simone Chu"))
   )
          # I used the tags$p to include text instead of assigning it an output because this was
          # the best way I could find to include hyperlinks in text. 
)

# Define server logic
server <- function(input, output) {
  
          # Since my plots were already made, it was simple to program the server to fetch
          # each relevant plot based on the selected option. 
  
  output$i_analysis <- renderText({
    if (input$ia == "Trump, 2017") {
      "'Great' was the most frequently-appearing word in Trump's Inaugural Address, and a part of his campaign motto to 'Make America Great Again'. This Inaugural Address was 1433 words long."
    }
    
    else if (input$ia == "Obama, 2013") {
      "Obama's second Inaugural Address was markedly more positive overall compared to his first Inaugural Address. This speech was 2096 words long."
    }
    
    else if (input$ia == "Obama, 2009") {
      "Obama first took office during the Great Recession, which may be why the word 'crisis' appears so often in his Inaugural Address -- as well as his emphasis on 'hard work'. This Inaugural Address was 2395 words long."
    }
    
    else if (input$ia == "Bush, 2005") {
      "This speech was 2071 words long."
    }
    
    else if (input$ia == "Bush, 2001") {
      "The ten most frequently used words in Bush's Inaugural Address were all coded as positive. This Inaugural Address was 1592 words long."
    }
    
    else if (input$ia == "Clinton, 1997") {
      "This speech was 2155 words long."
    }
    
    else if (input$ia == "Clinton, 1993") {
      "The language used over the duration of Clinton's Inaugural Address was overwhelmingly positive. This Inaugural Address was 1598 words long."
    }
  })
   
  output$i_topten <- renderPlot({
    if (input$ia == "Trump, 2017") {
      i2017_topten
      }
    
    else if (input$ia == "Obama, 2013") {
      i2013_topten
    }
    
    else if (input$ia == "Obama, 2009") {
      i2009_topten
    }
    
    else if (input$ia == "Bush, 2005") {
      i2005_topten
    }
    
    else if (input$ia == "Bush, 2001") {
      i2001_topten
    }
    
    else if (input$ia == "Clinton, 1997") {
      i1997_topten
    }
    
    else if (input$ia == "Clinton, 1993") {
      i1993_topten
    }
  }) 
  
  output$i_ot <- renderPlot({
    if (input$ia == "Trump, 2017") {
      i2017_ot
    }
    
    else if (input$ia == "Obama, 2013") {
      i2013_ot
    }
    
    else if (input$ia == "Obama, 2009") {
      i2009_ot
    }
    
    else if (input$ia == "Bush, 2005") {
      i2005_ot
    }
    
    else if (input$ia == "Bush, 2001") {
      i2001_ot
    }
    
    else if (input$ia == "Clinton, 1997") {
      i1997_ot
    }
    
    else if (input$ia == "Clinton, 1993") {
      i1993_ot
    }
  })
  
  output$ia_takeaways <- renderText({
    "The one thing each speech has in common: it ends on a positive note, which makes sense -- no one wants to start their presidency off on a downer."
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
    press_plot
  })
  
  output$ot_bo <- renderPlot({
    
    pressobama_ot
    
  })
  
  output$ot_dt <- renderPlot({
    
    presstrump_ot
    
  })
  
  output$fw_topten <- renderPlot({
    if  (input$farewell == "Barack Obama") {
      fwobama_topten
    }
    
    else if (input$farewell == "George W. Bush") {
      fwbush_topten
    }
    
    else if (input$farewell == "Bill Clinton") {
      fwclinton_topten
    }
  })
  
  output$fw_ot <- renderPlot({
    if (input$farewell == "Barack Obama") {
      fwobama_ot
    }
    
    else if (input$farewell == "George W. Bush") {
      fwbush_ot
    }
    
    else if (input$farewell == "Bill Clinton") {
      fwclinton_ot
    }
  })
  
  output$approv <- render_gt({
    if (input$farewell == "Barack Obama") {
      obama_approval
    }
    
    else if (input$farewell == "George W. Bush") {
      bush_approval
    }

    else if (input$farewell == "Bill Clinton") {
      clinton_approval
    }
  })
  
  output$closing <- renderText({
    if (input$farewell == "Barack Obama") {
      "Yes, we did. Yes, we can. Thank you. God bless you. May God continue to bless the United States of America. Thank you."
    }
    
    else if (input$farewell == "George W. Bush") {
      "And so, my fellow Americans, for the final time, good night. May God bless this house and our next President, and may God bless you and our wonderful country. Thank you."
    }
    
    else if (input$farewell == "Bill Clinton") {
      "My days in this office are nearly through, but my days of service, I hope, are not. In the years ahead, I will never hold a position higher or a covenant more sacred than that of President of the United States. But there is no title I will wear more proudly than that of citizen. Thank you. God bless you, and God bless America."
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

