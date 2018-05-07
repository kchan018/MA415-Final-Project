#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

boston_cuisine <- read.csv("Boston_Cuisine.csv")
chicago_cuisine <- read.csv("Chicago_Cuisine.csv")
dallas_cuisine <- read.csv("Dallas_Cuisine.csv")
sf_cuisine <- read.csv("San Francisco_Cuisine.csv")

# Define UI for application that draws graphs of most popular cuisines.
ui <- fluidPage(
   
   # Application title
   titlePanel("Most Popular Cuisines"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("city",
                     "City:",
                     choices = c("Boston", "Chicago", "Dallas", "San Francisco"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw graphs of most popular cuisines.
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     if (input$city == "Boston"){
       barplot(height = boston_cuisine[,2], 
               main = input$city,
               ylab = "Frequency",
               xlab = "Cuisine")
     }
     else if (input$city == "Chicago"){
       barplot(height = chicago_cuisine[,2], 
               main = input$city,
               ylab = "Frequency",
               xlab = "Cuisine")
     }
     else if (input$city == "Dallas"){
       barplot(height = dallas_cuisine[,2], 
               main = input$city,
               ylab = "Frequency",
               xlab = "Cuisine")
     }
     else {
       barplot(height = sf_cuisine[,2], 
               main = input$city,
               ylab = "Frequency",
               xlab = "Cuisine")
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

