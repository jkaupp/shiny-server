
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(navbarPage("ONCAT Transferability Framework",

  # Embedded googleform rendered server-side
  tabPanel("Evaluate Questions",
           htmlOutput("googleForm")),
  
  # Visualization dropdown
  navbarMenu("Visualizations",
             tabPanel("Depth of Knowledge",
                      plotOutput("depth", height = 800),
                      actionButton("refresh", "Refresh Data")),
             tabPanel("Type of Knowledge",
                      plotOutput("type", height = 800),
                      actionButton("refresh", "Refresh Data")),
             tabPanel("Interdependence",
                      plotOutput("interdependence", height = 800),
                      actionButton("refresh", "Refresh Data"))
             )
              
))
  
