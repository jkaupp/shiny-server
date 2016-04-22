library(shiny)

shinyUI(navbarPage("ONCAT Transferability Framework",
                   
  # Embedded googleform rendered server-side
  tabPanel("Evaluate Questions",
           htmlOutput("googleForm")),
  
  # Visualization dropdown
  navbarMenu("Visualizations",
             tabPanel("Question 1",
                      plotOutput("q1", height = 1000),
                      actionButton("refresh1", "Refresh Data")),
             
             tabPanel("Question 2",
                      plotOutput("q2", height = 1000),
                      actionButton("refresh2", "Refresh Data")),
             
             tabPanel("Question 3",
                      plotOutput("q3", height = 1000),
                      actionButton("refresh3", "Refresh Data"))
             
             )
              
))
  
