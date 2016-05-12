library(shiny)

shinyUI(navbarPage("ONCAT Transferability Framework", theme = "oncat.css", 
                   
  # Embedded googleform rendered server-side
  tabPanel("Evaluate Questions",
           htmlOutput("googleForm")),
  
  # Visualization dropdown
  navbarMenu("Visualizations",
             
             tabPanel("Question 1",
                      plotOutput("q1", height = 1000)
                      ),
             
             tabPanel("Question 2",
                      plotOutput("q2", height = 1000)
                      ),
             
             tabPanel("Question 3",
                      plotOutput("q3", height = 1000)
                      )
             
             )
))
  
