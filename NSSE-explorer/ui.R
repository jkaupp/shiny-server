library(shiny)
library(shinythemes)
library(rCharts)


# Define UI
shinyUI(

  # Define a navbarPage, embed a header image
  navbarPage(title = "Select View:", windowTitle = "NSEE Explorer",
    theme = shinytheme("cosmo"),
    
    
    
    # Program Dashboard Panel ----
    tabPanel("Program Dashboard",
             fluidRow(
               column(
                 width = 2,
                 
                 img(src = 'crest.png', width = "100%"),
                 
                 selectInput("department3", label = "Select Department", choices = department_list),
                 
                 checkboxGroupInput(
                   "compare_group3", label = "Select Comparison Institutions",
                   choices = institution_list, selected = institution_list
                 ),
                 
                 submitButton("Refesh Chart")
               ),
               column(width = 5, showOutput("spider_chart_fy", "highcharts")),
               column(width = 5, showOutput("spider_chart_sy", "highcharts"))
             )),
    
    # Program Overview Panel ----
    tabPanel("Program Overview",
             fluidRow(
               column(
                 width = 2,
                 
                 img(src = 'crest.png', width = "100%"),
      
                 selectInput("department", label = "Select Department", choices = department_list),
                 
                 checkboxGroupInput(
                   "compare_group", label = "Select Comparison Institutions",
                   choices = institution_list, selected = institution_list
                 ),
                 
                 submitButton("Refesh Chart"),
                 
                 br(),
                 
                 downloadButton("save_plot1", 'Save Chart')
               ),
               column(width = 10, plotOutput(
                 'plot1',  width = "100%", height = 1000 
               ))
             )),
    
    # Program Breakdown Panel ----
    tabPanel("Program Breadown",
             fluidRow(
               column(
                 width = 2,
                 
                 img(src = 'crest.png', width = "100%"),
                 
                 selectInput("department2", label = "Select Department", choices = department_list),
                 
                 selectInput(
                   "indicator", label = "Select Indicator", choices = c(
                     "Collaborative Learning" = "CL","Discussions With Diverse Others" = "DD","Effective Teaching Practises" =
                       "ET","Higher-Order Learning" = "HO","Learning Strategies" = "LS","Quality of Interaction" =
                       "QI","Quantitative Reasoning" = "QR","Reflective & Intergrative Learning" =
                       "RI","Supportive Environment" = "SE","Student-Faculty Interactions" = "SF"
                   )
                 ),
                 checkboxGroupInput(
                   "compare_group2", label = "Select Comparison Institutions",
                   choices = institution_list, selected = institution_list
                 ),
                 
                 submitButton("Refesh Chart"),
                 
                 br(),
                 
                 downloadButton("save_plot2", 'Save Chart')
               ),
               column(width = 10, plotOutput(
                 'plot2',  width = "100%", height = 1000
               ))
             ))
))


