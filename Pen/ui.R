library(shiny)
library(shinyjs)

shinyUI(
  navbarPage(tags$div(class = "pen_header", tagList(useShinyjs(),tags$span("Pen", class = "pen"), tags$span(": A reporting companion to GRASP", class = "pen_sub"))), header = tagList(includeCSS("skeleton.css"), includeCSS("normalize.css")), id = "pen_navbar", 
             
  tabPanel("Data",value = "data",
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput("grasp_in", "Select GRASP Results File:", accept = c("text/csv","text/comma-separated-values, text/plain", ".csv")),
                 downloadButton("downloadMarkReport", "Download Mark Report", class = "pen_button"),
                 tags$br(),
                 tags$br(),
                 downloadButton("downloadCommentReport", "Download Comment Report", class = "pen_button")
    ),
    mainPanel(width = 9)
  )),
  
 
  # Sidebar with a slider input for number of bins
  tabPanel("Marks",value = "marks",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput("markquestions"),
                          uiOutput("markteams")),
             mainPanel(width = 9,
                       dataTableOutput("DT_mark"))
           )
  ),
  tabPanel("Comments",value = "comments",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput("commentquestions"),
                          uiOutput("commentteams")
                          ),
             mainPanel(width = 9,
                       dataTableOutput("DT_comment"))
           )
  )
))
