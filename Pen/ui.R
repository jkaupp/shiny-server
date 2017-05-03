library(shiny)
library(shinyjs)

shinyUI(
  navbarPage(tags$div(class = "pen_header", tagList(useShinyjs(),tags$span("Pen", class = "pen"), tags$span(": A reporting companion to GRASP", class = "pen_sub"))), header = tagList(includeCSS("skeleton.css"), includeCSS("normalize.css")), id = "pen_navbar", windowTitle = "Pen: A reporting companion to GRASP",
             
  tabPanel("Data",value = "data",
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("survey_type", "What Survey?", list("Choose" = "", "APSC Peer Assessment" = "apsc","TeamQ Survey" = "teamq")),
                 fileInput("grasp_in", "Select GRASP Results File:", accept = c("text/csv","text/comma-separated-values, text/plain", ".csv")),
                 tags$hr(),
                 downloadButton("downloadMarkReport", "Download Mark Report", class = "pen_button"),
                 tags$br(),
                 tags$br(),
                 downloadButton("downloadCommentReport", "Download Comment Report", class = "pen_button"),
                 tags$br(),
                 tags$br(),
                 downloadButton("downloadTeamQReport", "Download TeamQ Report", class = "pen_button")
    ),
    mainPanel(width = 9, includeMarkdown("welcome.md"))
  )),
  
  
  tabPanel("Results", value = "results",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput("markquestions"),
                          uiOutput("markteams")),
             mainPanel(width = 9,
                       DT::dataTableOutput("DT_mark"))
           )
  ),
  tabPanel("Comments", value = "comments",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput("commentquestions"),
                          uiOutput("commentteams")
                          ),
             mainPanel(width = 9,
                       DT::dataTableOutput("DT_comment"))
           )
  )
))
