library(shiny)
library(shinyjs)
library(rmarkdown)
library(janitor)
library(magrittr)
library(readr)
library(stringi)
library(tidyverse)
library(DT)

source("pen.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Set hidden/visible elements 
  observe({toggleState("grasp_in", condition = input$survey_type != "")})
  
  observe({
    if (is.null(input$grasp_in)) {
      hide(selector = "#pen_navbar li a[data-value=results]")
      hide(selector = "#pen_navbar li a[data-value=comments]")
      hide("downloadMarkReport")
      hide("downloadCommentReport")
      hide("downloadTeamQReport")
    } else if (!is.null(input$grasp_in) && input$survey_type != "apsc") {
      show(selector = "#pen_navbar li a[data-value=results]")
      show("downloadTeamQReport")
    } else {
      show(selector = "#pen_navbar li a[data-value=results]")
      show(selector = "#pen_navbar li a[data-value=comments]")
      show("downloadMarkReport")
      show("downloadCommentReport")
    }
    })
  
   observe({
    toggleState("downloadMarkReport", condition = input$grasp_in)
    toggleState("downloadCommentReport", condition = input$grasp_in)
  })
  
  grasp_data <- reactive({read_csv(input$grasp_in[['datapath']], trim_ws = TRUE) %>% 
                           clean_names() %>%
                           remove_empty_rows()}) 
  
  
  
  observe({if (input$survey_type == "apsc") {
    
    mark_table <- reactive({make_tables(grasp_data(), type = "mark", survey = input$survey_type) })
    
    comment_table <- reactive({make_tables(grasp_data(), type = "comment", survey = input$survey_type)})
    
    output$DT_mark <- DT::renderDataTable(mark_table() %>% 
                                            select_(.dots = list(quote(team_number)), sprintf("table%s", input$q_num )) %>% 
                                            filter(team_number == input$mark_team) %>% 
                                            unnest() %>% 
                                            select(-team_number) %>% 
                                            DT::datatable(options = list(dom = 't'), rownames = FALSE) %>% 
                                            formatStyle('Ratings For',
                                                        target = 'row',
                                                        fontWeight = styleEqual("Average",'bold')))
    
    
    output$DT_comment <- DT::renderDataTable(comment_table() %>% 
                                               select(team_number, comments) %>% 
                                               filter(team_number == input$comment_team) %>% 
                                               unnest() %>% 
                                               filter_(~Question == sprintf("q%s", input$comment_q_num)) %>% 
                                               select(-team_number) %>% 
                                               DT::datatable(options = list(dom = 't'),  rownames = FALSE)
    )
    
  } else {
    
    mark_table <- reactive({make_tables(grasp_data(), survey = input$survey_type)})
    
    output$DT_mark <- DT::renderDataTable(mark_table() %>% 
                                            filter(team_number == input$mark_team, scales == input$scale) %>% 
                                            select(-team_number, -question, -scales) %>% 
                                            spread(items, value) %>% 
                                            DT::datatable(options = list(dom = 't'), rownames = FALSE)) 
  }})
  
  
## Generated UI Elements ----
  output$markteams <- renderUI({
    teams <- unique(grasp_data()$team)
    selectInput('mark_team', 'Teams', teams)
  })
  
  output$markquestions <- renderUI({

    if (input$survey_type == "teamq") {
      selectInput('scale', 'TeamQ Scale:', c("Contributes to Team Project", "Facilitates Contibutions of Others", "Planning and Management", "Fosters a Team Climate", "Manages Potential Conflict","Suggestions to Improve"))
    } else {
    
    questions <- stri_extract_all_regex(names(grasp_data()), "(q\\d+_mark)")
    
    qnumbers <-  stri_extract_all_regex(questions, "\\d+")
    
    selectInput('q_num', 'Question:', qnumbers[which(qnumbers != "NA")])
    }
  })
  
  output$commentteams <- renderUI({
    teams <- unique(grasp_data()$team)
    selectInput('comment_team', 'Teams', teams)
  })
  
  
  output$commentquestions <- renderUI({
    
    questions <- stri_extract_all_regex(names(grasp_data()), "(q\\d+_comment)")
    
    qnumbers <-  stri_extract_all_regex(questions, "\\d+")
    
    selectInput('comment_q_num', 'Question:', qnumbers[which(qnumbers != "NA")])
  })
  
  output$buttons <- renderUI({
    
    if (input$survey_type == "apsc" & !is.null(input$grasp_in)) {
      list(downloadButton("downloadMarkReport", "Download Results Report", class = "pen_button"), 
           tags$br(),
           tags$br(),
      downloadButton("downloadCommentReport", "Download Comment Report", class = "pen_button"))
    } else if (input$survey_type == "teamq" & !is.null(input$grasp_in)) {
      list(actionButton("processTeamQReport", "Process Team Q Report", class = "pen_button"),
           tags$br(),
           tags$br(),
      downloadButton("downloadTeamQReport", "Download Team Q Report", class = "pen_button"))
    }
  })
  
## Download Handlers ----  
  output$downloadMarkReport <- downloadHandler(
    filename = reactive(sprintf("GRASP Mark report - %s.pdf", tools::file_path_sans_ext(input$grasp_in[['name']]))),
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "grasp_report_template.Rmd")
      
      file.copy("grasp_report_template.Rmd", tempReport, overwrite = TRUE)
      
      parameters <- list(type = "mark")
      
      # Use rmarkdown::render to produce a pdf report
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = parameters,
                        clean = TRUE)
      
    }
  )
  
  
  output$downloadCommentReport <- downloadHandler(
    filename = reactive(sprintf("GRASP Comment report - %s.pdf", tools::file_path_sans_ext(input$grasp_in[['name']]))),
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "grasp_report_template.Rmd")
      
      file.copy("grasp_report_template.Rmd", tempReport, overwrite = TRUE)
      
      parameters <- list(type = "comment")
      
      # Use rmarkdown::render to produce a pdf report
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = parameters,
                        clean = TRUE)
      
    }
  )
  
  output$downloadTeamQReport <- downloadHandler(
    filename = function() {
      paste0("TeamQ", ".zip")
    },
    content = function(file) {
      
       zip(file, list.files(getwd(), pattern = "pdf"), flags = "-jm") 
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$processTeamQReport, {
    
    toggleState("downloadTeamQReport")
    
    ratings <- split(grasp_data(), grasp_data()[["reviewee_id"]])
    
    tempReport <- "teamq_report_template.Rmd"
    
    # file.copy("teamq_report_template.Rmd", tempReport, overwrite = TRUE)
    
    # Use rmarkdown::render to produce a pdf report
    # Need to make a custom function to wrap a progress bar within walk.
    # 
    
    renderBar <- function(x, y, i){
      
      withProgress(message = sprintf("Building Student Report %s of %s", x, length(ratings)), min = 1, max = length(ratings), value = x, {
      rmarkdown::render(tempReport,
                        output_file = sprintf("%s.pdf", y),
                        params = list(data = sprintf("ratings[[%s]]", x)),
                        clean = TRUE)
      
    })}
    
    walk2(seq_along(ratings), names(ratings), ~renderBar(.x, .y))

    on.exit(toggleState("downloadTeamQReport"))
    
  })
  
})
