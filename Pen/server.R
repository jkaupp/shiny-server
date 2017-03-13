library(shiny)
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
  
  
  observe({
    toggle(condition = input$grasp_in, selector = "#pen_navbar li a[data-value=marks]")
    toggle(condition = input$grasp_in, selector = "#pen_navbar li a[data-value=comments]")
    toggleState("downloadMarkReport", condition = input$grasp_in)
    toggleState("downloadCommentReport", condition = input$grasp_in)
  })
  
  grasp_data <- reactive({read_csv(input$grasp_in[['datapath']], trim_ws = TRUE) %>% 
                           clean_names() %>%
                           remove_empty_rows()}) 
  
  mark_table <- reactive({make_tables(grasp_data(), type = "mark") })
  
  comment_table <- reactive({make_tables(grasp_data(), type = "comment")})
  
  output$DT_mark <- renderDataTable(mark_table() %>% 
                                      select_(.dots = list(quote(team_number)), sprintf("table%s", input$q_num )) %>% 
                                      filter(team_number == input$mark_team) %>% 
                                      unnest() %>% 
                                      select(-team_number) %>% 
                                      datatable(options = list(dom = 't'), rownames = FALSE) %>% 
                                      formatStyle('Ratings For',
                                                  target = 'row',
                                                  fontWeight = styleEqual("Average",'bold')))
  
   
  output$DT_comment <- renderDataTable(comment_table() %>% 
                                         select(team_number, comments) %>% 
                                         filter(team_number == input$comment_team) %>% 
                                         unnest() %>% 
                                         filter_(~Question == sprintf("q%s", input$comment_q_num)) %>% 
                                         select(-team_number) %>% 
                                         datatable(options = list(dom = 't'),  rownames = FALSE)
                                       )
                                       
  
  output$markteams <- renderUI({
    teams <- unique(grasp_data()$team)
    selectInput('mark_team', 'Teams', teams)
  })
  
  output$markquestions <- renderUI({

    questions <- stri_extract_all_regex(names(grasp_data()), "(q\\d+_mark)")
    
    qnumbers <-  stri_extract_all_regex(questions, "\\d+")
    
    selectInput('q_num', 'Question:', qnumbers[which(qnumbers != "NA")])
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
})
