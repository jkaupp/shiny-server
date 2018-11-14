library(shiny)
library(shinyjs)
library(rmarkdown)
library(janitor)
library(magrittr)
library(readr)
library(stringi)
library(stringr)
library(viridis)
library(purrr)
library(tidyr)
library(tidytext)
library(DT)
library(grid)
library(gridExtra)
library(gtable)
library(forcats)
library(ggplot2)
library(RColorBrewer)
library(Cairo)
library(ggradar)
library(showtext)
library(plyr)
library(dplyr)

source("pen.R")

shinyServer(function(input, output) {
  
  #Set hidden/visible elements  ----
  observe({toggleState("grasp_in", condition = input$survey_type != "")})
  
  observe({
    if (is.null(input$grasp_in)) {
      hide(selector = "#pen_navbar li a[data-value=results]")
      hide(selector = "#pen_navbar li a[data-value=comments]")
    } else if (!is.null(input$grasp_in) & input$survey_type != "apsc") {
      show(selector = "#pen_navbar li a[data-value=results]")
      show("downloadTeamQReport")
    } else {
      show(selector = "#pen_navbar li a[data-value=results]")
      show(selector = "#pen_navbar li a[data-value=comments]")
    }
    })
  
  #  observe({
  #   toggleState("processMarkReport", condition =  input$grasp_in)
  #   toggleState("processCommentReport", condition = input$grasp_in)
  # })
  
  
  ## Data Ingest ----
  grasp_data <- reactive({read_csv(input$grasp_in[['datapath']], trim_ws = TRUE) %>% 
                           clean_names() %>%
                           remove_empty("rows")}) 
  
  file_name <- reactive({tools::file_path_sans_ext(input$grasp_in[['name']])})
  
  
  observe({if (input$survey_type == "apsc") {
    
    mark_table <- reactive({make_tables(grasp_data(), type = "mark", survey = input$survey_type) })
    
    comment_table <- reactive({make_tables(grasp_data(), type = "comment", survey = input$survey_type)})
    
    output$DT_mark <- DT::renderDataTable(mark_table() %>% 
                                            select_(.dots = list(quote(team_number)), sprintf("table%s", input$q_num )) %>% 
                                            filter(team_number == input$mark_team) %>% 
                                            unnest() %>% 
                                            ungroup() %>% 
                                            select(-team_number) %>% 
                                            DT::datatable(options = list(dom = 'pt'), rownames = FALSE) %>% 
                                            formatStyle('Ratings For',
                                                        target = 'row', 
                                                        fontWeight = styleEqual(c("Average","Adjusted Average", "Base Adjustment"),rep('bold',3)))) 
    
    output$DT_comment <- DT::renderDataTable(comment_table() %>% 
                                               select(team_number, comments) %>% 
                                               filter(team_number == input$comment_team) %>% 
                                               unnest() %>% 
                                               ungroup() %>% 
                                               filter_(~Question == sprintf("q%s", input$comment_q_num)) %>% 
                                               select(-team_number) %>% 
                                               DT::datatable(options = list(dom = 'pt'),  rownames = FALSE)
    ) 
    
    
  } else {
    
    mark_table <- reactive({make_tables(grasp_data(), survey = input$survey_type)})
    
    
    output$DT_mark <- DT::renderDataTable(mark_table() %>% 
                                            filter(team_number == input$mark_team, scales == input$scale) %>% 
                                            select(-team_number, -question, -scales) %>% 
                                            spread(items, value) %>% 
                                            DT::datatable(options = list(dom = 'pt'), rownames = FALSE))
    
  }})
  
  
## Generated UI Elements ----
  output$markteams <- renderUI({
    teams <- unique(grasp_data()$team)
    selectInput('mark_team', 'Teams', teams)
  })
  
  
  output$buttons <- renderUI({
    if (input$survey_type == "apsc" & !is.null(input$grasp_in)) {
      list(actionButton("processMarkReport", "Build Results Report", class = "pen_button"),
           hidden(downloadButton("downloadMarkReport", "Download Results Report", class = "pen_button")),
           tags$br(),
           tags$br(),
           actionButton("processCommentReport", "Build Comment Report", class = "pen_button"),
           hidden(downloadButton("downloadCommentReport", "Download Comment Report", class = "pen_button")))
    } else if (input$survey_type == "teamq_student" & !is.null(input$grasp_in)) {
      list(actionButton("processTeamQReport", "Build Team Q Report", class = "pen_button"),
           tags$br(),
           tags$br(),
           hidden(downloadButton("downloadTeamQReport", "Download Team Q Report", class = "pen_button")))
    } else if (input$survey_type == "teamq_diagnostic" & !is.null(input$grasp_in)) {
      list(downloadButton("downloadTeamQdiagnostic", "Download Team Q Report", class = "pen_button"))
    }
  })
  
  
  output$markquestions <- renderUI({

    if (input$survey_type == "teamq_student") {
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
 

  
## Download Handlers ----  
  output$downloadMarkReport <- downloadHandler(
    filename = sprintf("GRASP Mark report - %s.pdf", file_name()),
    content = function(file) {
      
     file.copy(list.files(pattern = "interim_mark_report.pdf", full.names = TRUE), file)
      file.remove("interim_mark_report.pdf")
    },
    contentType = "application/pdf"
    
  )

  output$downloadCommentReport <- downloadHandler(
    filename = sprintf("GRASP Comment report - %s.pdf", file_name()),
    content = function(file) {
      
      file.copy(list.files(pattern = "interim_comment_report.pdf", full.names = TRUE), file)
      file.remove("interim_comment_report.pdf")
    },
    contentType = "application/pdf"
    
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
  
  
  output$downloadTeamQdiagnostic <- downloadHandler(
    filename = function() {
      sprintf("TeamQDiagnostic %s.pdf", Sys.Date())
    },
    content = function(file) {
      
      grobs <-  grasp_data() %>% 
        split(.$team) 
      
      withProgress(message = "Building Diagnostic Reports", value = 0, {
        
        printReport <- function(obj, idx, max){
          
          teamq_plot_diagnostics(obj)
          
          incProgress(1/max, detail = sprintf("Printing report %s of %s", idx, max)) 
          }
      
        
      CairoPDF(file, width = 8.5, height = 11, onefile = TRUE)
      
        pwalk(list(obj = grobs, idx = seq_along(grobs), max = length(grobs)), printReport)
   
      dev.off() 
      
      })
      
    },
    contentType = "application/pdf"
  )
  
 
  ## Reactive Processing Functions ----
  observeEvent(input$processTeamQReport, {
    
    withProgress(message = "Building Student Reports", value = 0, {
      
      ratings <- split(grasp_data(), grasp_data()[["reviewee_id"]])
      
      tempReport <- "teamq_report_template.Rmd"
      
      renderBar <- function(x, y, max){
        
        rmarkdown::render(tempReport,
                          output_file = sprintf("%s.pdf", y),
                          params = list(data = sprintf("ratings[[%s]]", x)),
                          clean = TRUE,
                          quiet = TRUE)
        
        incProgress(1/max, detail = sprintf("Printing report %s of %s", x, max))
        
      }
      
      
      pwalk(tibble(x = seq_along(ratings), y = names(ratings), max = length(ratings)), renderBar) 
    })
    
    
    on.exit(list(show("downloadTeamQReport"), hide("processTeamQReport")))
    
  })
  
  
  observeEvent(input$processMarkReport, {
    
    
    printReport <- function(obj, idx, max){
      
      grid.draw(obj)
      grid.newpage()
      
      incProgress(1/max, detail = sprintf("Rendering report %s of %s", idx, max))
      
    }
    
    renderBar <- function(team_number, grob1, grob2, idx, max, ...){
      
      out <- make_apsc_mark_report(team_number, grob1, grob2)
      
      incProgress(1/max, detail = sprintf("Rendering report %s of %s", idx, max))
      
      return(out)
      
    }
    
    withProgress(message = "Building Mark Reports", value = 0, {
    
    mark_grobs <- grasp_data() %>%
      make_tables(type = "mark", survey = "apsc") %>% 
      ungroup() %>% 
      mutate(grob1 = map(table1, ~build_apsc_table_grob(.x, "Intellectual and Technical Contribution")),
             grob2 = map(table2, ~build_apsc_table_grob(.x, "Collaboration Communication and Work Ethic")),
             idx = row_number(team_number),
             max = nrow(.)) %>% 
      pmap(renderBar)
  
    pdf("interim_mark_report.pdf", width = 8.5, height = 11, onefile = TRUE)
    
    pwalk(list(obj = mark_grobs, idx = seq_along(mark_grobs), max = length(mark_grobs)), printReport)
    
    dev.off()
    
    on.exit(list(show("downloadMarkReport"), hide("processMarkReport")))
    
  })
  })
  
 observeEvent(input$processCommentReport, {
    
    withProgress(message = "Building Comment Reports", value = 0, {
      
      printReport <- function(obj, idx, max){
        
        print(obj)
        
        incProgress(1/max, detail = sprintf("Rendering report %s of %s", idx, max))
        
      }
      
      
      renderBar <- function(team_number, grob1, idx, max, ...){
        
        out <- make_apsc_comment_report(team_number, grob1)
        
        incProgress(1/max, detail = sprintf("Rendering report %s of %s", idx, max))
        
        return(out)
        
      }
      
      comment_grobs  <- grasp_data() %>% 
        make_tables(type = "comment", survey = "apsc") %>% 
        ungroup() %>% 
        mutate(grob1 = map(comments, ~build_apsc_table_grob(.x, NULL)),
                  idx = row_number(team_number),
                  max = nrow(.)) %>% 
        pmap(renderBar) 
      
      pdf("interim_comment_report.pdf", width = 8.5, height = 11, onefile = TRUE, paper = "letter")
      
      pwalk(list(obj = comment_grobs, idx = seq_along(comment_grobs), max = length(comment_grobs)), printReport)
      
      dev.off()
      
    })
    on.exit(list(show("downloadCommentReport"), hide("processCommentReport")))
    
  })  
  
  
})
