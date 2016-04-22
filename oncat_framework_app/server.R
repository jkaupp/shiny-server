library(shiny)
library(purrr)
library(googlesheets)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(grid)
library(gridExtra)
library(ggplot2)
library(viridis)


source("plots.R")

shinyServer(
  function(input, output, session) {
   
    # Dynamically render the google form UI
    output$googleForm <- renderUI({
      tags$iframe(
        id = "googleform",
        src = googleform_embed_link,
        width = "100%",
        height = 1000,
        frameborder = 0,
        marginheight = 0
      )
    })
    
    
    # Get googlesheet data
    options("googlesheets.httr_oauth_cache" = "gs_auth")
    gs_auth(verbose = FALSE)
    gs_file <- gs_key(x = googleform_data_key, verbose = FALSE)
    
    # Reactive Expression to Poll the Googlesheet every 4 seconds
    
    # gs_df <- reactive({
    #   #options("googlesheets.httr_oauth_cache" = "gs_auth")
    #   gs_auth(verbose = FALSE)
    #   gs_read(gs_file, verbose = FALSE)
    # })
    
     gs_df <- reactivePoll(
      4000,
      session,
      checkFunc = function() {
        gs_auth(verbose = FALSE)
        if (exists("gs_file"))
          gs_file$updated
        else
          ""
      },
      valueFunc = function() {
        options("googlesheets.httr_oauth_cache" = "gs_auth")
        gs_auth(verbose = FALSE)
        gs_read(gs_file, verbose = FALSE) }
    )
 
    # Plot Depth of Knowledge
    output$q1 <- renderPlot({
      input$refresh1
      generate_plots(gs_df(), "1")
    })
    
    # Plot Type of Knowledge
    output$q2 <- renderPlot({
      input$refresh2
      generate_plots(gs_df(), "2")
    })
      
      # Plot Interdependence
      output$q3 <- renderPlot({
        input$refresh3
        generate_plots(gs_df(), "3")
      })
    

  
})
