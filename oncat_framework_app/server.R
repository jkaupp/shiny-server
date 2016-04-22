library(shiny)
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
    
     gs_df <- reactivePoll(
       4000,
       session,
       checkFunc = function() {
         gs_key(x = googleform_data_key, verbose = FALSE, lookup = FALSE)$updated
         #googlesheets:::gd_metadata(googleform_data_key, auth = FALSE)$modifiedTime
       },
       valueFunc = function() {
         gs_read_csv(gs_file, verbose = FALSE) }
     )

    # Plot Depth of Knowledge
    output$q1 <- renderPlot({
      # input$refresh1
      # generate_plots(gs_read(gs_file, verbose = FALSE), "1")
      
      generate_plots(gs_df(), "1")
      
    })
    
    # Plot Type of Knowledge
    output$q2 <- renderPlot({
      # input$refresh2
      # observeEvent(input$refresh2, { gs_df <- gs_read(gs_file, verbose = FALSE)})
      
      generate_plots(gs_df(), "2")
    })
      
      # Plot Interdependence
      output$q3 <- renderPlot({
        # observeEvent(input$refresh3, { gs_df <- gs_read(gs_file, verbose = FALSE)})
        # 
        # gs_df <- gs_read(gs_file, verbose = FALSE)
        
        generate_plots(gs_df(), "3")
      })
    

  
})
