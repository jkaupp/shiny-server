library(shiny)
library(googlesheets)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(viridis)
library(gridExtra)

source("plots.R")

## ======================
googleform_embed_link <- "https://docs.google.com/forms/d/132Pw_5IoevjUbemoWnPrHEn2zOAXJcMTOXH8ClF4o5Y/viewform?embedded=true"
googleform_data_key <- "1YPusUZWrzPnERPfqiOPS5y8VrjQP_CAHe7-6D_leUFo"
## ======================

#Factor Levels
`type of knowledge_` <- c("Factual",
                         "Conceptual",
                         "Computational",
                         "Math translation",
                         "Investigative")

`cognitive process_` <- c("Remember",
                         "Understand",
                         "Apply",
                         "Analyze",
                         "Evaluate",
                         "Create")

transfer_ <- c(
  "Mathematical knowledge",
  "Apply in a disciplinary context",
  "Apply in other engineering contexts",
  "Apply to real-world predictable contexts",
  "Apply to real-world unpredictable contexts"
)

`depth of knowledge_` <- c(
  "Solved by standardized ways",
  "Solved by well-proven analytical techniques",
  "Originiality in analysis, no obvious soltions"
)

interdependence_ <- c(
  "Discrete components",
  "Parts of systems within complex engineering problems",
  "High level problems including many component parts or sub-problems"
)


shinyServer(function(input, output, session) {
   
  # Dynamically render the google form UI
  output$googleForm <- renderUI({
    tags$iframe(id = "googleform",
                src = googleform_embed_link,
                width = "100%",
                height = 1000,
                frameborder = 0,
                marginheight = 0)
  })
  
  
  # Get googlesheet data
  # options("googlesheets.httr_oauth_cache" = "gs_auth")
  # gs_auth(verbose = FALSE)
  gs_file <- gs_key(x = googleform_data_key, verbose = FALSE)
  
  # Reactive Expression to Poll the Googlesheet every 4 seconds
  gs_df <- reactivePoll(
    4000,
    session,
    checkFunc = function() {
      if (exists("gs_file"))
        gs_file$updated
      else
        ""
    },
    valueFunc = function() { 
      # options("googlesheets.httr_oauth_cache" = "gs_auth")
      # gs_auth(verbose = FALSE)
      gs_read(gs_file, verbose = FALSE) }
  )
  
    # Reactive expression to wrangle googleform results
    plot_data <- reactive({gs_df() %>% 
      set_names(str_trim(tolower(names(.)))) %>% 
      gather("item","value", -timestamp) %>% 
      separate(item, c("scale","question"), sep = "( \\[)") %>% 
      mutate(question = str_replace(question, "\\]","")) %>% 
      na.omit() %>% 
      spread(scale,value) %>%
      mutate(`cognitive process` = factor(`cognitive process`, `cognitive process_`),
             `depth of knowledge` = factor(`depth of knowledge`, `depth of knowledge_`),
             transfer = factor(transfer,transfer_),
             interdependence = factor(interdependence, interdependence_),
             `type of knowledge` = factor(`type of knowledge`, `type of knowledge_`)
      ) %>% 
      mutate_each(funs(as.numeric), -question, -timestamp, -transfer, -`cognitive process`) %>% 
      gather(item, rating, -timestamp, -question, -`cognitive process`, -transfer) %>% 
      mutate(variable = str_to_title(item)) %>% 
      arrange(rating, item, `cognitive process`, transfer)
    })
  
  
  # Plot Depth of Knowledge
  output$depth <- renderPlot({
    
    oncat_framework_scatter(filter(plot_data(), grepl("depth",item)))
  
  })
  
  # Plot Interdependence
  output$interdependence <- renderPlot({
    
    oncat_framework_scatter(filter(plot_data(), grepl("interdependence",item)))
    
  })
  
  # Plot Type of Knowledge
  output$type <- renderPlot({
    
    oncat_framework_scatter(filter(plot_data(), grepl("type",item)))
    
  })
  
  

  
})
