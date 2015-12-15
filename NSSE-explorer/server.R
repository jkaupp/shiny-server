library(shiny)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(rCharts)

# Use nsse_extract to read and format the data for the app ----


# Define server logic for random distribution application
shinyServer(function(input, output) {

  
  # Reactive data for Lollipop Chart ----
  data <- reactive({
    nsse_results %>%
      filter(
        School %in% input$compare_group |
          School == "Hogwarts" |
          School == "All",
          Dept %in% input$department,
        !is.na(Indicator),
        Indicator !="HIP"
      )
  })
  
  # Reactive data for Program breakdown Chart ----
  data2 <- reactive({
    
   nsse_results %>%
      filter(
        School %in% input$compare_group2 |
          School == "Hogwarts" |
          School == "All",
        Dept %in% input$department2,
        Indicator == input$indicator,
        !is.na(Indicator),
        Indicator !="HIP"
      )
  })
  
  data3 <- reactive({
  
    df3 <- nsse_results %>%
      filter(
        School %in% input$compare_group3 |
          School == "Hogwarts" |
          School == "All",
        Dept ==input$department3,
        Code == "Engagement Indicator",
        !is.na(Indicator),
        Indicator !="HIP"
      ) %>%
      group_by(Year, Indicator, Code) %>%
      mutate(`National Leader` = (Mean / Mean[School == "All"])) %>%
      top_n(1,`National Leader`) %>%
      select(Year, School, Indicator, Code, `National Leader`) 
    
    school_df <- nsse_results %>% 
      filter(
        School %in% input$compare_group3 |
          School == "Hogwarts" |
          School == "All",
        Dept == input$department3,
        Code == "Engagement Indicator",
        !is.na(Indicator),
        Indicator !="HIP"
      ) %>%  
      group_by(Year, Indicator, Code) %>% 
      summarize("Hogwarts" = Mean[School=="Hogwarts"] / Mean[School=="All"]) %>% 
      left_join(nsse_indicators, by="Indicator") 
    
    df3 %<>% 
      left_join(.,school_df) %>% 
      gather(Category, Ratio ,`National Leader`,`Hogwarts`) %>% 
      mutate(Indicator = factor(Indicator))
    
    return(df3)
  })
  
  # NSSE Lollipop Charting Reactive Function ----  
  nsse_lollipop <-  reactive({

    # New
    
    nsse_leaders <- data() %>% 
      filter(Code!="Engagement Indicator",  N>10) %>% 
      group_by(Year, Indicator, Code) %>%
      mutate(diff = (Mean / Mean[School == "All"]) - 1,
             Shape = 25) %>%
      filter(School != "All") %>% 
      top_n(1,diff) %>% 
      mutate(Fill = ifelse(School=="Hogwarts", "#EBCC2A", "#FFFFFF"),
             Colour = ifelse(School=="Hogwarts","#6AE03F","#3B9AB2"))%>% 
      filter(rank(diff, ties.method="first")==1) 
    
    local_nsse <- data() %>% 
      filter(School=="Hogwarts" | School=="All", Code != "Engagement Indicator", N>5) %>% 
      group_by(Year, Indicator, Code) %>%
      mutate(diff = (Mean[School == "Hogwarts"] / Mean[School == "All"]) - 1) %>%
      filter(School=="Hogwarts") %>% 
      mutate(Colour = ifelse(diff > 0,"#6AE03F","#CC0605"),
             Shape = 21,
             Fill = "#FFFFFF")
    
    ggplot(local_nsse, aes(x = Code, y = diff)) +
      geom_segment(aes(x = Code, xend = Code, y = 0, yend = diff, colour = Colour), size = 1.2) +
      geom_point(aes(shape = Shape, fill = Fill), colour = "black", size = 6, data = 
                   anti_join(local_nsse,nsse_leaders,by=c("Year","Dept","School","Code"))) +
      geom_hline(yintercept=0) +
      geom_point(aes(shape = Shape, fill = Fill, colour = Colour), data = nsse_leaders, size = 6) +
      geom_text(aes(y = diff + 0.01, label = School), angle=90, data=nsse_leaders, size = 3, hjust=-0.2) +
      facet_grid(Year~Description, scales="free_x", labeller = label_wrap_gen(width = 10)) +
      scale_colour_identity() +
      scale_fill_identity() +
      scale_size_identity() + 
      scale_y_continuous(labels=percent) +
      scale_shape_identity() +
      ylab("Difference from National Average\n") +
      xlab("\nSurvey Item") +
      ggtitle("NSSE: Engagement Indicator Comparison to National Average and National Leaders\n") +
      theme(
        text = element_text(size = 18),
        panel.grid.major.y = element_line(colour = "grey50", size = 0.05),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.margin = unit(0, "lines"),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.background = element_rect(fill = "#FFFFFF"),
        legend.key = element_rect(fill = "#FFFFFF"),
        strip.text.y = element_text(size = 14, angle = 0),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 14, color = "black", angle=90, hjust=1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF")
      )  
  })

  # NSSE Program Breakdown Charting Reactive Function ----  
  nsse_program_breakdown <- reactive({
    
    EI_nsse_scale <- data2() %>%
      filter(N >= 10, Code == "Engagement Indicator", School != "All") %>%
      group_by(Year) %>% 
      arrange( Mean) %>%
      mutate(School = factor(School,School))
    
    all_nsse_EI <-  data2() %>%
      filter(N > 10,  Code == "Engagement Indicator", School == "All")
    
    EI_plot <-
      ggplot(data = EI_nsse_scale, aes(x = School, y = Mean)) +
      geom_pointrange(aes(
        ymin = Mean - sd, ymax = Mean + sd, color = Color
      )
      , size = 1) +
      geom_hline(
        aes(yintercept = Mean), colour = "#F21A00", linetype = 4, size = 1, alpha = 0.5, data = all_nsse_EI
      ) +
      geom_text(aes(y = 0, x = School, label=paste("n=", (N), sep = "")), size = 4) +
      coord_flip() +
      facet_wrap(~Year) +
      ggtitle(str_wrap(paste0(unique(
        EI_nsse_scale$Prompt
      ),"\n"),50)) +
      ylab("Weighted Mean") +
      xlab("") +
      theme(
        text = element_text(size = 18),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        title = element_text(size = 16),
        strip.text.y = element_text(size = 12, angle = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF")
      ) +
      scale_y_continuous(limits = c(0,80)) +
      scale_color_identity()
    
    scale_item_plot <- function(df) {
      df <- df %>%
        group_by(Year) %>% 
        arrange(Code,Mean) %>%
        mutate(School = factor(School,School))
      
      ggplot(data = subset(df, School != "All"), aes(x = School, y = Mean)) +
        geom_pointrange(aes(
          ymin = Mean - sd, ymax = Mean + sd, color = Color
        ), size = 1) +
        geom_hline(
          aes(yintercept = Mean), color = "#F21A00", linetype = 4, size = 1, alpha = 0.5, data = subset(df, School ==
                                                                                                          "All")
        ) +
        geom_text(aes(y = 0.5, x = School, label=paste("n=", (N), sep = "")), size = 4) +
        coord_flip() +
        ggtitle(str_wrap(paste0(unique(df$Code),":",unique(df$Prompt)),70)) +
        ylab("") +
        xlab("") +
        facet_wrap(~Year) +
        theme(
          text = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          strip.text.y = element_text(size = 12, angle = 0),
          strip.text.x = element_text(size = 12),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          panel.background = element_rect(fill = "#FFFFFF"),
          strip.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF")
        ) +
        scale_y_discrete(limits = c("Never","Sometimes", "Often", "Very Often")) +
        scale_color_identity()
    }
    
    
    scales_plot <- data2() %>%
      filter(N >= 10, Code != "Engagement Indicator") %>%
      dlply(., .(Code), scale_item_plot)
    
    scales_plot$EI_plot <- EI_plot
    
    grid.arrange(
      grobs = scales_plot, layout_matrix = layout_frame[[paste0(input$indicator,"_layout")]], top = textGrob(
        "NSSE: Engagement Indicator Item Breakdown", gp =
          gpar(fontsize = 22)
      )
    )
  
  })

  
  # Rcharts Dashboard Reactive Function ----
  output$spider_chart_fy <- renderChart2({
    spider_fy <- hPlot(x = "Description", y="Ratio",  data = subset(data3(),Year=="First Year"), type = "line", group = "Category",title = "NSSE Engagement Indicators: First Year", subtitle = "Ratio to National Average")
    spider_fy$chart(polar = TRUE)
    spider_fy$params$width <- 750
    spider_fy$params$height <- 600
    return(spider_fy)
  })
  
  output$spider_chart_sy <- renderChart2({
    spider_sy <- hPlot(x = "Description", y="Ratio",  data = subset(data3(), Year=="Senior Year"), type = "line", group = "Category",title = "NSSE Engagement Indicators: Senior Year", subtitle = "Ratio to National Average")
    spider_sy$chart(polar = TRUE)
    spider_sy$params$width <- 750
    spider_sy$params$height <- 600
    return(spider_sy)
  })
  
  
  # Output nsse_lollipop to plot 1 UI element
  output$plot1 <- renderPlot({
    nsse_lollipop() 
  })
  
  # Output nsse_program_breakdown to plot 2 UI element
  output$plot2 <- renderPlot({
    nsse_program_breakdown()
  })

  # Code to save nsse_lollipop plot.
  output$save_plot1 = downloadHandler(
    filename = paste0("Hogwarts", input$department, "NSSE Program Overview.pdf", sep=" "),
    content = function(file) {
      ggsave(file, plot = nsse_lollipop(), scale = 1, device = pdf, width=22, height=17, dpi=300)
      embed_fonts(file)
    })

  # Code to save nsse program breakdown
  output$save_plot2 = downloadHandler(
    filename = paste0("Hogwarts",input$department2,' - NSSE Program Breakdown ', input$indicator, '.pdf'),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::pdf(..., width = width, height = height)
      }
      device(file, width=22, height=17)
      grid.draw(nsse_program_breakdown())
      dev.off()
      embed_fonts(file)
    })
  
  })
