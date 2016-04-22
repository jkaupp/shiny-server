## Googlesheets links
googleform_embed_link <- "https://docs.google.com/forms/d/132Pw_5IoevjUbemoWnPrHEn2zOAXJcMTOXH8ClF4o5Y/viewform?embedded=true"
googleform_data_key <- "1YPusUZWrzPnERPfqiOPS5y8VrjQP_CAHe7-6D_leUFo"

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


oncat_framework_scatter <- function(df) {
  
  value_factor <- get(tolower(paste0(unique(df$item),"_")))

  df$value <- factor(df$rating, c(1:length(value_factor)), value_factor)
  
  plot <- ggplot(df, aes(x = transfer, y =`cognitive process`)) +
    geom_point(aes(fill = factor(rating)), size = 8, pch = 21, color = "white", alpha = 0.7, show.legend = TRUE, position = position_jitter()) +
    labs(x = NULL, y = NULL, title = str_to_title(unique(df$item)), subtitle = NULL, caption = NULL) +
    geom_vline(xintercept=seq(1.5, length(transfer_)-0.5, 1), lwd = 0.1, colour="grey80") +
    geom_hline(yintercept=seq(1.5, length(`cognitive process_`)-0.5, 1), lwd=0.1, colour="grey80") +
    scale_y_discrete(limits = `cognitive process_`, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
    scale_x_discrete(limits = transfer_, labels = function(x) str_wrap(x, width = 15), drop = FALSE) +
    scale_fill_viridis("Color Key", limits = 
                         seq(1, length(get(tolower(paste0(unique(df$item),"_"))))),  labels = str_to_title(str_wrap(get(tolower(paste0(unique(df$item),"_"))), 60)), discrete = TRUE, drop = FALSE) +
    theme(
      text = element_text(size = 16, color = "black"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.background = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 180),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.margin.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 12, face = "italic"))
  
  return(plot)
}

generate_plots <- function(df, q_num){
  
  data <- df %>% 
    set_names(str_trim(tolower(names(.)))) %>%
    gather("item", "value",-timestamp) %>%
    separate(item, c("scale", "question"), sep = "( \\[)") %>%
    mutate(question = str_replace(question, "\\]", "")) %>%
    na.omit() %>% 
    spread(scale, value) %>%
    mutate(
      `cognitive process` = factor(`cognitive process`, `cognitive process_`),
      `depth of knowledge` = factor(`depth of knowledge`, `depth of knowledge_`),
      transfer = factor(transfer, transfer_),
      interdependence = factor(interdependence, interdependence_),
      `type of knowledge` = factor(`type of knowledge`, `type of knowledge_`)
    ) %>%
    mutate_each(funs(as.numeric), -question,-timestamp,-transfer,-`cognitive process`) %>%
    gather(item, rating,-timestamp,-question,-`cognitive process`,-transfer) %>%
    arrange(rating, item, `cognitive process`, transfer) 
  
  plots <- data %>% 
    group_by(question, item) %>% 
    do(plots = oncat_framework_scatter(.)) %>% 
    filter(grepl(q_num, question))
   
  grid.arrange(grobs = plots$plots, top = textGrob(
    paste(str_to_title(unique(plots$question))), gp = gpar(fontsize = 20)),
    left = textGrob("Cognitive Process", gp = gpar(fontsize = 20), rot = 90),
    bottom =textGrob("Transfer", gp = gpar(fontsize = 20))
    )
}