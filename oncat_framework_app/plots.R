
oncat_framework_scatter <- function(df) {

  value_factor <- get(tolower(paste0(unique(df$item),"_")))
  
  df$value <- factor(df$rating, c(1:length(value_factor)), value_factor)
  
  plot <- ggplot(df, aes(x = transfer, y =`cognitive process`)) +
    geom_point(aes(fill = factor(rating)), size = 8, pch = 21, color = "white", alpha = 0.7, show.legend = TRUE, position = position_jitter()) +
    labs(x = "Transfer", y = "Cognitive Process\n", title = unique(df$variable), subtitle = NULL, caption = NULL) +
    geom_vline(xintercept=seq(1.5, length(transfer_)-0.5, 1), lwd=0.1, colour="grey80") +
    geom_hline(yintercept=seq(1.5, length(`cognitive process_`)-0.5, 1), lwd=0.1, colour="grey80") +
    scale_y_discrete(limits = `cognitive process_`, labels = function(x) str_wrap(x, width = 10), drop = FALSE) +
    scale_x_discrete(limits = transfer_, labels = function(x) str_wrap(x, width = 15), drop = FALSE) +
    scale_fill_viridis("Color Key",labels = str_wrap(get(tolower(paste0(unique(df$item),"_"))),60), discrete = TRUE) +
    theme(
      text = element_text(family = "Oswald Light", size = 20, color = "black"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
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
      plot.caption = element_text(size = 14, family = "Quattrocento", face = "italic"))
  
  plot
}