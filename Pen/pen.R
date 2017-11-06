# APSC Peer Review Survey Functions----

# Master make tables function
make_tables <- function(x, survey, type) {
  
  switch(survey,
         "apsc" = make_apsc_tables(x, type = type),
         "teamq_student" = make_teamq_tables(x),
         "teamq_diagnostic" = make_teamq_diag_tables(x))
}

# Make APSC Peer Assessment survey tables & grobs
make_apsc_tables <- function(x, type) {
  
  var <- switch(type,
                "mark" = list("q1_mark","q2_mark"),
                "comment" = "comment")
  
  if (type == "mark") {
    
    # "Intellectual and Technical Contribution"
    tables <- x %>%
      mutate(team_number = team) %>% 
      nest(-team_number) %>% 
      group_by(team_number) %>% 
      transmute(table1 = map(data, ~apsc_mark_tables(.x, var = var[[1]])),
             table2 = map(data, ~apsc_mark_tables(.x, var = var[[2]])))
  

    
  } else {
    
    #Peer Comment Tables
    tables <- x %>% 
      mutate(team_number = team) %>% 
      nest(-team_number) %>% 
      group_by(team_number) %>% 
      transmute(comments = map(data, ~apsc_comment_tables(.x, var = var)))
  }
  return(tables)
}


# Make apsc peer assessment mark data tables
apsc_mark_tables <- function(x, var) {
  
  main <- select_(x, ~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, var) %>% 
    mutate(reviewer_name = paste(reviewer_first_name,reviewer_last_name, sep = " ")) %>% 
    mutate(reviewee_name = paste(reviewee_first_name,reviewee_last_name, sep = " ")) %>% 
    arrange(reviewer_last_name, reviewee_last_name) %>%
    select_(~reviewee_name, ~reviewer_name, var) %>%
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) 
  
  summary <- main %>% 
    spread_("reviewee_name", var) %>% 
    mutate_at("reviewer_name", funs(as.character)) %>% 
    clean_names() %>% 
    summarize_at(-1, funs(mean), na.rm = TRUE) %>% 
    mutate(reviewer_name = "Average") %>% 
    mutate_if(is.numeric, funs(trunc))
  
  adj_avg <- main %>% 
    mutate_at("reviewer_name", funs(as.character)) %>% 
    filter(reviewer_name != reviewee_name) %>% 
    group_by(reviewee_name) %>% 
    summarize_at(var,  funs(mean), na.rm = TRUE) %>% 
    spread_("reviewee_name", var) %>% 
    mutate(reviewer_name = "Adjusted Average") %>% 
    clean_names() %>% 
    mutate_if(is.numeric, funs(trunc)) 
  
  base_adjust <- main %>% 
    mutate_at("reviewer_name", funs(as.character)) %>% 
    filter(reviewer_name != reviewee_name) %>% 
    group_by(reviewee_name) %>% 
    summarize_at(var,  funs(mean), na.rm = TRUE) %>% 
    mutate_at(var, function(x) x - (100/length(x))) %>% 
    spread_("reviewee_name", var) %>% 
    mutate(reviewer_name = "Base Adjustment") %>% 
    clean_names() %>% 
    mutate_if(is.numeric, funs(trunc)) 
  
  names <- as.character(unique(main$reviewee_name))
  
  main <- main %>% 
    mutate_at(var, function(x) replace(x, is.na(x), "???")) %>% 
    spread_("reviewee_name", var) %>% 
    mutate_at("reviewer_name", funs(as.character)) %>% 
    clean_names() %>% 
    mutate_at(-1, funs(as.numeric))
  
  main %>%
    bind_rows(summary, adj_avg, base_adjust) %>% 
    set_names(c("Ratings For", names)) 
  
}

# Make apsc peer assessment mark data tables
apsc_comment_tables <- function(x, var) {
  
  select_(x,~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, ~contains(var)) %>% 
    mutate(reviewer_name = paste(reviewer_first_name,reviewer_last_name, sep = " ")) %>% 
    mutate(reviewee_name = paste(reviewee_first_name,reviewee_last_name, sep = " ")) %>% 
    gather("item","comment", contains("comment")) %>% 
    separate(item, c("question","drop"), sep = "_") %>% 
    arrange(reviewee_last_name, reviewer_last_name, question) %>% 
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    mutate(comment = stri_replace_all_regex(comment, "[\r]" , "")) %>% 
    mutate(comment = stri_replace_all_regex(comment, "[^[:alnum:]]", " ")) %>% 
    select(reviewee_name, reviewer_name, question, comment) %>% 
    replace_na(list(comment = "")) %>% 
    rename(To = reviewee_name,
           From = reviewer_name,
           Question = question,
           Comment = comment) 
  
}

# Function to generate APSC Mark report for APSC Peer Asssessment
make_apsc_mark_report <- function(team_number, grob1, grob2) {
  
  title <- sprintf("Peer Evalution Team: %s", team_number)
  
  grid.arrange(grob1, grob2, top = title) 
  
  
}

# Function to generate APSC comment report for APSC Peer Asssessment
make_apsc_comment_report <- function(team_number, grob1) {
  
  title <- sprintf("Peer Comments Team: %s", team_number)
  
  grid.arrange(grob1, top = title) 
  
}


# Build Table Grobs for APSC Peer Assessment Survey report
build_apsc_table_grob <- function(tbl, tbl_title) {
  
  rows <- nrow(tbl)
  cols <- ncol(tbl)
  
  if (rows < 10) {
    theme <- ttheme_default(core = list(fg_params = list(fontface = c(rep("plain", rows - 3), rep("bold", 3))))) 
  } else {
    
    theme <- ttheme_default(base_size = 8) 
    
    tbl <- mutate_at(tbl, "Comment", funs(str_wrap), width = 100) 
  }
  
  table <- tableGrob(tbl, rows = NULL, theme = theme)
  
  if (rows < 10) {
    
    for (i in seq(rows - 1, rows + 1)) {
      
      table <- gtable_add_grob(table,
                               grobs = segmentsGrob( # line across the bottom
                                 x0 = unit(0,"npc"),
                                 y0 = unit(1,"npc"),
                                 x1 = unit(1,"npc"),
                                 y1 = unit(1,"npc"),
                                 gp = gpar(lwd = 3.0)),
                               t = i, b = i, l = 1, r = cols) } 
  } else {
    
    for (i in seq(2, rows, by = 2)) {
      
      table <- gtable_add_grob(table,
                               grobs = segmentsGrob( # line across the bottom
                                 x0 = unit(0,"npc"),
                                 y0 = unit(1,"npc"),
                                 x1 = unit(1,"npc"),
                                 y1 = unit(1,"npc"),
                                 gp = gpar(lwd = 3.0)),
                               t = i, b = i, l = 1, r = cols) }
  }
  
  
  if(!is.null(title)) {
    
    title <- textGrob(tbl_title , gp = gpar(fontsize = 14))
    
    padding <- unit(5,"mm")
    
    table <- gtable_add_rows(table, heights = grobHeight(title) + padding, pos = 0)
    
    table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table), clip = "off") 
  }
  
  return(table)
  
}

# TeamQ Student ----

# Make teamQ raw data tables
make_teamq_tables <- function(x) {
  
  teamq <- tibble(question = 1:15, 
                  scales =c(rep("Contributes to Team Project", 3),rep("Facilitates Contibutions of Others",3),rep("Planning and Management",3),rep("Fosters a Team Climate",2),rep("Manages Potential Conflict",3),"Suggestions to Improve"),
                  items = c("Participates Actively and Accepts a Fair Share of the Group Work", 
             "Works Skillfully on Assigned Tasks and Completes Them on Time", 
             "Gives Timely, Constructive Feedback to Team Members, in the Appropriate Format", 
             "Communicates Actively and Constructively", "Encourages All Perspective be Considered and Acknowledges Contributions of Others", 
             "Constructively Builds on Contributions of Others and Integrates Own Work with Work of Others", 
             "Takes on an Appropriate Role in Group (E.g. Leader, Note Taker)", 
             "Clarifies Goals and Plans the Project", "Reports to Team on Progress", 
             "Ensures Consistency Between Words, Tone, Facial Expression and Body Language", 
             "Expresses Positivity and Optimism About Team Members and Project", 
             "Displays Appropriate Assertiveness: Neither Dominating, Submissive, nor Passive Aggressive", 
             "Contributes Appropriately to Healthy Debate", "Responds to and Manages Direct/Indirect Conflict Constructively and Effectively",
             "Write one specific, actionable goal to help your teammate improve their teamwork competency"
  ))
  
  x %>%
    select_(~team, ~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, ~contains("q")) %>% 
    unite(reviewer_name, reviewer_first_name, reviewer_last_name, sep = " ") %>% 
    unite(reviewee_name, reviewee_first_name, reviewee_last_name, sep = " ") %>% 
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    mutate_if(is.character, funs(stri_replace_all_regex(., "[\r]" , ""))) %>% 
    mutate_if(is.character, funs(stri_replace_all_regex(., "[^[:alnum:]]", " "))) %>%
    mutate_if(is.numeric, function(x) factor(x, 1:5, c("Never","Sometimes","Ususally","Regularly","Always"))) %>% 
    gather(question, value, contains("q")) %>% 
    mutate(question = parse_number(question)) %>% 
    left_join(teamq, by = "question") %>% 
    rename(To = reviewee_name,
           From = reviewer_name,
           team_number  = team)
}



# TeamQ Diagnostic

# Make teamQ Diagnostic Report tables
make_teamq_diag_tables <- function(x) {
  
  teamq <- tibble(question = 1:14, 
                  scales = c(rep("Contributes to Team Project", 3),rep("Facilitates Contibutions of Others",3),rep("Planning and Management",2),rep("Fosters a Team Climate",2),rep("Manages Potential Conflict",2), "Team Contributions", "Suggestions to Improve"),
                  items = c("Participated Actively and Accepts a Fair Share of the Group Work", 
                            "Works Skillfully on Assigned Tasks and Completes Them on Time", 
                            "Gave Timely, Constructive Feedback to Team Members, in the Appropriate Format", 
                            "Communicated Actively and Constructively", 
                            "Encouraged All Perspective be Considered and Acknowledges Contributions of Others", 
                            "Constructively Built on Contributions of Others and Integrates Own Work with Work of Others", 
                            "Took on an Appropriate Role in Group (E.g. Leader, Note Taker)", 
                            "Clarified Goals and Plans the Project", 
                            "Ensured Consistency Between Words, Tone, Facial Expression and Body Language", 
                            "Expressed Positivity and Optimism About Team Members and Project", 
                            "Displayed Appropriate Assertiveness: Neither Dominating, Submissive, nor Passive Aggressive", 
                            "Responded to and Manages Direct/Indirect Conflict Constructively and Effectively",
                            "Think about the overall contribution of all team members overall.",
                            "Free-form Comments"
                  ))
  
  x %>%
    select_(~team, ~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, ~contains("q")) %>% 
    unite(reviewer_name, reviewer_first_name, reviewer_last_name, sep = " ") %>% 
    unite(reviewee_name, reviewee_first_name, reviewee_last_name, sep = " ") %>% 
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    mutate_if(is.character, funs(stri_replace_all_regex(., "[\r]" , ""))) %>% 
    mutate_if(is.character, funs(stri_replace_all_regex(., "[^[:alnum:]]", " "))) %>%
    mutate_if(is.numeric, function(x) factor(x, 1:5, c("Never","Sometimes","Ususally","Regularly","Always"))) %>% 
    gather(question, value, contains("q")) %>% 
    mutate(question = parse_number(question)) %>% 
    left_join(teamq, by = "question") %>% 
    rename(To = reviewee_name,
           From = reviewer_name,
           team_number  = team)
    
  
}

# Make teamQ diagnostics plots
teamq_plot_diagnostics <- function(x){
  
  teamq <- tibble(question = 1:14, 
                  scales = c(rep("Contributes to Team Project", 3),rep("Facilitates Contibutions of Others",3),rep("Planning and Management",2),rep("Fosters a Team Climate",2),rep("Manages Potential Conflict",2), "Team Contributions", "Suggestions to Improve"),
                  items = c("Participated Actively and Accepts a Fair Share of the Group Work", 
                            "Works Skillfully on Assigned Tasks and Completes Them on Time", 
                            "Gave Timely, Constructive Feedback to Team Members, in the Appropriate Format", 
                            "Communicated Actively and Constructively", 
                            "Encouraged All Perspective be Considered and Acknowledges Contributions of Others", 
                            "Constructively Built on Contributions of Others and Integrates Own Work with Work of Others", 
                            "Took on an Appropriate Role in Group (E.g. Leader, Note Taker)", 
                            "Clarified Goals and Plans the Project", 
                            "Ensured Consistency Between Words, Tone, Facial Expression and Body Language", 
                            "Expressed Positivity and Optimism About Team Members and Project", 
                            "Displayed Appropriate Assertiveness: Neither Dominating, Submissive, nor Passive Aggressive", 
                            "Responded to and Manages Direct/Indirect Conflict Constructively and Effectively",
                            "Think about the overall contribution of all team members overall.",
                            "Free-form Comments"
                  ))
  
  lookup <- setNames(teamq$scales,teamq$question)
  
  team_no <- unique(x[["team"]])
  
  survey_data <- x %>% 
    dplyr::select(team, dplyr::contains("reviewer"), dplyr::contains("reviewee"), -dplyr::contains("d2l"), dplyr::matches("q\\d+")) %>% 
    gather(question, value, -1:-7) %>% 
    separate(question, into = c("question","type"), sep = "\\_", fill = "right") %>% 
    mutate(question = parse_number(question),
           to = sprintf("%s %s", reviewee_first_name, reviewee_last_name),
           from = sprintf("%s %s", reviewer_first_name, reviewer_last_name),
           type = ifelse(question == 14, "comment", type),
           value = gsub("#NAME?", NA_character_, value)) 
  
  if (!all(is.na(survey_data[['value']]))) {
  
  total <- filter(survey_data, question == 13, type == "mark") %>% 
    left_join(teamq, by = "question") %>% 
    group_by(team, to, from, scales) %>% 
    dplyr::summarize(value = mean(as.numeric(value), na.rm = TRUE)) %>% 
    mutate(value = ifelse(is.nan(value), NA_real_, value))
  
  marks <- filter(survey_data, !(question %in% 13:14)) %>% 
    left_join(teamq, by = "question") %>% 
    group_by(team, to, from, scales) %>% 
    dplyr::summarize(value = mean(as.numeric(value), na.rm = TRUE)) %>% 
    mutate(value = cut(value, breaks = 0:5, labels =  c("Never", "Sometimes", "Usually", "Often", "Always")))
  
  
  if (nrow(filter(survey_data, question %in% 13:14,  type == "comment", !is.na(value))) > 0) {
    analyze_sentiment <- filter(survey_data, question %in% 13:14,  type == "comment")  %>%
      unnest_tokens(word, value) %>%
      anti_join(tidytext::stop_words, by = "word") %>% 
      inner_join(filter(sentiments, lexicon == "bing"), by = "word") %>% 
      group_by(question, from, to) %>% 
      dplyr::count(word, sentiment)
    
    if (nrow(analyze_sentiment) != 0) {
      
      analyze_sentiment <- spread(analyze_sentiment, sentiment, n, fill = 0) 
      
      
      if (!("negative" %in% names(analyze_sentiment))) {
        analyze_sentiment <- mutate(analyze_sentiment, net = positive)
      } else if (!("positive" %in% names(analyze_sentiment))) {
        analyze_sentiment <- mutate(analyze_sentiment, net = -negative)
      } else {
        analyze_sentiment <- mutate(analyze_sentiment, net = positive - negative)
      }
      
      
      analyze_sentiment <- analyze_sentiment %>% 
        dplyr::group_by(question, to , from) %>% 
        dplyr::summarize(net =  case_when(sum(net) < -2 ~ "Very Negative",
                                   between(sum(net), 0, -2) ~ "Slightly Negative",
                                   sum(net) == 0 ~ "Neutral",
                                   between(sum(net), 0, 2) ~ "Slightly Positive",
                                   sum(net) > 2 ~ "Very Positive")) 
      
      comment_sent <- filter(survey_data, question %in% 13:14, type == "comment") %>% 
        left_join(analyze_sentiment, by = c("question", "to", "from")) %>% 
        mutate(net = factor(net, c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive"))) %>% 
        mutate(net = fct_explicit_na(net, "Neutral")) %>% 
        mutate(id = as.character(row_number(team)),
               value = gsub("\\'", "", value)) %>% 
        replace_na(list(value = "No Comment Provided"))  
      
    } else {
      comment_sent <- filter(survey_data, question %in% 13:14, type == "comment") %>% 
        mutate(net = NA_character_) %>% 
        mutate(net = factor(net, c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive"))) %>% 
        mutate(net = fct_explicit_na(net, "Neutral")) %>% 
        replace_na(list(value = "No Comment Provided"))
      
    }
  }
  
  
  
  if (!all(is.na(total$value))) {
    t_limits <- c(0, round_any(max(total$value, na.rm = TRUE), 10, ceiling)) 
    
    t_breaks <- seq(0, round_any(max(total$value, na.rm = TRUE), 10, ceiling), by = 10) 
  } else {
    
    t_limits <- c(0, 60)
    
    t_breaks <- seq(0, 60, by = 10)
  }
  
  total_plot <- ggplot(total, aes(x = to, y = from, fill = value)) +
    geom_tile(color = "grey20") +
    geom_text(aes(label = value), na.rm = TRUE) +
    scale_fill_viridis("", limits = t_limits , na.value = "white", breaks = t_breaks ) +
    coord_equal() +
    scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
    scale_y_discrete(labels = function(x) str_wrap(x, 10)) +
    labs(x = NULL,
         y = NULL,
         title = "Point Distribution") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          plot.title = element_text(size = 10),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.25, "cm")) 

  
  mark_plots <- marks %>% 
    split(.$scales) %>% 
    map(build_mark_plot) 
  
  
  #Modify marks and total into a 6x6
    
  mark_plots$total <- total_plot
  
  mark_grobs <- map(mark_plots, ggplotGrob)
  
  marks_legend <- mark_grobs[[1]]$grobs[which(sapply(mark_grobs[[1]]$grobs, function(x) x$name) == "guide-box")]
  
  total_legend <-  mark_grobs[[length(mark_grobs)]]$grobs[which(sapply(mark_grobs[[length(mark_grobs)]]$grobs, function(x) x$name) == "guide-box")]
  
  mark_plots <- map(mark_plots, ~.x + theme(legend.position = "none"))
  
  mark_plots[1:3] <- map(mark_plots[1:3], ~.x + theme(axis.text.x = element_blank()))
  
  legend <- cbind(marks_legend, marks_legend, total_legend)
  
  plot <- arrangeGrob(grobs = c(mark_plots, legend), heights = c(10,11.5,1))

  plot$grobs[[7]] <- zeroGrob();
  
  if (nrow(filter(survey_data, question %in% 13:14,  type == "comment", !is.na(value))) > 0) {
    
    comments <- ggplot(comment_sent, aes(x = to, y = from)) +
      geom_tile(aes(fill = net), color = "grey20") +
      geom_text(aes(label = str_wrap(value, 75)), size = 1.2) +
      facet_wrap(~question, labeller = as_labeller(lookup), nrow = 2) +
      scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
      scale_y_discrete(labels = function(x) str_wrap(x, 10)) +
      coord_fixed(ratio = 0.25) +
      labs(x = NULL,
           y = NULL,
           title = "Sentiment analysis of comments",
           subtitle = NULL) +
      scale_fill_manual("Net Sentiment", values = setNames(brewer.pal(5, "RdBu"), c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive")), limits = c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive"), na.value = "white") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 6),
            axis.text = element_text(size = 6),
            panel.grid = element_blank(),
            plot.margin = unit(c(1,0.1,0,0.1), "cm"),
            legend.key.size = unit(0.5, "cm"))
  } 
  
  if (exists("comments")) {
    #Cairo::CairoPDF("test.pdf", width = 8.5, height = 11, onefile = TRUE)
    grid.arrange(plot, comments, top = sprintf("Team %s", team_no), heights = c(0.4, 0.6)) 
    #dev.off()

  } else {
    grid.arrange(plot, top = sprintf("Team %s", team_no)) 
  
  }
  
  } else {
    
    output <- textGrob(label = sprintf("No member of team %s has completed the GRASP survey", team_no),
                   x = 0.5,
                   y = 0.5,
                   gp = gpar(col = "Red"))
    
    
    grid.draw(output)
     
  }
}


# Helper function to flag trouble teams in TeamQ
flag_teams <- function(x) {
  x %>%
    select(team, reviewee_id, q13_mark) %>%
    group_by(reviewee_id) %>%
    summarize(flag = mean(q13_mark, na.rm = TRUE)) %>%
    mutate(flag = ifelse(flag < 21, TRUE, FALSE)) %>%
    select(flag) %>%
    map_lgl( ~ any(.x))
}


build_mark_plot <- function(x) {
  
 g <- ggplot(x, aes(x = to, y = from, fill = value)) +
    geom_tile(color = "grey20") +
    scale_fill_viridis("", limits = c("Never", "Sometimes", "Usually", "Often", "Always"), na.value = "white", discrete = TRUE) +
    coord_equal() +
    scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
    scale_y_discrete(labels = function(x) str_wrap(x, 10)) +
    labs(x = NULL,
         y = NULL,
         title = unique(x$scales)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          plot.title = element_text(size = 10),
          legend.key.size = unit(0.5, "cm"))
 

 return(g)
}
