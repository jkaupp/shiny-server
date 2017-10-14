apsc_mark_tables <- function(x, var) {
  
  main <- select_(x, ~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, var) %>% 
    mutate(reviewer_name = paste(reviewer_first_name,reviewer_last_name, sep = " ")) %>% 
    mutate(reviewee_name = paste(reviewee_first_name,reviewee_last_name, sep = " ")) %>% 
    arrange(reviewer_last_name, reviewee_last_name) %>%
    select_(~reviewee_name, ~reviewer_name, var) %>%
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    spread_("reviewee_name", var) %>% 
    mutate_each(funs(as.character), reviewer_name)
  
  average <- summarize_each(main, funs(mean(., na.rm = TRUE)), -reviewer_name) %>% 
    mutate(reviewer_name = "Average") %>% 
    mutate_each(funs(trunc), -reviewer_name)
  
  bind_rows(main, average) %>% 
    set_names(c("Ratings For", names(.)[-1])) 
  
}

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
    rename(To = reviewee_name,
           From = reviewer_name,
           Question = question,
           Comment = comment) 
  
}

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


make_apsc_tables <- function(x, type) {
  
  var <- switch(type,
                "mark" = list("q1_mark","q2_mark"),
                "comment" = "comment")
  
  if (type == "mark") {
    
    # "Intellectual and Technical Contribution"
    tables <- x %>%
      mutate(team_number = team) %>% 
      group_by(team_number) %>% 
      nest() %>% 
      mutate(table1 = map(data, ~apsc_mark_tables(.x, var = var[[1]])),
             table2 = map(data, ~apsc_mark_tables(.x, var = var[[2]])))
    
  } else {
    
    #Peer Comment Tables
    tables <- x %>% 
      mutate(team_number = team) %>% 
      group_by(team_number) %>% 
      nest() %>% 
      mutate(comments = map(data, ~apsc_comment_tables(.x, var = var)))
  
  }
  return(tables)
}

make_tables <- function(x, survey, type) {
  
  switch(survey,
         "apsc" = make_apsc_tables(x, type = type),
         "teamq_student" = make_teamq_tables(x),
         "teamq_diagnostic" = make_teamq_diag_tables(x))
}

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



flag_teams <- function(x) {
  x %>%
    select(team, reviewee_id, q13_mark) %>%
    group_by(reviewee_id) %>%
    summarize(flag = mean(q13_mark, na.rm = TRUE)) %>%
    mutate(flag = ifelse(flag < 21, TRUE, FALSE)) %>%
    select(flag) %>%
    map_lgl( ~ any(.x))
}


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
  
  
  total <- filter(survey_data, question == 13, type == "mark") %>% 
    left_join(teamq, by = "question") %>% 
    group_by(team, to, from, scales) %>% 
    summarize(value = mean(as.numeric(value), na.rm = TRUE)) 
  
  marks <- filter(survey_data, !(question %in% 13:14)) %>% 
    left_join(teamq, by = "question") %>% 
    group_by(team, to, from, scales) %>% 
    summarize(value = mean(as.numeric(value), na.rm = TRUE)) %>% 
    mutate(value = cut(value, breaks = 0:5, labels =  c("Never", "Sometimes", "Usually", "Often", "Always")))
  
  
  if (nrow(filter(survey_data, question %in% 13:14,  type == "comment", !is.na(value))) > 0) {
    analyze_sentiment <- filter(survey_data, question %in% 13:14,  type == "comment")  %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words, by = "word") %>% 
      inner_join(filter(sentiments,lexicon == "bing"), by = "word") %>% 
      group_by(question, from, to) %>% 
      count(word, sentiment)
    
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
        summarize(net =  case_when(sum(net) < -2 ~ "Very Negative",
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
  
  
  mark_plot <-  ggplot(marks, aes(x = to, y = from, fill = value)) +
    geom_tile(color = "grey20") +
    scale_fill_viridis(limits = c("Never", "Sometimes", "Usually", "Often", "Always"), na.value = "white", discrete = TRUE) +
    facet_wrap(~scales) +
    scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
    labs(x = "To",
         y = "From",
         title = "Team Q Subscales Heatmap",
         subtitle = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank())
  
  total <- ggplot(total, aes(x = to, y = from, fill = value)) +
    geom_tile(color = "grey20") +
    geom_text(aes(label = value)) +
    scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
    scale_fill_viridis(limits = c(0, round_any(max(total$value, na.rm = TRUE), 10, ceiling)), na.value = "white", breaks = seq(0, round_any(max(total$value, na.rm = TRUE), 10, ceiling),by = 5)) +
    theme_minimal() +
    labs(x = "To",
         y = "From",
         title = "Points Distribution",
         subtitle = "Students assign points to each team member from a pool of 120") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank())
  
  if (nrow(filter(survey_data, question %in% 13:14,  type == "comment", !is.na(value))) > 0) {
    
    comments <- ggplot(comment_sent, aes(x = to, y = from)) +
      geom_tile(aes(fill = net), color = "grey20") +
      facet_wrap(~question, labeller = as_labeller(lookup)) +
      scale_x_discrete(labels = function(x) str_wrap(x, 5)) +
      labs(x = "To",
           y = "From",
           title = "Sentiment analysis of comments",
           subtitle = "Net sentiment assigned by frequency of positive/negative words") +
      scale_fill_manual("Net Sentiment", values = setNames(brewer.pal(5, "RdBu"), c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive")), limits = c("Very Negative","Slightly Negative", "Neutral", "Slightly Positive", "Very Positive"), na.value = "white") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid = element_blank())
  } 
  
  if (exists("comments")) {
    grid.arrange(mark_plot, total, comments,  nrow = 3, top = sprintf("Team %s", team_no)) 
  } else {
    grid.arrange(mark_plot, total, nrow = 2, top = sprintf("Team %s", team_no))
  }
}

