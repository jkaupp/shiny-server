grasp_mark_tables <- function(x, var) {
  
  main <- select_(x, ~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, var) %>% 
    mutate(reviewer_name = paste(reviewer_first_name,reviewer_last_name, sep = " ")) %>% 
    mutate(reviewee_name = paste(reviewee_first_name,reviewee_last_name, sep = " ")) %>% 
    arrange(reviewer_last_name, reviewee_last_name) %>%
    select_(~reviewee_name, ~reviewer_name, var) %>%
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    spread_("reviewee_name", var) 
  
  average <- summarize_each(main, funs(mean(., na.rm = TRUE)), -reviewer_name) %>% 
    mutate(reviewer_name = "Average") %>% 
    mutate_each(funs(trunc), -reviewer_name)
  
  #adj_average <- 
  
  bind_rows(main, average) %>% 
    set_names(c("Ratings For", names(.)[-1])) 
  
}

grasp_comment_tables <- function(x, var) {
  
  select_(x,~reviewer_last_name, ~reviewer_first_name, ~reviewee_last_name, ~reviewee_first_name, ~contains(var)) %>% 
    mutate(reviewer_name = paste(reviewer_first_name,reviewer_last_name, sep = " ")) %>% 
    mutate(reviewee_name = paste(reviewee_first_name,reviewee_last_name, sep = " ")) %>% 
    gather("item","comment", contains("comment")) %>% 
    separate(item, c("question","drop"), sep = "_") %>% 
    arrange(reviewee_last_name, reviewer_last_name, question) %>% 
    mutate(reviewee_name = factor(reviewee_name,levels = unique(reviewee_name))) %>% 
    mutate(reviewer_name = factor(reviewer_name,levels = unique(reviewer_name))) %>% 
    mutate(comment = str_replace_all(comment, "[\r]" , "")) %>% 
    mutate(comment = str_replace_all(comment, "[^[:alnum:]]", " ")) %>% 
    select(reviewee_name, reviewer_name, question, comment) %>% 
    rename(To = reviewee_name,
           From = reviewer_name,
           Question = question,
           Comment = comment) 
  
}

make_tables <- function(data, type) {
  
  var <- switch(type,
                "mark" = list("q1_mark","q2_mark"),
                "comment" = "comment")
  
  if (type == "mark") {
    
    # "Intellectual and Technical Contribution"
    tables <- data %>%
      mutate(team_number = team) %>% 
      group_by(team_number) %>% 
      nest() %>% 
      mutate(table1 = map(data, ~ grasp_mark_tables(.x, var = var[[1]])),
             table2 = map(data, ~ grasp_mark_tables(.x, var = var[[2]])))
    
  } else {
    
    #Peer Comment Tables
    tables <- data %>% 
      mutate(team_number = team) %>% 
      group_by(team_number) %>% 
      nest() %>% 
      mutate(comments = map(data, ~ grasp_comment_tables(.x, var = var)))
  
    tables %>% 
      select(team_number, comments) %>% 
      filter(grepl("Team 10A", team_number)) %>% 
      unnest()
    
    
    #teams <- sprintf("Peer Comments Team: %s\\\n", names(comment_tables))
  }
  return(tables)
}
