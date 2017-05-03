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
    mutate_at("q15", funs(stri_replace_all_regex(., "[\r]" , ""))) %>% 
    mutate_at("q15", funs(stri_replace_all_regex(., "[^[:alnum:]]", " "))) %>%
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
         "teamq" = make_teamq_tables(x))
}


