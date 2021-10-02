
sort_comments <- function(
  new_round = TRUE,
  remove_bad_bags = TRUE
){

  con <- dbConnect(RSQLite::SQLite(), "db/db.db")

  last_round <- tbl(con, "sorted_comments") %>% 
    select(
      round
    ) %>% 
    distinct() %>% 
    collect() %>% 
    pull(round) %>% 
    max()

  if(new_round){
    round_insert <- last_round + 1  
  } else {
    round_insert <- last_round
  }

  existing_evaluations <- tbl(con, "evaluations") %>% 
    select(
      id_comment_pk
    ) %>% 
    distinct() 
  
  comments_satd <-  tbl(con, "comments") %>% 
    filter(satd == 1) %>% 
    filter(
      !bad_bags | !remove_bad_bags
    ) %>%
    anti_join(
      existing_evaluations
    ) %>% 
    group_by(
      file, beginline, endline, id_bag
    ) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    collect()
    
  pk_sorted <- comments_satd %>% 
    select(id_comment_pk) %>% 
    sample_n(
      size = nrow(.),
      replace = FALSE
    ) %>% 
    mutate(
      order = row_number(),
      round = round_insert
    ) 

  
  dbWriteTable(
    con,
    name = "sorted_comments",
    pk_sorted,
    append = TRUE
  )
  
}


choose_next_comment <-  function(user, con){
  

  user_param <- user
  
  commented <- tbl("evaluations", src = con) %>% 
    filter(
      user == user_param
    ) 
  
  next_comment <- tbl("sorted_comments", src = con) %>% 
    anti_join(
      commented,
      by = "id_comment_pk"
    ) %>% 
    arrange(
      order
    ) %>% 
    head(n = 1) %>% 
    collect() %>% 
    pull(id_comment_pk)
  
  next_comment
}


