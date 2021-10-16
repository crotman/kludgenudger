
##constants
commit_event <- 0L
review_event <- 1L 
re_review_event <- 2L 

action_developer_kludge <- 
action_developer_no_kludge 



simulate <- function(){
  
  backlog <- create_backlog()
  
  
  
}






add_event <- function(
  event_backlog,
  event_type,
  time_from_now,
  now
){
  
  new_event <- tibble::tibble(
    event_type = event_type,
    time = time_from_now + now
  ) 
  
  new_backlog <-  structtibble::sorted_bind_rows(
    sorted_tibble = event_backlog,
    binding_tibble = new_event
  )

  new_backlog
}

create_backlog <- function(){
  
  event_backlog <- tibble::tibble(
    event_type = integer(),
    time = integer()
  ) %>% 
    structtibble::sorted_tibble(
      key = time
    ) 
  
}










