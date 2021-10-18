test_that("multiplication works", {
  
  backlog <- create_backlog()
  
  backlog <- add_event(
    time_from_now = 12L,
    now = 0L,
    event_backlog = backlog,
    event_type = commit_event
  ) %>% 
    add_event(
      event_type = commit_event,
      time_from_now = 13L,
      now = 0L
    ) %>% 
    add_event(
      event_type = commit_event,
      time_from_now = 11L,
      now = 0L
    ) %>% 
    add_event(
      event_type = commit_event,
      time_from_now = 11L,
      now = 0L
    )
  
  expect_equal(
    object = is.unsorted(backlog$time),
    expected = FALSE
  )
  
})
