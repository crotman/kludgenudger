diffs <- read_lines("teste.diff") %>% 
  enframe(
    name = "diff_line",
    value = "diff_string"
  ) %>% 
  mutate(
    begin_diff = str_detect(diff_string,"^[ ]*diff --git"),
    item = cumsum(begin_diff)
  ) 




