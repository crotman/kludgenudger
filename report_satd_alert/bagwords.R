library(tidyverse)
library(tidytext)
library(readxl)


bags <- read_excel(here::here("bagwords/bagwords.xlsx"), sheet = "bags") 

bags2 <- read_excel(here::here("bagwords/bagwords.xlsx"), sheet = "bags2") 


comments <- read_rds(here::here("temp/comments/comments")) %>% 
  ungroup()


tokens <- comments %>% 
  unnest_tokens(
    output = "token",
    input = comment,
    drop = FALSE
  ) %>% 
  left_join(
    bags,
    by = c()
  )