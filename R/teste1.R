

  
tidytext::parts_of_speech %>% 
  mutate(
    word = SnowballC::wordStem(word)
  )