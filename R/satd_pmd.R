

#' Title
#'
#' @param code_location 
#' @param output_location 
#' @param pmd_location 
#' @param alertrules_location 
#' @param blockrules_location 
#'
#' @import here
#' @import readxl
#'
#' @return
#' @export
#'
#' @examples
correlate_satd_pmd <- function(
  code_location = "C:/doutorado/ant/ant-rel-1.10.10",
  output_location = here::here("temp/alerts"),
  pmd_location = here::here("tests/testthat/pmd/bin"),
  alertrules_location = "rulesets/java/quickstart.xml",
  blockrules_location = here::here("tests/testthat/data/blockrules/blockrules_simple.xml"),
  output_file,
  bags = FALSE
){


  alerts <- read_raw_ast_nodes(
    code_location = code_location,
    pmd_location = pmd_location,
    output_location = output_location,
    blockrules_location = alertrules_location,
    include_file_in_output = TRUE
  )
  
  blocks <- read_raw_ast_nodes(
    code_location = code_location,
    pmd_location = here::here("tests/testthat/pmd/bin"),
    output_location = here::here("temp/alerts"),
    blockrules_location = blockrules_location ,
    include_file_in_output = TRUE
  )
  
  comments <- extract_comments_from_directory(
    dir = code_location,
    dest_file = here::here("temp/comments/comments")
  )
  
  comments_grouped <- comments %>% 
    mutate(
      single_line = beginline == endline,
      new_comment = !(file == lag(file) & beginline == lag(endline) + 1 ),
    ) %>% 
    replace_na(
      list(new_comment = TRUE)
    ) %>% 
    group_by(
      file
    ) %>% 
    mutate(id_comment_in_file = cumsum(new_comment)) %>% 
    ungroup() %>% 
    group_by(
      file, id_comment_in_file
    ) %>% 
    summarise(
      id = min(id),
      file = first(file),
      beginline = min(beginline),
      endline = max(endline),
      begincolumn = first(begincolumn),
      endcolumn = last(endcolumn),
      comment = str_flatten(comment),
      n = n() 
    )
  
  write_rds(comments_grouped, file = here::here("temp/comments/comments"))
  
  if(!bags){
    comments <- NULL
    comments_grouped <- NULL
    
    extract_selected_comments(
      path_to_comments = here::here("temp/comments"),
      output = here::here("temp/selected_comments")
    )
    
    selected_comments <- read_rds(here::here("temp/selected_comments")) %>% 
      mutate(file = str_replace_all(
        string = file,
        pattern = "/",
        replacement = "\\\\"
      )
      )
    
    most_frequent <- NULL
    

    
  } else{
    
    comments_grouped <- read_rds(here::here("temp/comments/comments")) %>% 
      ungroup()
    
    
    bagsword <- read_excel(here::here("bagsword/bagsword.xlsx")) %>% 
      group_by(
        id_bag
      ) %>% 
      mutate(
        n_groups = n_distinct(id_bag_group)
      ) %>% 
      mutate(
        id = row_number()  
      ) %>% 
      ungroup() %>% 
      tidytext::unnest_ngrams(
        input = word,
        output = one_word,
        n = 1,
        drop = FALSE
      ) %>% 
      mutate(
        stem = SnowballC::wordStem(one_word)
      ) %>% 
      select(
        -one_word
      ) %>% 
      group_by(
        across(
          .cols = -stem
        )  
      ) %>% 
      summarise(
        word = str_flatten(stem, collapse = " ")
      ) %>% 
      select(-id) %>% 
      mutate(
        length = str_length(word)
      )
    
    lexicon_words <- tidytext::parts_of_speech %>% 
      select(
        word
      ) %>% 
      mutate(
        word = SnowballC::wordStem(word) 
      ) %>% 
      distinct()

    token_1 <- comments_grouped %>% 
      tidytext::unnest_tokens(
        input = comment,
        output = comments_token,
        to_lower = TRUE,
        drop = FALSE
      ) %>% 
      mutate(
        id_word = row_number()  
      ) %>% 
      ungroup() %>% 
      tidytext::unnest_ngrams(
        input = comments_token,
        output = one_word,
        n = 1,
        drop = FALSE
      ) %>% 
      mutate(
        stem = SnowballC::wordStem(one_word)
      ) %>% 
      select(
        -one_word
      ) %>% 
      group_by(
        across(
          .cols = -stem
        )  
      ) %>% 
      summarise(
        comments_token = str_flatten(stem, collapse = " ")
      ) %>% 
      select(-id_word) %>% 
      ungroup()
    
    
    token_2 <- comments_grouped %>% 
      tidytext::unnest_tokens(
        input = comment,
        output = comments_token,
        to_lower = TRUE,
        drop = FALSE,
        token = "ngrams",
        n = 2
      ) %>% 
      mutate(
        id_word = row_number()  
      ) %>% 
      ungroup() %>% 
      tidytext::unnest_ngrams(
        input = comments_token,
        output = one_word,
        n = 1,
        drop = FALSE
      ) %>% 
      mutate(
        stem = SnowballC::wordStem(one_word)
      ) %>% 
      select(
        -one_word
      ) %>% 
      group_by(
        across(
          .cols = -stem
        )  
      ) %>% 
      summarise(
        comments_token = str_flatten(stem, collapse = " ")
      ) %>% 
      select(-id_word) %>% 
      ungroup()
    
    
    token_3 <- comments_grouped %>% 
      tidytext::unnest_tokens(
        input = comment,
        output = comments_token,
        to_lower = TRUE,
        drop = FALSE,
        token = "ngrams",
        n = 3
      ) %>% 
      mutate(
        id_word = row_number()  
      ) %>% 
      ungroup() %>% 
      tidytext::unnest_ngrams(
        input = comments_token,
        output = one_word,
        n = 1,
        drop = FALSE
      ) %>% 
      mutate(
        stem = SnowballC::wordStem(one_word)
      ) %>% 
      select(
        -one_word
      ) %>% 
      group_by(
        across(
          .cols = -stem
        )  
      ) %>% 
      summarise(
        comments_token = str_flatten(stem, collapse = " ")
      ) %>% 
      select(-id_word) %>% 
      ungroup()
    
    
    tokens <- bind_rows(token_1, token_2, token_3) %>% 
      mutate(
        id_token = row_number(),
        length_token = str_length(comments_token)
      ) %>% 
      filter(
        length_token > 10
      )
      
    
    
    token_inner_1 <- tokens %>% 
      filter(
        !str_detect(comment, "licens") 
      ) %>% 
      select(
        id,
        id_comment_in_file,
        comments_token
      ) %>% 
      fuzzyjoin::stringdist_inner_join(
        bagsword,
        by = c("comments_token" = "word" ),
        max_dist = 1,
        distance_col = "distance",
        method = "lv"
      ) %>% 
      filter(
        distance == 1
      ) %>% 
      mutate(
       id_one_word = row_number()
      ) %>% 
      tidytext::unnest_tokens(
        output = one_word,
        input = comments_token,
        drop = FALSE
      ) %>% 
      left_join(
        lexicon_words %>% rename(lexicon_word = word),
        by = c("one_word" = "lexicon_word"),
        keep = TRUE
      ) %>% 
      mutate(
        not_found_word = is.na(lexicon_word)
      ) %>% 
      group_by(
        across(
          .cols = -c(lexicon_word, not_found_word, one_word)
        )
      ) %>% 
      summarise(
        not_found_word = sum(not_found_word)
      ) %>% 
      ungroup() %>% 
      filter(
        not_found_word > 0
      ) %>% 
      select(
        id,
        id_comment_in_file,
        id_bag,
        id_bag_group,
        n_groups
      )
      
    
    token_inner_0 <- tokens %>% 
      filter(
        !str_detect(comment, "licens") 
      ) %>% 
      left_join(
        bagsword,
        by = c("comments_token" = "word" )
      ) %>%
      group_by(
        id,
        id_comment_in_file,
        id_bag
      ) %>% 
      mutate(
        n_match = n_distinct(id_bag_group)
      ) %>% 
      ungroup() %>% 
      filter(
        n_match == n_groups
      ) %>% 
      select(
        id,
        id_comment_in_file,
        id_bag,
        id_bag_group,
        n_groups
      )
    
    token <- bind_rows(
      token_inner_0, token_inner_1
    ) 

    filtered_tokens <- bind_rows(
      token
    ) %>% 
      select(id, id_comment_in_file) %>%        
      distinct()
    

    selected_comments <- comments_grouped %>% 
      semi_join(
        filtered_tokens,
        by = c("id", "id_comment_in_file")
      ) %>%  
      mutate(file = str_replace_all(
        string = file,
        pattern = "/",
        replacement = "\\\\"
      )
      )    
    
    most_frequent <- token %>% 
      group_by(
        id_bag
      ) %>% 
      mutate(
        n_comments = n_distinct( id, id_comment_in_file)
      )


  }
  

  methods <- blocks %>% 
    filter(
      rule == "method"
    ) %>% 
    arrange(
      file,
      beginline
    ) %>% 
    group_by(
      file
    ) %>% 
    mutate(
      last_line_previous = lag(endline)
    ) %>% 
    ungroup() %>% 
    relocate(
      last_line_previous
    ) %>% 
    mutate(
      beginline_extended = if_else(is.na(last_line_previous), beginline, last_line_previous + 1L)
    ) %>% 
    relocate(
      beginline_extended
    ) %>% 
    mutate(
      id = row_number()
    ) %>% 
    rename_with(
      ~str_glue("{.x}_blocks")
    )
  
  blocks <- NULL  

  alerts_treated <- alerts %>% 
    select(
      linha, 
      beginline, 
      endline, 
      rule, 
      ruleset, 
      package, 
      class,
      method,
      priority,
      id_alert,
      file
    ) %>% 
    mutate(
      across(
        .cols = where(is.character),
        .fns = as.factor
      )
    ) %>% 
    mutate(linha = as.integer(linha))

  methods_treated <- methods %>% 
    select(
      beginline_extended_blocks,
      last_line_previous_blocks,
      linha_blocks, 
      beginline_blocks, 
      endline_blocks, 
      rule_blocks, 
      ruleset_blocks, 
      package_blocks, 
      class_blocks,
      method_blocks,
      priority_blocks,
      id_alert_blocks,
      file_blocks,
      id_blocks
    ) %>% 
    mutate(
      across(
        .cols = where(is.character),
        .fns = as.factor
      )
    ) %>% 
    mutate(linha_blocks = as.integer(linha_blocks))
  
    
    alerts_with_methods <- alerts_treated %>%
      left_join(
        methods_treated,
        by = c(
          "file" = "file_blocks",
          "class" = "class_blocks"
         ),
        keep = TRUE
      ) %>% 
      mutate(
        inside = beginline >= beginline_extended_blocks & endline <= endline_blocks
      ) %>% 
      replace_na(
        list(inside = FALSE)
      ) %>% 
      select(
          rule,
          ruleset,
          priority,
          class_blocks,
          package_blocks,
          method_blocks,
          id_blocks,
          inside
      ) %>% 
      group_by(
        rule,
        ruleset,
        priority,
        id_blocks,
        package_blocks,
        class_blocks,
        method_blocks
      ) %>% 
      summarise(
        inside = sum(inside)
      ) %>% 
      filter(
        inside > 0
      ) %>% 
      rename(
        n_alerts = inside
      )

    alerts <- NULL
    
    alerts_per_method <- alerts_with_methods %>% 
      group_by(
        package_blocks,
        class_blocks,
        method_blocks,
        id_blocks
      ) %>% 
      summarise(
        n_alerts = sum(n_alerts)
      )
      

    comments_with_methods <- selected_comments %>%
      left_join(
        methods,
        by = c(
          "file" = "file_blocks"
        )
      ) %>% 
      mutate(
        inside = beginline >= beginline_extended_blocks & endline <= endline_blocks
      ) %>% 
      replace_na(
        list(inside = FALSE)
      ) %>%     
      group_by(
        package_blocks,
        class_blocks,
        method_blocks,
        id_blocks
      ) %>% 
      summarise(
        inside = sum(inside)
      ) %>% 
      filter(
        inside > 0
      ) %>% 
      rename(
        n_comments = inside
      )
    

    methods_satd_alerts <- methods %>% 
      tidylog::left_join(
        alerts_per_method,
        by = c(
          "package_blocks",
          "class_blocks",
          "method_blocks",
          "id_blocks"
        )
      ) %>% 
      left_join(
        comments_with_methods,
        by = c(
          "package_blocks",
          "class_blocks",
          "method_blocks",
          "id_blocks"
        )
      ) %>% 
      replace_na(
        list(
          n_alerts = 0,
          n_comments = 0
        )
      ) %>% 
      mutate(
        has_satd = n_comments > 0,
        has_alert = n_alerts > 0
      ) 
      
    methods_satd_alerts_partition <- methods_satd_alerts %>% 
      group_by(
        has_satd
      ) %>% 
      summarise(
        prop_alerts = sum(has_alert)/ n(),
        alerts_per_method = sum(n_alerts)/ n(),
        n = n()
      )
    
    methods_satd_alerts_prop <- methods_satd_alerts %>% 
      summarise(
        prop_alerts = sum(has_alert)/ n(),
        n = n()
      )
    
    with_satd <- methods_satd_alerts_partition %>% 
      filter(
        has_satd
      ) 
    
    without_satd <- methods_satd_alerts_partition %>% 
      filter(
        !has_satd
      ) 
    
    prop_no_satd <- without_satd$prop_alerts
    
    n_no_satd <- without_satd$n
    
    prop_satd <- with_satd$prop_alerts
    
    n_satd <- with_satd$n
    
    prop_total <- methods_satd_alerts_prop$prop_alerts
    
    p_value <- sum(rbinom(1000000, size = n_satd, prop_total  ) / n_satd > prop_satd)/1000000

    most_frequent <- most_frequent

    data_comments <- comments_grouped %>% 
      select(
        id_comment_in_file,
        id,
        file,
        beginline,
        endline,
        comment
      ) %>% 
      left_join(
        token %>% select(id, id_comment_in_file, id_bag)
      ) %>% 
      mutate(
        satd = !is.na(id_bag)
      ) 

    

    answer <- list(
      data = methods_satd_alerts,
      summarised_data = methods_satd_alerts_partition,
      prop_no_satd = prop_no_satd,
      n_no_satd = n_no_satd,
      prop_satd = prop_satd,
      n_satd = n_satd,
      prop_total = prop_total,
      p_value = p_value,
      most_frequent = most_frequent,
      data_comments = data_comments

    )

    
    write_rds(answer, output_file)
    
    0
        
} 





calculate_summaries_satd_alerts <-  function(
  methods_satd_alerts
){
  
  methods_satd_alerts_partition <- methods_satd_alerts %>% 
    group_by(
      has_satd
    ) %>% 
    summarise(
      prop_alerts = sum(has_alert)/ n(),
      alerts_per_method = sum(n_alerts)/ n(),
      n = n()
    )
  
  methods_satd_alerts_prop <- methods_satd_alerts %>% 
    summarise(
      prop_alerts = sum(has_alert)/ n(),
      n = n()
    )
  
  with_satd <- methods_satd_alerts_partition %>% 
    filter(
      has_satd
    ) 
  
  without_satd <- methods_satd_alerts_partition %>% 
    filter(
      !has_satd
    ) 
  
  prop_no_satd <- without_satd$prop_alerts
  
  n_no_satd <- without_satd$n
  
  prop_satd <- with_satd$prop_alerts
  
  n_satd <- with_satd$n
  
  prop_total <- methods_satd_alerts_prop$prop_alerts


  if(prop_satd > prop_no_satd ){
    p_value <- sum(rbinom(1000000, size = n_satd, prop_total  ) / n_satd > prop_satd)/1000000
  } else{
    p_value <- sum(rbinom(1000000, size = n_satd, prop_total  ) / n_satd < prop_satd)/1000000
  }
  
  
  answer <- list(
    data = methods_satd_alerts,
    summarised_data = methods_satd_alerts_partition,
    prop_no_satd = prop_no_satd,
    n_no_satd = n_no_satd,
    prop_satd = prop_satd,
    n_satd = n_satd,
    prop_total = prop_total,
    p_value = p_value
  )
  
  answer
  
  
}











