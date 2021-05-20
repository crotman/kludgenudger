

#' Title
#'
#' @param code_location 
#' @param output_location 
#' @param pmd_location 
#' @param alertrules_location 
#' @param blockrules_location 
#'
#' @import here
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
  output_file
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











