library(readr)
library(stringr)


test_that("map_lines works", {
  caso_1 <- read_rds("data/info_map_lines_info_teste_1.rds")
  expect_equal(
    caso_1$function_output, 
    map_lines(
      file = str_glue("data/caso1_map_lines/{caso_1$file}"),
      lines_prev_param = caso_1$lines_prev_param,
      lines_post_param = caso_1$lines_post_param
    )
  )
  
  
  caso_2 <- read_rds("data/info_map_lines_info_teste_2.rds")
  expect_equal(
    caso_1$function_output, 
    map_lines(
      file = str_glue("data/caso1_map_lines/{caso_2$file}"),
      lines_prev_param = caso_1$lines_prev_param,
      lines_post_param = caso_1$lines_post_param
    )
  )
  
})


test_that("decorate_code_and_alerts works", {
  caso_1 <- read_rds("data/info_decorate_code_and_alerts.rds")
  expect_equal(
    caso_1$function_output, 
    decorate_code_and_alerts(
      strings = caso_1$strings,
      alerts = caso_1$alerts,
      region_only = caso_1$region_only,
      region_size = caso_1$region_size
    )
  )
  
  
})


test_that("decorate_code_alerts_mapped works", {
  caso_1 <- read_rds("data/info_decorate_code_alerts_mapped.rds")
  expect_equal(
    caso_1$function_output, 
    decorate_code_alerts_mapped(
      strings_old_param = caso_1$strings_old_param,
      alerts_old_param = caso_1$alerts_old_param,
      strings_new_param = caso_1$strings_new_param,
      alerts_new_param = caso_1$alerts_new_param,
      map_param = caso_1$map_param,
      region_only = caso_1$region_only,
      region_size = caso_1$region_size
      
    )
  )
})


test_that("extract_piece_of_code works", {
  expect_equal(
    "int varX = function(paramX, paramY);", 
  extract_piece_of_code(
      strings_param = read_lines("data/caso1_extract_piece_of_code/code.java") %>% str_flatten("\n"),
      begin_line = 9,
      end_line = 9,
      begin_column = 9,
      end_column = 44
    )
  )
})

test_that("read_raw_ast_nodes works", {
  caso_1 <- read_rds("data/info_read_raw_ast_nodes.rds")
  expect_equal(
    caso_1$function_output, 
    read_raw_ast_nodes(
      code_location = "data/caso1_read_raw_ast_nodes/code.java",
      output_location = "data/caso1_read_raw_ast_nodes/code.xml",
      pmd_location = "C:/doutorado/AnaliseTwitter4j/pmd/bin",
      blockrules_location = "data/blockrules/blockrules.xml"
    )
  )
})


test_that("generate_ast_tree_from_raw_nodes works", {
  caso_1 <- read_rds("data/info_generate_ast_tree_from_raw_nodes.rds")
  expect_equal(
    caso_1$function_output %>% tidygraph::activate(nodes) %>%  as_tibble(), 
    generate_ast_tree_from_raw_nodes(
      nodes = caso_1$nodes
    ) %>% tidygraph::activate(nodes) %>% as_tibble()
  )
  expect_equal(
    caso_1$function_output %>% tidygraph::activate(edges) %>%  as_tibble(), 
    generate_ast_tree_from_raw_nodes(
      nodes = caso_1$nodes
    ) %>% tidygraph::activate(edges) %>% as_tibble()
  )
})


test_that("cross_versions works", {
  caso_1 <- read_rds("data/info_cross_versions.rds")
  old <- read_lines("data/caso1_velho/code.java")
  new <- read_lines("data/caso1_new/code.java")
  param <- caso_1$examples_executed %>% 
    mutate(path = c("data/caso1_velho", "data/caso1_new"))
  expect_equal(
    caso_1$function_output$lines_map[[1]] %>%  select(-c(file_prev, file_post)), 
    cross_versions(
      examples_executed = param
    )$lines_map[[1]] %>%  select(-c(file_prev, file_post))
  )
})



test_that("calculate_features works", {
  caso_1 <- read_rds("data/info_calculate_features.rds")
  expect_equal(
    caso_1$function_output, 
    calculate_features(
      graph_old = caso_1$graph_old,
      graph_new = caso_1$graph_new,
      coordinates = caso_1$coordinates
    )
  )
})



test_that("calculate_features_from_versions works", {
  caso_1 <- read_rds("data/info_calculate_features_from_versions.rds")
  expect_equal(
    caso_1$output_function$features$features, 
    calculate_features_from_versions(
      code_file_new = "data/caso1_calculate_features_from_versions_novo/code.java",
      code_file_old = "data/caso1_calculate_features_from_versions_velho/code.java",
      pmd_path = "pmd/bin/pmd.bat"
    )$features$features
  )
})


test_that("extract_comments_from_code works", {
  
  function_output <- tribble(
    ~beginline, ~endline, ~begincolumn, ~endcolumn, ~comment,
    5,          5,        4,            33,         "Comentário que vai dar merda",
    24,         26,       13,           20,         "e outro aqui\n            pulando linha\n            bonito",
    28,         28,       13,           18,         "aqui",
    33,         33,        9,           24,         "aqui mais um"           
  ) %>% 
    mutate(
      across(where(is.numeric), as.integer)
    )
  
  
  expect_equal(
    function_output, 
    extract_comments_from_code(
      file_path = "data/caso1_extract_comments_from_code/code.java"
    )
  )
})


test_that("decide_heurist_if_same_alert works", {
  
  features_input <- tribble(
    ~same_rule,~same_id_group,~same_method_group,~same_method_name,~same_block,~same_code,~same_method_code,~dist_line,~dist_line_normalized_block,~dist_line_normalized_method,~dist_line_normalized_unit,
    TRUE,      TRUE,          TRUE,              TRUE,             TRUE,       TRUE,      TRUE,             0,         0,                          0,                           0                         ,                                
    TRUE,      FALSE,          FALSE,              TRUE,             FALSE,       TRUE,      TRUE,             0,         1,                          0,                           0                        , 
    TRUE,      TRUE,          TRUE,              TRUE,             TRUE,       FALSE,      FALSE,             0,         0,                          0,                           0                         ,                                
    TRUE,      FALSE,          TRUE,              TRUE,             TRUE,       TRUE,      FALSE,             4,         0,                          0,                           0                         ,                                
    FALSE,      TRUE,          TRUE,              TRUE,             TRUE,       TRUE,      TRUE,             0,         0,                          0,                           0                         ,                                
    TRUE,      FALSE,          TRUE,              TRUE,             TRUE,       TRUE,      FALSE,             6,         0,                          0,                           0                         ,                                
  )

  
  output <- tribble(
    ~same_alert,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE
    
  )

  expect_equal(
    output, 
    decide_heurist_if_same_alert(
      clean_calculated_features = features_input
    )
  )
})

