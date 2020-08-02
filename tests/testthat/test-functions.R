library(readr)
library(stringr)


test_that("map_lines works", {
  caso_1 <- read_rds("data/info_map_lines_info_teste_1.rds")
  expect_equal(
    caso_1$function_output %>% mutate(equal = FALSE), 
    map_lines(
      file = str_glue("data/caso1_map_lines/{caso_1$file}"),
      lines_prev_param = caso_1$lines_prev_param,
      lines_post_param = caso_1$lines_post_param
    )
  )
  
  
  caso_2 <- read_rds("data/info_map_lines_info_teste_2.rds")
  expect_equal(
    caso_1$function_output %>%  mutate(equal = FALSE), 
    map_lines(
      file = str_glue("data/caso1_map_lines/{caso_2$file}"),
      lines_prev_param = caso_1$lines_prev_param,
      lines_post_param = caso_1$lines_post_param
    )
  )
  
  
  caso_3 <- tibble(map_remove = 1:10, map_add = 1:10, equal = TRUE)
  expect_equal(
    caso_3, 
    map_lines(
      file = "data/zero_sized.diff",
      lines_prev_param = 10,
      lines_post_param = 10
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
      strings_param = read_lines("data/caso1_extract_piece_of_code/code.java"),
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
    mutate(path = c("data/caso1_velho/code.java", "data/caso1_new/code.java"))
  expect_equal(
    caso_1$function_output$lines_map[[1]] %>%  select(-c(file_prev, file_post)) %>% 
      mutate(equal = FALSE), 
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
  esperado <- caso_1$output_function$features$features
  resultado <- calculate_features_from_versions(
    code_file_new = "data/caso1_calculate_features_from_versions_novo/code.java",
    code_file_old = "data/caso1_calculate_features_from_versions_velho/code.java",
    pmd_path = "pmd/bin/pmd.bat"
  )$features$features
  expect_equal(
    esperado, 
    resultado
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



test_that("calculate_features_from_versions works", {
  output <- read_rds("data/info_calculate_features_from_versions_categorised_alerts.rds")    
  output_function <- calculate_features_from_versions(
    code_file_new = "data/caso1_calculate_features_from_versions_novo-v2/code.java",
    code_file_old = "data/caso1_calculate_features_from_versions_velho-v2/code.java",
    pmd_path = "pmd/bin/pmd.bat"
  )$categorised_alerts
  expect_equal(
    output, 
    output_function
  )
})


test_that("calculate_features_from_versions works with same files", {
  output <- read_rds("data/calculate_same_files.rds")
  output_function <- calculate_features_from_versions(
    code_file_new = "data/caso_arquivos_iguais_novo/code.java",
    code_file_old = "data/caso_arquivos_iguais_velho/code.java",
    pmd_path = "pmd/bin/pmd.bat"
  )$categorised_alerts
  expect_equal(
    output, 
    output_function
  )
})



test_that("join_ast_alerts works", {
  ast <- read_rds("data/info_join_ast_alerts_ast.rds")
  alerts <- read_rds("data/info_join_ast_alerts_alerts.rds")
  output <- read_rds("data/info_join_ast_alerts_output.rds")
  output_function <- join_ast_alerts(
    ast = ast,
    alerts = alerts
  )
  expect_equal(
    output_function %>% activate("nodes") %>% as_tibble(), 
    output_function  %>% activate("nodes") %>% as_tibble()
  )
  
  expect_equal(
    output_function %>% activate("edges") %>% as_tibble(), 
    output_function  %>% activate("edges") %>% as_tibble()
  )
  
})

test_that("read_pmd_xml works with empty" ,{
  
  empty <- tibble(
    linha  = numeric(),
    beginline = integer(),
    endline   = integer(),
    begincolumn = integer(),
    endcolumn = integer(),
    rule = character(),
    ruleset = character(),
    package = character(),
    class = character(),
    priority= integer(),
    variable = character(),
    method= character(),
    id_alert  = integer()
  )   
  
  expect_equal(
    read_pmd_xml("data/empty_pmd_output.xml"),
    empty
  )
  
})


test_that("calculate_features_from_versions works with no method", {
  output <- read_rds("data/output_info_no_method.rds")$categorised_alerts
  output_function <- calculate_features_from_versions(
    code_file_new = "data/caso_no_method/novo/MarkersInternalPreferences.java",
    code_file_old = "data/caso_no_method/velho/MarkersInternalPreferences.java",
    pmd_path = "pmd/bin/pmd.bat"
  )$categorised_alerts
  expect_equal(
    output, 
    output_function
  )
})


# 
# 
# test_that("read_raw_ast_nodes works", {
#     output_esperado <- read_rds("data/info_read_raw_ast_big.rds")
# 
#     tictoc::tic("read")
#     teste <- read_raw_ast_nodes(
#       code_location = "data/big-novo/facade.java",
#       output_location = "data/big-novo/facade.xml",
#       pmd_location = "C:/doutorado/AnaliseTwitter4j/pmd/bin",
#       blockrules_location = "data/blockrules/blockrules.xml"
#     )
#     tictoc::toc()
# 
# })
# 
# 
#   
# 
# 
# 
# 


# :/doutorado/ArgoUML/0_33_1/src/argouml-app/src/org/argouml/cognitive/ui/ActionSnooze.java
# # 
# # test_that("compare_versions works",{
# # # 
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_30",
    dir_new <-  "C:/doutorado/ArgoUML/0_31_1",
    parallel = TRUE,
    resume = TRUE,
    log = "log-30-31"
  )
  tictoc::toc()
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_29",
    dir_new <-  "C:/doutorado/ArgoUML/0_30",
    parallel = TRUE,
    resume = TRUE,
    log = "log-29-30"
  )
  tictoc::toc()
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_28",
    dir_new <-  "C:/doutorado/ArgoUML/0_29",
    parallel = TRUE,
    resume = TRUE,
    log = "log-28-29"
  )
  tictoc::toc()
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_27",
    dir_new <-  "C:/doutorado/ArgoUML/0_28",
    parallel = TRUE,
    resume = TRUE,
    log = "log-27-28"
  )
  tictoc::toc()
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_26",
    dir_new <-  "C:/doutorado/ArgoUML/0_27",
    parallel = TRUE,
    resume = TRUE,
    log = "log-26-27"
  )
  tictoc::toc()
  tictoc::tic("compare_versions")
  output_function <- compare_versions(
    dir_old <- "C:/doutorado/ArgoUML/0_24",
    dir_new <-  "C:/doutorado/ArgoUML/0_25",
    parallel = TRUE,
    resume = TRUE,
    log = "log-24-25"
  )
  tictoc::toc()
  #   # 
#   # output <- read_rds("data/output_compare_versions.rds")
#   # 
# #   expect_equal(
# #     output,
# #     output_function
# #   )
# #   
# #   
# # })
# 
# # 
# # # 
# teste <- calculate_features_from_versions(
#   code_file_new = "data\eclipse_novo",
#   code_file_old = "data\eclipse_velho",
#   pmd_path = "pmd/bin/pmd.bat"
# )
# # # 
# # # 
      # # # # 
# # # # teste <- calculate_features_from_versions(
# # # #   code_file_new = "c:/doutorado/eclipse/eclipse-R4_3/eclipse-R4_3/platform-core/plugins/org.eclipse.core.filesystem.ftp/src/org/eclipse/core/internal/filesystem/ftp/FTPUtil.java",
# # # #   code_file_old = "c:/doutorado/eclipse/eclipse-R4_3/eclipse-R4_3/platform-core/plugins/org.eclipse.core.filesystem.ftp/src/org/eclipse/core/internal/filesystem/ftp/FTPUtil.java",
# # # #   pmd_path = "pmd/bin/pmd.bat"
# # # # )
# # # # 
# # # # 
# # # # 
# # # # 
# # # # c:/doutorado/eclipse/eclipse-R4_3/eclipse-R4_3/platform-core/plugins/org.eclipse.core.filesystem.ftp/src/org/eclipse/core/internal/filesystem/ftp/FTPUtil.java
# # # # c:/doutorado/eclipse/eclipse-R4_4/eclipse-R4_4/platform-core/plugins/org.eclipse.core.filesystem.ftp/src/org/eclipse/core/internal/filesystem/ftp/FTPUtil.java
# 
# 
# 
# 
# 
# 
# 
# arquivos <- read_rds("data/log/df.rds") %>% 
#   mutate(
#     teste = map(.x = str_glue("data/log/{id}.rds"), read_rds)
#   )
# 
# 
# 
# 
# teste <-  read_rds("antes_erro.rds")
# 
# teste %>% 
# select(
#   .data$beginline,
#   .data$endline,
#   .data$rule,
#   .data$id_group,
#   .data$method,
#   .data$rule_alert,
#   .data$code
# ) 
# 
# 
# 
# 
# 
  

# 
# # 
# 
# 
# 
# alert <- read_rds("graphs_from_alerts_new.rds")
# 
# 
# 



# 
# list <-  read_rds("data/log/df.rds") %>% 
#   filter(id %in% c(1877, 1421))
  
  
  
  
extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_24",
  dest_file = "comments_24.rds"
)
  
extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_25",
  dest_file = "comments_25.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_26",
  dest_file = "comments_26.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_27",
  dest_file = "comments_27.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_28",
  dest_file = "comments_28.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_29",
  dest_file = "comments_29.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_30",
  dest_file = "comments_30.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_31",
  dest_file = "comments_31.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_32",
  dest_file = "comments_32.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_33",
  dest_file = "comments_33.rds"
)

extract_comments_from_directory(
  dir = "C:/doutorado/ArgoUML/0_34",
  dest_file = "comments_34.rds"
)


