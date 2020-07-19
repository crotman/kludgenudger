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

