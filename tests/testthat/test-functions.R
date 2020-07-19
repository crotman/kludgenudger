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
