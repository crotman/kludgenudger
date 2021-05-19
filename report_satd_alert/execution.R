


correlate_satd_pmd(
  code_location = "C:/doutorado/ant/ant-rel-1.10.10",
  output_file = here::here("temp/ant.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/joda-time/joda-time-2.10",
  output_file = here::here("temp/jodatime.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/ArgoUML/0_35",
  output_file = here::here("temp/argouml.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/kafka-2.7.1/kafka-2.7.1",
  output_file = here::here("temp/kafka.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/hive-rel-release-2.3.8/hive-rel-release-2.3.8",
  output_file = here::here("temp/hive.rds")
)





resultados_ant <- read_rds(here::here("temp/ant.rds"))
resultados_jodatime <- read_rds(here::here("temp/jodatime.rds"))
resultados_argouml <- read_rds(here::here("temp/argouml.rds"))
resultados_kafka <- read_rds(here::here("temp/kafka.rds"))
resultados_hive <- read_rds(here::here("temp/hive.rds"))


resultados_ant$summarised_data
resultados_jodatime$summarised_data
resultados_argouml$summarised_data
resultados_kafka$summarised_data
resultados_hive$summarised_data

resultados_jodatime$p_value 
resultados_ant$p_value
resultados_argouml$p_value
resultados_kafka$p_value







