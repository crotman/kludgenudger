


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

correlate_satd_pmd(
  code_location = "C:/doutorado/junit/junit5-r5.7.2/junit5-r5.7.2",
  output_file = here::here("temp/junit.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/spark-2.9.3", 
  output_file = here::here("temp/spark.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/netbeans-12.2.2",
  output_file = here::here("temp/netbeans.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/pulsar-2.7.2/pulsar-2.7.2",
  output_file = here::here("temp/pulsar.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/RxJava-3.0.12/RxJava-3.0.12",
  output_file = here::here("temp/rxjava.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/glide-4.12.0/glide-4.12.0",
  output_file = here::here("temp/glide.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/kubernetes-10.0.1/java-client-parent-java-10.0.1",
  output_file = here::here("temp/kubernetes.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/lottie-android-3.7.0/lottie-android-3.7.0",
  output_file = here::here("temp/lottie.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/EventBus-3.2.0/EventBus-3.2.0",
  output_file = here::here("temp/eventbus.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/joda-money-1.0.1/joda-money-1.0.1",
  output_file = here::here("temp/jodamoney.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/dbeaver-21.0.5/dbeaver-21.0.5",
  output_file = here::here("temp/dbeaver.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/MPAndroidChart-3.1.0/MPAndroidChart-3.1.0",
  output_file = here::here("temp/mpandroidchart.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/checkstyle-checkstyle-8.42/checkstyle-checkstyle-8.42",
  output_file = here::here("temp/checkstyle.rds")
)

correlate_satd_pmd(
  code_location = "C:\doutorado\presto-0.253/presto-0.253",
  output_file = here::here("temp/presto.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/storm-2.2.0/storm-2.2.0",
  output_file = here::here("temp/storm.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/camel-camel-3.10.0/camel-camel-3.10.0",
  output_file = here::here("temp/camel.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/Activiti-7.1.338/Activiti-7.1.338",
  output_file = here::here("temp/activiti.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/ExoPlayer-r2.14.0/ExoPlayer-r2.14.0",
  output_file = here::here("temp/exoplayer.rds")
)


correlate_satd_pmd(
  code_location = "C:/doutorado/hbase-rel-2.2.7/hbase-rel-2.2.7",
  output_file = here::here("temp/hbase.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/beam-2.29.0/beam-2.29.0",
  output_file = here::here("temp/beam.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/spring-security-5.5.0/spring-security-5.5.0",
  output_file = here::here("temp/spring.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/tomcat-10.0.6/tomcat-10.0.6",
  output_file = here::here("temp/tomcat.rds")
)

correlate_satd_pmd(
  code_location = "C:/doutorado/redisson-redisson-3.15.5/redisson-redisson-3.15.5",
  output_file = here::here("temp/redisson.rds")
)









resultados_ant <- read_rds(here::here("temp/ant.rds"))
resultados_jodatime <- read_rds(here::here("temp/jodatime.rds"))
resultados_argouml <- read_rds(here::here("temp/argouml.rds"))
resultados_kafka <- read_rds(here::here("temp/kafka.rds"))
resultados_hive <- read_rds(here::here("temp/hive.rds"))
resultados_junit <- read_rds(here::here("temp/junit.rds"))
resultados_spark <- read_rds(here::here("temp/spark.rds"))
resultados_netbeans <- read_rds(here::here("temp/netbeans.rds"))
resultados_pulsar <- read_rds(here::here("temp/pulsar.rds"))
resultados_rxjava <- read_rds(here::here("temp/rxjava.rds"))
resultados_glide <- read_rds(here::here("temp/glide.rds"))
resultados_kubernetes <- read_rds(here::here("temp/kubernetes.rds"))
resultados_lottie <- read_rds(here::here("temp/lottie.rds"))
resultados_eventbus <- read_rds(here::here("temp/eventbus.rds"))
resultados_jodamoney <- read_rds(here::here("temp/jodamoney.rds"))
resultados_dbeaver <- read_rds(here::here("temp/dbeaver.rds"))
resultados_mpandroidchart <- read_rds(here::here("temp/mpandroidchart.rds"))
resultados_checkstyle <- read_rds(here::here("temp/checkstyle.rds"))
resultados_presto <- read_rds(here::here("temp/presto.rds"))
resultados_storm <- read_rds(here::here("temp/storm.rds"))
resultados_camel <- read_rds(here::here("temp/camel.rds"))
resultados_activiti <- read_rds(here::here("temp/activiti.rds"))
resultados_exoplayer <- read_rds(here::here("temp/exoplayer.rds"))
resultados_hbase <- read_rds(here::here("temp/hbase.rds"))
resultados_beam <- read_rds(here::here("temp/beam.rds"))
resultados_spring <- read_rds(here::here("temp/spring.rds"))
resultados_tomcat <- read_rds(here::here("temp/tomcat.rds"))
resultados_redisson  <- read_rds(here::here("temp/redisson.rds"))




resultados_ant <- calculate_summaries_satd_alerts(resultados_ant$data)
resultados_jodatime <- calculate_summaries_satd_alerts(resultados_jodatime$data)
resultados_argouml <- calculate_summaries_satd_alerts(resultados_argouml$data)
resultados_kafka <- calculate_summaries_satd_alerts(resultados_kafka$data)
resultados_hive <- calculate_summaries_satd_alerts(resultados_hive$data)
resultados_junit <- calculate_summaries_satd_alerts(resultados_junit$data)
resultados_spark <- calculate_summaries_satd_alerts(resultados_spark$data)
resultados_netbeans <- calculate_summaries_satd_alerts(resultados_netbeans$data)
resultados_pulsar <- calculate_summaries_satd_alerts(resultados_pulsar$data)
resultados_rxjava <- calculate_summaries_satd_alerts(resultados_rxjava$data)
resultados_glide <- calculate_summaries_satd_alerts(resultados_glide$data)
resultados_kubernetes <- calculate_summaries_satd_alerts(resultados_kubernetes$data)
resultados_lottie <- calculate_summaries_satd_alerts(resultados_lottie$data)
resultados_eventbus <- calculate_summaries_satd_alerts(resultados_eventbus$data)
resultados_jodamoney <- calculate_summaries_satd_alerts(resultados_jodamoney$data)
resultados_dbeaver <- calculate_summaries_satd_alerts(resultados_dbeaver$data)
resultados_mpandroidchart <- calculate_summaries_satd_alerts(resultados_mpandroidchart$data)
resultados_checkstyle <- calculate_summaries_satd_alerts(resultados_checkstyle$data)
resultados_presto <- calculate_summaries_satd_alerts(resultados_presto$data)
resultados_storm <- calculate_summaries_satd_alerts(resultados_storm$data)
resultados_camel <- calculate_summaries_satd_alerts(resultados_camel$data)
resultados_activiti <- calculate_summaries_satd_alerts(resultados_activiti$data)
resultados_exoplayer <- calculate_summaries_satd_alerts(resultados_exoplayer$data)
resultados_hbase <- calculate_summaries_satd_alerts(resultados_hbase$data)
resultados_beam <- calculate_summaries_satd_alerts(resultados_beam$data)
resultados_spring <- calculate_summaries_satd_alerts(resultados_spring$data)
resultados_tomcat <- calculate_summaries_satd_alerts(resultados_tomcat$data)
resultados_redisson <- calculate_summaries_satd_alerts(resultados_redisson$data)



resultados_ant$summarised_data
resultados_jodatime$summarised_data
resultados_argouml$summarised_data
resultados_kafka$summarised_data
resultados_hive$summarised_data
resultados_junit$summarised_data
resultados_spark$summarised_data
resultados_netbeans$summarised_data
resultados_pulsar$summarised_data
resultados_rxjava$summarised_data
resultados_glide$summarised_data
resultados_kubernetes$summarised_data
resultados_lottie$summarised_data
resultados_eventbus$summarised_data
resultados_jodamoney$summarised_data
resultados_dbeaver$summarised_data
resultados_mpandroidchart$summarised_data
resultados_checkstyle$summarised_data
resultados_presto$summarised_data
resultados_storm$summarised_data
resultados_camel$summarised_data
resultados_activiti$summarised_data
resultados_exoplayer$summarised_data
resultados_hbase$summarised_data
resultados_beam$summarised_data
resultados_flatbuffers$summarised_data
resultados_spring$summarised_data
resultados_tomcat$summarised_data


resultados_jodatime$p_value 
resultados_ant$p_value
resultados_argouml$p_value
resultados_kafka$p_value
resultados_hive$p_value
resultados_junit$p_value
resultados_spark$p_value
resultados_netbeans$p_value
resultados_pulsar$p_value
resultados_rxjava$p_value
resultados_glide$p_value
resultados_kubernetes$p_value
resultados_lottie$p_value
resultados_eventbus$p_value
resultados_jodamoney$p_value
resultados_dbeaver$p_value
resultados_mpandroidchart$p_value
resultados_checkstyle$p_value
resultados_presto$p_value
resultados_storm$p_value
resultados_camel$p_value
resultados_activiti$p_value
resultados_exoplayer$p_value



extract_summary <- function(name){
  

  if (str_detect(string = name, pattern = "resultado")){
    
    local_results <- .GlobalEnv[[name]]
    
    n_classes <- local_results$data$class_blocks %>% unique() %>% length()
    
    n_methods <- local_results$data %>% 
      count(          package_blocks,
                      class_blocks,
                      method_blocks,
                      id_blocks
      ) %>% 
      nrow()
    
    n_methods_satd <- local_results$n_satd
    
    prop_alerts_satd <- local_results$prop_satd
    
    prop_alerts_no_satd <- local_results$prop_no_satd
    
    greater_class <- if_else(prop_alerts_no_satd > prop_alerts_satd, "NO SATD", "SATD" )
    
    p_value <- local_results$p_value
    
    output <- tibble(
      project = str_remove(string = name, pattern = "resultados\\_" ),
      n_classes = n_classes,
      n_methods = n_methods,
      n_methods_satd = n_methods_satd,      
      prop_alerts_satd = prop_alerts_satd,
      prop_alerts_no_satd = prop_alerts_no_satd,
      greater_class = greater_class,
      p_value = p_value
    )
    
    
  } else {
    output <- NULL  
  }
  
  print(name)
   
  output
   
}



outputs <- map_df(.x = names(.GlobalEnv), .f = extract_summary )




