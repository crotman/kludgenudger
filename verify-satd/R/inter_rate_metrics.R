
library(tidyverse)
library(grid)
library(scales)
library(ggforce)
library(ggrepel)   
library(irr)

label_answer <- tribble(
  ~answer,           ~label,                            ~value,
  "Certainly No",    "Why not?",                        -2,
  "No",    "Why not?",                        -2,
  "Probably No",     "Why not 'Certainly No'?",         -1,
  "Unknown",         "Why unkown?",                     0,
  "Probably Yes",     "Why not 'Certainly Yes'?",       1,
  "Certainly Yes",    "",                                2,
  "Yes",    "",                                2
)


# Interpretation
# Cicchetti (1994)[19] gives the following often quoted guidelines for interpretation for kappa or ICC inter-rater agreement measures:
#   
#   Less than 0.40—poor.
# Between 0.40 and 0.59—fair.
# Between 0.60 and 0.74—good.
# Between 0.75 and 1.00—excellent.
# A different guideline is given by Koo and Li (2016):[20]
# 
# below 0.50: poor
# between 0.50 and 0.75: moderate
# between 0.75 and 0.90: good
# above 0.90: excellent

regions <- tribble(
  ~metric,                   ~name,   ~color,             ~min, ~max,   ~reference, 
  "interclass correl coef", "poor",   "red",             0,    0.4,    "Cicchetti (1994)",
  "interclass correl coef", "fair",    "yellow",         0.4,    0.59, "Cicchetti (1994)",
  "interclass correl coef", "good",    "greenyellow",    0.6,    0.75, "Cicchetti (1994)",
  "interclass correl coef", "excellent",    "green",          0.75,    1,          "Cicchetti (1994)",
  
  "interclass correl coef", "poor",   "red",             0,    0.5,    "Koo and Li (2016)",
  "interclass correl coef", "moderate",    "yellow",         0.5,    0.75, "Koo and Li (2016)",
  "interclass correl coef", "good",    "greenyellow",    0.75,    0.90, "Koo and Li (2016)",
  "interclass correl coef", "excellent",    "green",          0.90,    1,  "Koo and Li (2016)",

  "Kendall coef concordance", "No/Weak agreement",   "red",             0,    0.2,    "many sources Internet",
  "Kendall coef concordance", "Moderate",    "yellow",         0.2,    0.45, "many sources Internet",
  "Kendall coef concordance", "Strong",    "greenyellow",    0.45,    0.8, "many sources Internet",
  "Kendall coef concordance", "Perfect",    "green",          0.8,    1,  "many sources Internet",

  "mean Spearman",   "negative",    "red",    -1,    0, "many sources Internet",
  "mean Spearman",   "positove",    "yellow",  0,    1,  "many sources Internet",

  "Krippendorff alpha", "No/Weak agreement",   "red",   0,    0.667,     "Krippendorff",
  "Krippendorff alpha", "tentative",   "yellow",   0.667,    0.8,     "Krippendorff",
  "Krippendorff alpha", "good",   "green",   0.8,    1,     "Krippendorff"

)




combination <- tribble(
    ~id_combination, ~user,          ~name,       ~members,   
    1,               "Bruno",        "All of 3",  3,          
    1,               "Prof. Marcio", "All of 3",  3,
    1,               "Prof. Earl",   "All of 3",  3,
    
    2,               "Prof. Marcio", "Earl & Marcio",  2,
    2,               "Prof. Earl",   "Earl & Marcio",  2,
    
    3,               "Bruno",        "Earl & Bruno",  2,
    3,               "Prof. Earl",   "Earl & Bruno",  2,

    4,               "Bruno",        "Marcio & Bruno",  2,
    4,               "Prof. Marcio", "Marcio & Bruno",  2,

        
  )
  


get_evaluations <- function(con, round = 3){
  
  evaluations <- tbl(con, "evaluations") %>% 
    filter(
      round == local(round)
    ) %>% 
    collect() %>% 
    left_join(
      label_answer,
      by = c("satd" ="answer")
    ) %>% 
    select(
      user,
      satd,
      value,
      id_comment_pk
    )

}


calculate_joint_probability <- function(
  evaluations,
  truncate_probably = FALSE,
  remove_unknown = FALSE
){

  
  results <- combination %>% 
    inner_join(
      evaluations,
      by = c("user") 
    ) %>%
    group_by(
      id_comment_pk,
      id_combination
    ) %>% 
    mutate(
      total = n(),
      has_unknown = sum(value == 0 )
    ) %>% 
    filter(
      members == total
    ) %>% 
    filter(
      has_unknown == 0 | !remove_unknown 
    ) %>% 
    ungroup() %>% 
    mutate(
      truncate_probably = truncate_probably,
      final_value = if_else(truncate_probably, sign(value), value )
    ) %>% 
    group_by(
      id_combination, id_comment_pk, name
    ) %>% 
    summarise(
      satd = str_flatten(satd),
      different_answers = n_distinct(final_value)
    ) %>% 
    group_by(
      name
    ) %>% 
    summarise(
      n = n(),
      agreement = sum(different_answers == 1)
    ) %>% 
    mutate(
      value = agreement/n
    ) %>% 
    mutate(
      metric = "joint probability"
    )

}


calculate_proportion <- function(
  evaluations, 
  simplify_answer_probably = TRUE,
  filter_answered_by_all = TRUE
){
  
  results <- evaluations %>% 
    mutate(
      simplify_answer_probably = simplify_answer_probably,
      final_value = if_else(simplify_answer_probably, sign(value), value )
    ) %>% 
    group_by(
      id_comment_pk
    ) %>% 
    filter(
      !filter_answered_by_all | n() == 3
    ) %>% 
    ungroup() %>% 
    group_by(
      user
    ) %>% 
    mutate(
      n = n()
    ) %>% 
    ungroup() %>% 
    group_by(
      user,
      final_value,
      n
    ) %>% 
    summarise(
      n_category = n()
    ) %>% 
    mutate(
      proportion = n_category/n
    ) %>% 
    mutate(
      answer = case_when(
        simplify_answer_probably & final_value == 1 ~ "Yes & Prob. Yes",
        simplify_answer_probably & final_value == 0 ~ "Unknown",
        simplify_answer_probably & final_value == -1 ~ "No & Prob. No",
        !simplify_answer_probably & final_value == -2 ~ "No",
        !simplify_answer_probably & final_value == 0 ~ "Unknown",
        !simplify_answer_probably & final_value == -1 ~ "Prob. No",
        !simplify_answer_probably & final_value == 1 ~ "Prob. Yes",
        !simplify_answer_probably & final_value == 2 ~ "Yes"
      )
    )

}





plot_results <- function(
  evaluation,
  simplify_answer_probably = FALSE,
  filter_answered_by_all = FALSE
){
  
  data <- calculate_proportion(evaluation, filter_answered_by_all = filter_answered_by_all, simplify_answer_probably = simplify_answer_probably ) %>% 
    ungroup() %>% 
    complete(
      nesting(final_value, answer), nesting(user, n),
      fill = list(
        n_category = 0,
        proportion = 0
      )
    ) %>% 
    mutate(
      answer = fct_reorder(answer, .x = final_value)
    ) 
    
  ggplot(data) +
    geom_col(
      aes(
        x = answer,
        y = proportion,
        fill = answer
      ),
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        label = paste0(percent(proportion, accuracy = 1), "\n",n_category, "/", n),
        color = answer,
        x = answer,
        y = proportion + 0.075 
      ),
      show.legend = FALSE
    ) +
    facet_grid(
      .~user,
      
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      text = element_text(size = 16),
      panel.spacing = unit(2, "lines"),
      axis.text.x = element_text(angle = 90)
    ) +
    scale_fill_manual(
      values = c(
      "Yes & Prob. Yes" = "darkgreen",
      "Unknown" = "bisque4",
      "No & Prob. No" = "darkred",
      "No" = "darkred",
      "Prob. No" = "red",
      "Prob. Yes" = "darkolivegreen4",
      "Yes" = "darkgreen"
      )
    ) +
    scale_color_manual(
      values = c(
        "Yes & Prob. Yes" = "darkgreen",
        "Unknown" = "bisque4",
        "No & Prob. No" = "darkred",
        "No" = "darkred",
        "Prob. No" = "red",
        "Prob. Yes" = "darkolivegreen4",
        "Yes" = "darkgreen"
      )
    ) +
    scale_y_continuous(
      limits = c(0,1),
      labels = percent_format(accuracy = 1)
    )
    

}


plot_joint_probablity <-  function(

  evaluations,
  simplify_answer_probably = TRUE
  
  
){
  
  joint <- calculate_joint_probability(
    evaluations,
    simplify_answer_probably
  )  
  
  ggplot(
    joint
  ) +
    geom_col(
      aes(
        x = name,
        y = joint_probability,
        fill = name
      ),
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        x = name,
        y = joint_probability + 0.05,
        color = name,
        label = paste0(percent(joint_probability, accuracy = 1 ), "\n", agreement, "/", n)
      ),
      show.legend = FALSE
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      text = element_text(size = 16),
      panel.spacing = unit(2, "lines"),
      axis.text.x = element_text(angle = 90)
    ) +
    scale_y_continuous(
      limits = c(0,1),
      labels = percent_format(accuracy = 1)
    ) +
    coord_flip()
  
  
  
}



make_rates_tidy <- function(
  evaluation,
  users = c("Bruno", "Prof. Earl", "Prof. Marcio"),
  truncate_probably = FALSE,
  shuffle_ordinal = FALSE,
  remove_unknown = FALSE
  
) {
  
  
  data <- evaluation %>% 
    select(
      -satd
    ) %>% 
    filter(
      user %in% users
    ) %>% 
    mutate(
      truncate_probably = truncate_probably,
      value = as.integer(if_else(truncate_probably, sign(value), value))
    ) %>% 
    mutate(
      shuffle_ordinal = shuffle_ordinal,
      value = if_else(
        !shuffle_ordinal,
        value,
        case_when(
          value == -2 ~ sample(-100:100, 1),
          value == -1 ~ sample(-100:100, 1),
          value == -0 ~ sample(-100:100, 1),
          value == 1 ~ sample(-100:100, 1),
          value == 2 ~ sample(-100:100, 1)
        )
      )
    ) %>% 
    select(-c(truncate_probably, shuffle_ordinal)) %>% 
    pivot_wider(
      names_from = user,
      values_from = value
    ) %>% 
    filter(
      across(
        -id_comment_pk,
        .fns = ~!is.na(.x)
      )
    ) %>% 
    filter(
      across(
        -id_comment_pk,
        .fns = ~(.x!=0 | !remove_unknown)
      )
    ) %>% 
    select(
      -id_comment_pk
    )
  
  
  
}

calculate_icc <- function(
  evaluation,
  users = c("Bruno", "Prof. Earl", "Prof. Marcio"),
  model = "twoway",
  type = "consistency",
  truncate_probably = FALSE,
  remove_unknown = FALSE
){
  
  data <- make_rates_tidy(
    evaluation = evaluation, 
    users = users, 
    truncate_probably = truncate_probably, 
    shuffle_ordinal = FALSE, 
    remove_unknown = remove_unknown
  )
  
  teste <- irr::icc(data, model = "twoway", type = "consistency")
  
  saida <- tibble(
    value = teste$value,
    lbound = teste$lbound, 
    ubound = teste$ubound ,
    p_value = teste$p.value,
    metric = "interclass correl coef"
  )
  
  saida 
  
}


calculate_krippendorff_alpha <- function(
  evaluation,
  users = c("Bruno", "Prof. Earl", "Prof. Marcio"),
  truncate_probably = FALSE,
  remove_unknown = FALSE
  
){
  
  data <- make_rates_tidy(
    evaluation = evaluation, 
    users = users, 
    truncate_probably = truncate_probably, 
    shuffle_ordinal = FALSE,
    remove_unknown = remove_unknown
  )
  
  teste <- irr::kripp.alpha(data %>% as.matrix() %>% t(), method = "ordinal" ) 

  saida <- tibble(
    value = teste$value,
    p_value = NA,
    metric = "Krippendorff alpha"
    
  )    
  
  saida 
  
}


calculate_kendall <- function(
  evaluation,
  users = c("Bruno", "Prof. Earl", "Prof. Marcio"),
  remove_unknown = FALSE,
  truncate_probably = FALSE
){
  
  data <- make_rates_tidy(
    evaluation = evaluation, 
    users = users, 
    shuffle_ordinal = FALSE,
    remove_unknown = remove_unknown,
    truncate_probably = truncate_probably
  )
  
  teste <- irr::kendall(data, correct = TRUE)
  
  saida <- tibble(
    value = teste$value,
    p_value = teste$p.value,
    metric = "Kendall coef concordance"
    
  )
  
  saida 
  
}



calculate_meanrho <- function(
  evaluation,
  users = c("Bruno", "Prof. Earl", "Prof. Marcio"),
  truncate_probably = FALSE, 
  remove_unknown = FALSE
  
){
  
  data <- make_rates_tidy(
    evaluation = evaluation, 
    users = users, 
    truncate_probably = truncate_probably, 
    shuffle_ordinal = FALSE, 
    remove_unknown = remove_unknown
  )
  
  teste <- irr::meanrho(data)
  
  saida <- tibble(
    value = teste$value,
    p_value = teste$p.value,
    metric = "mean Spearman"
    
  )
  
  saida 
  
}


calculate_all <- function(
  evaluation,
  truncate_probably = FALSE,
  remove_unknown  = FALSE
){
  

  combinations <- combination %>% 
    nest(user = user) %>% 
    rowwise() %>% 
    mutate(
      user = list(user$user)
    ) %>% 
    ungroup()
  
  joint <- calculate_joint_probability(evaluations = evaluation, truncate_probably = truncate_probably , remove_unknown = remove_unknown )
  
  combinations_icc <- combinations %>% 
    mutate(
      results = map(.x = user, .f = ~calculate_icc(evaluation = evaluation, users = .x, truncate_probably = truncate_probably, remove_unknown = remove_unknown) )
    ) %>% 
    unnest(
      results 
    )
  
  combinations_kendall <- combinations %>% 
    mutate(
      results = map(.x = user, .f = ~calculate_kendall(evaluation = evaluation, users = .x, truncate_probably = truncate_probably, remove_unknown = remove_unknown) )
    ) %>% 
    unnest(
      results 
    )

  
  combinations_meanrho <- combinations %>% 
    mutate(
      results = map(.x = user, .f = ~calculate_meanrho(evaluation = evaluation, users = .x, truncate_probably = truncate_probably, remove_unknown = remove_unknown) )
    ) %>% 
    unnest(
      results 
    )

  combinations_kripendorff  <- combinations %>% 
    mutate(
      results = map(.x = user, .f = ~calculate_krippendorff_alpha(evaluation = evaluation, users = .x, truncate_probably = truncate_probably, remove_unknown = remove_unknown) )
    ) %>% 
    unnest(
      results 
    )
  
    
  bind_rows(
    joint,
    combinations_icc,
    combinations_kendall,
    combinations_meanrho,
    combinations_kripendorff
  ) 
  
}


plot_agreement <-  function(
  data
  
){
  
  ggplot(
    data
  ) +
    geom_point(
      aes(
        y = value,
        x = name,
        color = name,
        size = 2
      ),
      show.legend = FALSE
    ) +
    facet_grid(
      metric~.
    ) +
    geom_rect(
      data = regions,
      aes(
        ymin = min,
        ymax = max,
        xmin = 0,
        xmax = 5,
        fill = color
      ),
      alpha = 0.2
    ) +
    scale_fill_identity() +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 16),  
    )
    
    
}








