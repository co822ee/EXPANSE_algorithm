change_dir <- function(in_data, neg_pred){
   train_sub %>% as_tibble() %>% mutate(across(matches(neg_pred), function(x) -x ))
}


