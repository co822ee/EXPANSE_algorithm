proc_in_data <- function(in_data, neg_pred){
   # truncate x and y
   in_data <- in_data %>% mutate(x = Xcoord-min(Xcoord)/(max(Xcoord)-min(Xcoord)),
                                 y = Ycoord-min(Ycoord)/(max(Ycoord)-min(Ycoord)))
   # transform altitude
   in_data <- in_data %>% mutate(alt_t = sqrt((alt10_enh-min(alt10_enh))/max(alt10_enh-min(alt10_enh))))
   # change direction of effect
   in_data %>% as_tibble() %>% mutate(across(matches(neg_pred), function(x) -x ))
}


