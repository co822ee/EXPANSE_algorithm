proc_in_data <- function(in_data, neg_pred, xcoord="Xcoord", ycoord="Ycoord"){
   # truncate x and y
   in_data$x_trun <- (in_data[,xcoord]-min(in_data[,xcoord]))/(max(in_data[,xcoord])-min(in_data[,xcoord]))
   
   in_data$y_trun <- (in_data[,ycoord]-min(in_data[,ycoord]))/(max(in_data[,ycoord])-min(in_data[,ycoord]))
   # in_data <- in_data %>% mutate(x_trun = (Xcoord-min(Xcoord))/(max(Xcoord)-min(Xcoord)),
   #                               y_trun = (Ycoord-min(Ycoord))/(max(Ycoord)-min(Ycoord)))
   # transform altitude
   # in_data <- in_data %>% mutate(alt_t = sqrt((alt10_enh-min(alt10_enh))/max(alt10_enh-min(alt10_enh))))
   # add year as a grouping factor
   in_data$year <- as.factor(in_data$year)
   # change direction of effect
   in_data %>% as_tibble() %>% mutate(across(matches(neg_pred), function(x) -x ))
}


