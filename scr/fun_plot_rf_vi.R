plot_rf_vi <- function(csv_name, var_no){
   var_importance <- read.csv(paste0('data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   ggplot(var_importance %>% top_n(var_no, vi))+
      geom_col(aes(reorder(var_name, vi), vi),
               position = 'dodge', fill='khaki')+
      coord_flip() +
      theme_light()+
      labs(x = 'variable', y = 'importance value (impurity)',
           title = csv_name)
}