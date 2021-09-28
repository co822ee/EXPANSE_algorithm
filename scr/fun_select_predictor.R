select_predictor <- function(df_all){
   if(length(unique(df_all$year))==1){
      exc_names <- c('system.index', 'obs', 'sta_code', 'component_caption', '.geo', 'year', 
                     'cntr_code', 'xcoord', 'ycoord', 'sta_type')
   }else{
      exc_names <- c('system.index', 'obs', 'sta_code', 'component_caption', '.geo',
                     'cntr_code', 'xcoord', 'ycoord', 'sta_type')
   }
   pred_c <- names(df_all)[!(names(df_all)%in%exc_names)]
   pred_c
}