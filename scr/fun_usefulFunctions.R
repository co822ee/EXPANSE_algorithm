plotFunction <- function(i, pred, data_p, 
                         output = T, outpath = '../graph/predGIS/'){   
   # the pollutant conc's name should be NO2
   ggplot(data_p %>% select(matches(pred[[i]]),'NO2') %>%
             gather(pred, values,-'NO2'))+
      geom_point(aes(x=NO2, y=values))+
      facet_wrap(pred~., scales = 'free_y')
   if(output){
      print(pred[[i]])
      ggsave(paste0(outpath, i, '.tiff'), width=8, height=5, 
             dpi=300,units = 'in')
   }
}
gen_train_test <- function(df, seed, frac){
   set.seed(seed)
   # generate training and test data by stratifying the data
   df <- df %>% mutate(id = cur_group_rows())
   id_region <- df %>% dplyr::select(airid, REGION) %>% unique()
   train_id <- id_region %>% group_by(REGION) %>% sample_frac(frac) %>% ungroup
   test_id <- id_region %>% anti_join(train_id, by = 'airid')
   
   train_data <- df %>% filter(airid %in% train_id$airid)
   test_data <- df %>% filter(airid %in% test_id$airid)
   
   training_id <- train_data$id
   test_id <- test_data$id
   return(list(training_id, test_id,
               train_data %>% dplyr::select(-id), 
               test_data %>% dplyr::select(-id)))
   #!?! should I use EU_data %>% group_by(REGION) %>% sample_frac(0.6) %>% ungroup
   #    instead??? Because for some stations there are fewer years available.
   # to test whether stratification is successful
}