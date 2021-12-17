library(dplyr)
library(ggplot2)
nngbs <- lapply(list.files('data/workingData/', 'GWR_nngb_all_'), function(filename){
   poll <- strsplit(filename, '_')[[1]][4]
   yr <- gsub('.txt','',strsplit(filename, '_')[[1]][5])
   
   data.frame(year=yr, poll=poll,
              nngb=read.table(paste0('data/workingData/', filename))[1,1])
}) %>% do.call(rbind,.)
(nngbs %>% filter(poll=='NO2'))$nngb %>% range
(nngbs %>% filter(poll=='O3'))$nngb %>% range
(nngbs %>% filter(poll=='PM10'))$nngb %>% range
(nngbs %>% filter(poll=='PM2.5'))$nngb %>% range
ggplot()+
   geom_line(data=nngbs, aes(x=year, y=nngb, col=poll, group=poll))+
   labs(col='pollutant', y='adaptive bandwidth')+
   theme_bw()
ggsave('graph/nngb.png', width=8, height=3, dpi=200, units='in')
