library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
target_poll <- 'NO2'
files <- list.files('data/processed/gee/', paste0('predictionsAll_', target_poll,'_'))
no2 <- lapply(paste0('data/processed/gee/', files), read.csv)  #add a index
no2 <- do.call(cbind, no2)
names(no2)
exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo')
no2_clean <- no2[,!(names(no2)%in%exc_names)]
names(no2_clean)
yrs_name <- substr(names(no2_clean), nchar(names(no2_clean))-3, nchar(names(no2_clean))) %>% as.numeric()
no2_clean <- no2_clean[, order(yrs_name)]
# no2_clean <- no2_clean[, mixedsort(names(no2_clean))]
names(no2_clean)
yr=2008
no2_clean_p <- no2_clean[, grepl(yr, names(no2_clean))]
# gather(no2_clean_p, 'model', 'values', )

plotM <- function(data_df, colorZone=F, lim_range){

   # data_df <- dcast(data_df,
   #                    # as.formula(paste0('station_european_code+zone.name~', time_var)), 
   #                    value.var = 'conc')
   
   upper.panel <- function(x, y){
      points(x, y,xlim=c(-2,220), ylim=c(-2,220))
      abline(0,1, col='red')
   }
   lowerFn <- function(data, mapping, ...) {
      p <- ggplot(data = data, mapping = mapping) +
         geom_point(colour = "black") +
         geom_abline(intercept=0, slope=1, col='red')+
         lims(x=lim_range,y=lim_range)
      p
   }
   if(colorZone){
      ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)),
              mapping=ggplot2::aes(colour=zone.name))
   }else{
      ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)))
   }
   
}
# png(paste0("graph/randomPoints", target_poll, '.png'),
#      height=12, width=15, units='in', res=300)
# plotM(no2_clean, F, range_limit)
# dev.off()
if(target_poll=='NO2'){
   range_limit <- c(0, 125)
}else{
   range_limit <- c(0, 35)
}

plotM(no2_clean[, grepl('2018', names(no2_clean))], F, range_limit)
plotM(no2_clean[, grepl('2012', names(no2_clean))], F, range_limit)
plotM(no2_clean[, grepl('2009', names(no2_clean))], F, range_limit)
plotM(no2_clean[, grepl('2000', names(no2_clean))], F, range_limit)

# png(paste0("graph/randomPoints", target_poll, '_slr.png'),
#     height=12, width=15, units='in', res=300)
# plotM(no2_clean[, grepl('slr', names(no2_clean))], F, range_limit)
# dev.off()

png(paste0("graph/randomPoints", target_poll, '_gwr.png'),
    height=12, width=15, units='in', res=300)
plotM(no2_clean[, grepl('gwr', names(no2_clean))], F, range_limit)
dev.off()

png(paste0("graph/randomPoints", target_poll, '_rf.png'),
    height=12, width=15, units='in', res=300)
plotM(no2_clean[, grepl('rf', names(no2_clean))], F, range_limit)
dev.off()

