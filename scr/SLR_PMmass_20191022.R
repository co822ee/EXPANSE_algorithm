###### 2019-09-29
###### 2019-06-10
###### adapted 2019-07-31 
###### adapted 2019-09-23 # Restrict potential predictor variables offered to different models --------------------------
###### adapted 2019-10-10 # change industrial variables in the dataset; exclude variables with no variation; new XY values
###### adapted 2019-10-11 # leave only three buffers of industrial variables in the dataset--------------
###### adapted 2019-10-16 # updated extract values from resampled SAT and CTM surfaces (so comparable with RF models)--------------
library(dplyr)
library(tidyr)
library(car)    #vif()
EU_data <- read.csv('data/NO2_2010.csv') %>% na.omit()
pred=c(c('alt10_enh','Xcoord', 'Ycoord', 'XY'),
          'clc10',
          'clc14',
          'clc3',
          'clc5',
          'clc7',
          'MAJRDS_EU',
          'ROADS_EU',
          c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT'),
          'RES')
neg_pred <- c('alt10_enh', 'clc14', 'clc7')
# PM_mass-----------------------------------------
EU_poll <- EU_data %>% select(NO2_2010)
EU_pred <- EU_data %>% select(matches(pred)) %>% 
  mutate_at(EU_data %>% select(matches(neg_pred)) %>% names(), function(x) -x)


############################################################
#### first try: exclude Greece, include emission data (n=374) 
############################################################

############################################################
#### Second try: exclude emission data, include other variable from E-PRTR (n=414)
#### Calculated as amount/distance   ref: ESCAPE model for NOx (Beelen 2013)
#--------------------------------------------------------------------------
#### Third try: exclude emission data, include other variable from E-PRTR (n=414)
#### 
############################################################

All <- cbind(EU_poll,EU_pred)
# remove rows with NA
All <- na.omit(All)
rm(EU_pred)
rm(EU_poll)

# CREATE NEGATIVE  altitude(alt_enh), nature(Nat), urban green(Urbgr), --------------
# -----------------------------------------------------------------
### offer X,Y transform and meteorology at the end
### boundary layer height(blh), total precipitation (tp):
# allow both directions for wind (si10), temperature (t2m), total cloud cover (tcc) 
pred <- All[,-1]
# names(pred)



##########################
#### for FULL model: 
# AllVal <- pred
# PollTest <- All[,1:2]

####################################
## for subset models
## Make TEST set van VALIDATION set
 PollVal <- All[,1]
 AllVal <- pred

 # pred <- pred[pred$strata4 != 5,]     # i guess this is excluding Greece
 # PollTest <- PollVal[PollVal$strata4 != 5,]
 PollTest <- PollVal
################################################

#####now set the values of some variables that will be needed
nfirstvar <- 1  # given that the list of explanatory variables is at the end of the table: specify in which row the first explanatory variable is
nlastvar <- dim(pred)[2] #here the number of the last row is calculated

##### Set the pollutant:    
names(PollTest)                 #SELECT FROM THIS LIST
POLL <- PollTest

##########################################################################
models<-list()

##########################################################################
##    -------1---------------
##   1. Variable will be chosen
##########################################################################
modeln<-1
##################
models[[modeln]]<-list()
###
R2<-1
betaaddedvar<-1
addedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,i])  
    # 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)#adjusted R2
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##beta of added variable
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-1]))#beta of added variable (for modlen=1)
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)

##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel])

####this is the r2 of that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]) 

models[[1]]$summarybestmodel
models[[1]]$namesbestmodel
models[[1]]$R2bestmodel
models[[1]]$summarybestmodel$coefficients[(modeln+1),4]<0.1    #P-value
########################################################################################################################
##    -------2---------------
## add second variable (the only difference of the above part is that now modeln=2 the modelstatement has 2 variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-2
#######################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] )

####this is the r2 of that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]) 

models[[2]]$summarybestmodel
models[[2]]$namesbestmodel
models[[1]]$R2bestmodel
models[[2]]$R2bestmodel
models[[2]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

###################################################################################################################################
##    -------3---------------
## add third variable (the only difference of the above part is that now modeln=3 the modelstatement has 3 variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-3
###############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){  
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]) 


models[[3]]$summarybestmodel
models[[3]]$namesbestmodel
models[[2]]$R2bestmodel
models[[3]]$R2bestmodel
models[[3]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

##############################
##    -------4---------------
## add forth variable (the only difference of the above part is that now modeln=4 the modelstatement more variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-4
#########################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel])

####this is the r2 of that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]) 

models[[4]]$summarybestmodel
models[[4]]$namesbestmodel
models[[3]]$R2bestmodel
models[[4]]$R2bestmodel
models[[4]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

###################################################################################################################################
##    -------5---------------
## add fifth variable (the only difference of the above part is that now modeln=5 the modelstatement more variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-5
###########################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel], names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel])

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]) 


models[[5]]$summarybestmodel
models[[5]]$namesbestmodel
models[[4]]$R2bestmodel
models[[5]]$R2bestmodel
models[[5]]$summarybestmodel$coefficients[(modeln+1),4]<0.1
###################################################################################################################################
##    -------6---------------
## add sixth variable (the only difference of the above part is that now modeln=6 the modelstatement more variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-6
############################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel])

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]) 


models[[6]]$summarybestmodel
models[[6]]$namesbestmodel
models[[5]]$R2bestmodel
models[[6]]$R2bestmodel
models[[6]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # FALSE, stop for PM25_K

###################################################################################################################################
##    -------7---------------
## add sixth variable (the only difference of the above part is that now modeln=6 the modelstatement more variables)
###and the call for the list of variable names in the model changed slightly
###################################################################################################################################
modeln<-7
###################################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel])

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]) 


models[[7]]$summarybestmodel
models[[7]]$namesbestmodel
models[[6]]$R2bestmodel
models[[7]]$R2bestmodel
models[[7]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # stop for PM25_K

### 2020/10/01 stop for NO2_2010
### improvement smaller than 10%
(models[[6]]$R2bestmodel-models[[5]]$R2bestmodel)/models[[5]]$R2bestmodel*100
(models[[7]]$R2bestmodel-models[[6]]$R2bestmodel)/models[[6]]$R2bestmodel*100
models[[6]]$summarybestmodel
models[[6]]$namesbestmodel
# "ROADS_EU_20p" "no2_10MACC"   "ROADS_EU_1p"  "clc10_5p"     "MAJRDS_EU_1p"
# "Xcoord"      
# Adjusted R-squared:  0.599 
###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-8
###################################
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel])

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]) 

models[[8]]$summarybestmodel
models[[8]]$namesbestmodel
models[[7]]$R2bestmodel
models[[8]]$R2bestmodel
models[[8]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # stop for PM25_SI, PM25_V

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-9
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0


###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]) 


models[[9]]$summarybestmodel
models[[9]]$namesbestmodel
models[[8]]$R2bestmodel
models[[9]]$R2bestmodel
models[[9]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # stop for PM25_Si

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-10
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel], names(pred)[models[[10]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]) 

models[[10]]$summarybestmodel
models[[10]]$namesbestmodel
models[[9]]$R2bestmodel
models[[10]]$R2bestmodel
models[[10]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # stop for PM25_Zn

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-11
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    # at this point, coefficient [-11] exclude beta for wind variable
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel], names(pred)[models[[11]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]) 


models[[11]]$summarybestmodel
models[[11]]$namesbestmodel
models[[10]]$R2bestmodel
models[[11]]$R2bestmodel
models[[11]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_CU, PM25_NI

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-12
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel], names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]) 


models[[12]]$summarybestmodel
models[[12]]$namesbestmodel
models[[11]]$R2bestmodel
models[[12]]$R2bestmodel
models[[12]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_FE

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-13
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel], names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]) 

models[[13]]$summarybestmodel
models[[13]]$namesbestmodel
models[[12]]$R2bestmodel
models[[13]]$R2bestmodel
models[[13]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_S

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-14
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel], names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]) 

models[[14]]$summarybestmodel
models[[14]]$namesbestmodel
models[[13]]$R2bestmodel
models[[14]]$R2bestmodel
models[[14]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_Ni, PM25_ZN

###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-15
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel], names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]) 

models[[15]]$summarybestmodel
models[[15]]$namesbestmodel
models[[14]]$R2bestmodel
models[[15]]$R2bestmodel
models[[15]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_S

# at this point I always check wether the adjusted R2 of the next model is better than the one before, and if not I end here
# last step is to exclude all var with p>0.1 one by one with the lowes p first, until all <0.1.
###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-16
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel],
                                   names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel], names(pred)[models[[16]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]) 

models[[16]]$summarybestmodel
models[[16]]$namesbestmodel
models[[15]]$R2bestmodel
models[[16]]$R2bestmodel
models[[16]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # False - STOP HERE for PM25_Cu


# at this point I always check wether the adjusted R2 of the next model is better than the one before, and if not I end here
# last step is to exclude all var with p>0.1 one by one with the lowes p first, until all <0.1.
###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-17
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel],
                                   names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel], names(pred)[models[[16]]$indexbestmodel], names(pred)[models[[17]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]) 


models[[17]]$summarybestmodel
models[[17]]$namesbestmodel
models[[16]]$R2bestmodel
models[[17]]$R2bestmodel
models[[17]]$summarybestmodel$coefficients[(modeln+1),4]<0.1 # FALSE, stop for PM25_FE

# at this point I always check wether the adjusted R2 of the next model is better than the one before, and if not I end here
# last step is to exclude all var with p>0.1 one by one with the lowes p first, until all <0.1.
###################################################################################################################################
###if improvement just use the above to make a model with 18 covariates
###################################################################################################################################
modeln<-18
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel],
                                   names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel], names(pred)[models[[16]]$indexbestmodel], names(pred)[models[[17]]$indexbestmodel], names(pred)[models[[18]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+ pred[,models[[18]]$indexbestmodel]) 


models[[18]]$summarybestmodel
models[[18]]$namesbestmodel
models[[17]]$R2bestmodel
models[[18]]$R2bestmodel
models[[18]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

# at this point I always check wether the nextmodel is more than 1%better than the one before and if not I end here
# last step is to exclude all var with p>0.1 one by one with the lowes p first, until all <0.1.
###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-19
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+ pred[,models[[18]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel],
                                   names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel], names(pred)[models[[16]]$indexbestmodel], names(pred)[models[[17]]$indexbestmodel], names(pred)[models[[18]]$indexbestmodel], names(pred)[models[[19]]$indexbestmodel])

####this is the r2 or that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+ pred[,models[[18]]$indexbestmodel]+ pred[,models[[19]]$indexbestmodel]) 


models[[19]]$summarybestmodel
models[[19]]$namesbestmodel
models[[18]]$R2bestmodel
models[[19]]$R2bestmodel
models[[19]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

# at this point I always check wether the nextmodel is more than 1%better than the one before and if not I end here
# last step is to exclude all var with p>0.1 one by one with the lowes p first, until all <0.1.
###################################################################################################################################
###if improvement just use the above to make a model with 8 covariates
###################################################################################################################################
modeln<-20
##############
models[[modeln]]<-list()
###
R2<-1
addedvar<-1
betaaddedvar<-1
minbetas<-1
j<-1

for (i in nfirstvar:nlastvar){
  if (is.numeric(pred[,i])==TRUE){
    modeltry<-lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+pred[,models[[4]]$indexbestmodel]+pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+ pred[,models[[18]]$indexbestmodel]+ pred[,models[[19]]$indexbestmodel]+pred[,i]) 
    R2<-rbind(R2,summary(modeltry)$adj.r.squared)
    #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
    betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
    ####find out if there is a negative beta at pred. The intercept does not count [-1]!
    minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
    #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
    addedvar<-rbind(addedvar,names(pred)[i])
    j<-rbind(j, i)
    models[[modeln]][[i]]<-summary(modeltry)
  }
}

models[[modeln]]$results<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)

### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate
models[[modeln]]$resultspos<-subset(models[[modeln]]$results, minbetas>0) # | addedvar%in%c("v10","u10","t2m","tcc")

#rank the R2's of these models
models[[modeln]]$resultspos<-(data.frame(models[[modeln]]$resultspos, rank=rank(-models[[modeln]]$resultspos$R2, ties.method = "first")))

## this is the index of the row with the highest R2 and  no negative betas in this subset
models[[modeln]]$indexbestmodel<-subset(models[[modeln]]$resultspos, rank==1)$j

subset(models[[modeln]]$resultspos, rank==1)
##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
# subset(models[[modeln]]$resultspos, rank==1)$minbetas >0

###this is the model
models[[modeln]]$summarybestmodel<-models[[modeln]][[models[[modeln]]$indexbestmodel]]

###here are the names
models[[modeln]]$namesbestmodel<-c(names(pred)[models[[1]]$indexbestmodel], names(pred)[models[[2]]$indexbestmodel] , names(pred)[models[[3]]$indexbestmodel], names(pred)[models[[4]]$indexbestmodel], names(pred)[models[[5]]$indexbestmodel], names(pred)[models[[6]]$indexbestmodel], names(pred)[models[[7]]$indexbestmodel], names(pred)[models[[8]]$indexbestmodel], names(pred)[models[[9]]$indexbestmodel],names(pred)[models[[10]]$indexbestmodel],
                                   names(pred)[models[[11]]$indexbestmodel], names(pred)[models[[12]]$indexbestmodel], names(pred)[models[[13]]$indexbestmodel], names(pred)[models[[14]]$indexbestmodel], names(pred)[models[[15]]$indexbestmodel], names(pred)[models[[16]]$indexbestmodel], names(pred)[models[[17]]$indexbestmodel], names(pred)[models[[18]]$indexbestmodel], names(pred)[models[[19]]$indexbestmodel], names(pred)[models[[20]]$indexbestmodel])

####this is the r2 of that model
models[[modeln]]$R2bestmodel<-subset(models[[modeln]]$resultspos, rank==1)$R2

####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
models[[modeln]]$bestmodel <- lm(POLL~pred[,models[[1]]$indexbestmodel]+pred[,models[[2]]$indexbestmodel]+pred[,models[[3]]$indexbestmodel]+ pred[,models[[4]]$indexbestmodel]+ pred[,models[[5]]$indexbestmodel]+ pred[,models[[6]]$indexbestmodel]+ pred[,models[[7]]$indexbestmodel]+ pred[,models[[8]]$indexbestmodel]+ pred[,models[[9]]$indexbestmodel]+ pred[,models[[10]]$indexbestmodel]+ 
                                   pred[,models[[11]]$indexbestmodel]+ pred[,models[[12]]$indexbestmodel]+ pred[,models[[13]]$indexbestmodel]+ pred[,models[[14]]$indexbestmodel]+ pred[,models[[15]]$indexbestmodel]+ pred[,models[[16]]$indexbestmodel]+ pred[,models[[17]]$indexbestmodel]+ pred[,models[[18]]$indexbestmodel]+ pred[,models[[19]]$indexbestmodel]+ pred[,models[[20]]$indexbestmodel]) 

models[[20]]$summarybestmodel
models[[20]]$namesbestmodel
models[[19]]$R2bestmodel
models[[20]]$R2bestmodel
models[[20]]$summarybestmodel$coefficients[(modeln+1),4]<0.1

###################
# exclusion step  #
###################---------------------

temp <- 0
besti <- 0

for (i in 1:20){
  print(paste("model", i, "R2", models[[i]]$R2bestmodel))
  if(((models[[i]]$R2bestmodel-temp)/temp > 0.01)==T){   #R2 improvement > 1%
    temp <- models[[i]]$R2bestmodel
    besti <- i
  } else {
    n <- besti
  }}

n

###  n = the last step - 1
#     n=12

#########################     
# the adjusted R2 of the 15th model is not better than the 14th. 
##########################---------------------
Final <- cbind(POLL)
colnames(Final) <- c("Final")
for (j in 1:(n)){
  for (i in nfirstvar:nlastvar){
    if(names(pred)[i]==models[[n]]$namesbestmodel[j])
      Final <- cbind(Final, pred[i])
  }}
lastmodel <- lm(Final~., data=Final)

# exclude variables whose p values are >0.1 from the model, one by one with the highest p
for(j in (dim(Final)[2]-1):1){  
  if(max(summary(lastmodel)$coefficients[-1,4])>=0.1){
    a <- max(summary(lastmodel)$coefficients[-1,4])
    for (i in 1:j){
      if(summary(lastmodel)$coefficients[i+1,4]==a){
        Final <- Final[c(-(i+1))]}
    }
  }
  
  lastmodel <- lm(Final~., data=Final)
}

# install.packages("car")
# library(car)
############
##exclude vif>=3
############-------------------

# md <- lm(pm25_NI~tcc+indcount_80p+blh+PM25_EU_SIA+Nat_60p+v10+ROADS_EU_20p+Urbgr_50p+Port_15p+MACC_SS, data = All)
# md <- lm(pm25_S~ PM25_EU_annual+PM25_EU_DUST+u10+indcount_100p+MACC_BC+Tbu_40p+MACC_TCSO2+v10+ROADS_EU_3p, data = All)
# md <- lm(pm25_SI~ tcc+ROADS_EU_10p+Ind_60p+Hdr_2p+u10+MAJRDS_EUp, data = All)


# summary(md)
# vif(md)------------------------

for(j in (dim(Final)[2]-1):1){  
  if(max(vif(lastmodel))>=3){
    b <- max(vif(lastmodel))
  }
  for (i in 1:j){
    if(vif(lastmodel)[i]==b){
      Final <- Final[c(-(i+1))]}}
  lastmodel <- lm(Final~., data=Final)
}

######
###### vif(lastmodel)
###### 
##### Final <- Final[c(-5)]
##### lastmodel <- lm(Final~., data=Final)
##### summary(lastmodel)-------------------

finalBestModel <- lastmodel

###here are the names
finalBestModelNames <-names(Final[-1])
####this is the r2 of that model     
finalBestModelR2 <-summary(finalBestModel)$adj.r.squared

summary(finalBestModel)

increR2 <- c()
FinalR2 <- Final
for (i in (dim(Final)[2]-1):1){
  model <- lm(Final~., data=FinalR2)
  increR2 <- cbind(increR2, summary(model)$adj.r.squared)
  FinalR2 <- FinalR2[c(-(i+1))]
}

summarymodel <- names(Final)
for(i in 1:4){  
  summarymodel <- cbind(summarymodel, summary(finalBestModel)$coefficients[,i])
}
vif <- c("NA", vif(finalBestModel))
incresR2 <- c()
for(i in length(finalBestModelNames):1){
  incresR2 <- cbind(incresR2, increR2[i])
}  
incresR2 <- c("NA", incresR2)
summarymodel <- cbind(summarymodel,vif,incresR2)
colnames(summarymodel) <- c("variables", "beta", "Std.Error", "t", "P", "VIF", "increR2")
write.table (summarymodel, file= "data/SLR_summary_model.csv" ,sep="\t",dec = "," ,row.names=F)

## max cooks distance ##
max(cooks.distance(finalBestModel))
# summary(cooks.distance(finalBestModel))
# b <- cooks.distance(finalBestModel)
# which(cooks.distance(finalBestModel)==a)

#########################
# SAVE predicted values ---------------------
vall <- c()    # SELECT COLUMNS THAT ARE INCLUDED IN THE MODEL
coef <- c()
for (c in 1:(length(finalBestModelNames)+1)){     #LIST ALL COEFFICIENTS AND NAMES
  coef <- cbind(coef, finalBestModel$coef[c])
}
colnames(coef) <- c("Intercept", finalBestModelNames)

vall <- cbind(vall, rep(coef[1], each = dim(AllVal)[1]))  # intercept
vall2 <- vall   # SELECT COLUMNS THAT ARE INCLUDED IN THE MODEL and WEIGH BY COEFFICIENT

for (c in 2:(length(finalBestModelNames)+1)){
  variable <- colnames(coef)[c]    
  for (d in 1:length(AllVal)){
    if (colnames(AllVal[d]) == variable) 
      valplus <- d
  }
  vall <- cbind(vall, AllVal[valplus])            # COLUMNS IN MODEL
  vall2 <- cbind(vall2, AllVal[valplus]*coef[c])  # COLUMNS IN MODEL * COEFFICIENT
}

vall2$TOTAL <- rowSums(vall2) #CALCULATE TOTAL MODELED EXPOSURE PER SITE

colnames(vall) <- c("Intercept", finalBestModelNames)
colnames(vall2) <- c("SUMIntercept", paste0("SUM",finalBestModelNames, sep=""), "TOTAL")
# full model
# residuals <- POLL - vall2$TOTAL
# vall2 <- cbind(vall2, POLL, residuals)

# validation models
 residuals <- PollVal - vall2$TOTAL
 vall2 <- cbind(vall2, PollVal, residuals)

write.table (vall2, file= "U:/my data/component/results/20191016_slr/PM_mass_5.csv" ,sep="\t",dec = "," ,row.names=F)
#####################
### compute Moran'I
#####################
# library(ape)
#####################-----------------
All <- cbind(All,residuals)

poll.dists <- as.matrix(dist(cbind(All$Xcoord, All$Ycoord)))
# inverse distance matrix
poll.dists.inv <- 1/poll.dists
diag(poll.dists.inv) <- 0
# poll.dists.inv[1:5, 1:5]

Moran.I(All$residuals, poll.dists.inv)

# binary distance matrix
poll.dists.bin <- (poll.dists > 0 & poll.dists <= 300000)
Moran.I(All$residuals, poll.dists.bin)


