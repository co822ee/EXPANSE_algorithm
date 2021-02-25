slr <- function(POLL, pred, cv_n=1){
   nfirstvar <- 1  # given that the list of explanatory variables is at the end of the table: specify in which row the first explanatory variable is
   nlastvar <- dim(pred)[2] #here the number of the last row is calculated
   
   
   
   
   ##########################################################################
   models<-list()
   
   ##########################################################################
   ##    -------1---------------
   ##   1. Variable will be chosen
   ##########################################################################
   modeln<-1
   models <- list()
   ##################
   while(modeln<=20){
      if(modeln==1){
         models_candid <-list()
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
               models_candid[[i]]<-summary(modeltry)
            }
         }
         
         models$results[[modeln]]<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)
         
         ### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
         models$resultspos[[modeln]]<-subset(models$results[[modeln]], minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")
         
         #rank the R2's of these models
         models$resultspos[[modeln]]<-(data.frame(models$resultspos[[modeln]], rank=rank(-models$resultspos[[modeln]]$R2, ties.method = "first")))
         
         ## this is the index of the row with the highest R2 in this subset
         models$indexbestmodel[[modeln]]<-subset(models$resultspos[[modeln]], rank==1)$j
         
         subset(models$resultspos[[modeln]], rank==1)
         
         ##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
         # subset(models$resultspos[[modeln]], rank==1)$minbetas >0
         
         ###this is the model
         models$summarybestmodel[[modeln]]<-models_candid[[models$indexbestmodel[[modeln]]]]
         
         ###here are the names
         models$namesbestmodel[[modeln]]<-c(names(pred)[models$indexbestmodel[[modeln]]])
         
         ####this is the r2 of that model
         models$R2bestmodel[[modeln]]<-subset(models$resultspos[[modeln]], rank==1)$R2
         
         ####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
         models$bestmodel[[modeln]] <- lm(POLL~pred[,models$indexbestmodel[[1]]]) 
         
         models$summarybestmodel[[1]]
         models$namesbestmodel[[1]]
         models$R2bestmodel[[1]]
         models$summarybestmodel[[1]]$coefficients[(modeln+1),4]<0.1    #P-value
         modeln <- modeln+1
      }else{
         #######################
         models_candid <-list()
         ###
         R2<-1
         addedvar<-1
         betaaddedvar<-1
         minbetas<-1
         j<-1
         
         for (i in nfirstvar:nlastvar){
            if (is.numeric(pred[,i])==TRUE){
               pred_df = cbind(pred[,unlist(models$indexbestmodel[1:(modeln-1)])], pred[,i])
               data_df = cbind(POLL, pred_df)
               eq_lm <- as.formula(paste0('POLL~.')) #+ (1|station_european_code) + (1|year)
               modeltry<-lm(eq_lm, data=as.data.frame(data_df)) 
               R2<-rbind(R2,summary(modeltry)$adj.r.squared)
               #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
               betaaddedvar<-rbind(betaaddedvar, modeltry$coefficients[(modeln+1)])##
               ####find out if there is a negative beta at pred. The intercept does not count [-1]!
               minbetas<-rbind(minbetas, min(modeltry$coefficients[-c(1)]))
               #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
               addedvar<-rbind(addedvar,names(pred)[i])
               j<-rbind(j, i)
               models_candid[[i]]<-summary(modeltry)
            }
         }
         
         models$results[[modeln]]<-data.frame(addedvar=addedvar[-1],R2=R2[-1]*1, j=j[-1], betaaddedvar=betaaddedvar[-1]*1,minbetas=minbetas[-1]*1)
         
         ### this are the candidates models with the highest R2, not a negative beta for the new nor for any other covariate, except "v10","u10","t2m","tcc"
         models$resultspos[[modeln]]<-subset(models$results[[modeln]], minbetas>0 ) # | addedvar%in%c("v10","u10","t2m","tcc")
         
         #rank the R2's of these models
         models$resultspos[[modeln]]<-(data.frame(models$resultspos[[modeln]], rank=rank(-models$resultspos[[modeln]]$R2, ties.method = "first")))
         
         ## this is the index of the row with the highest R2 and  no negative betas in this subset
         models$indexbestmodel[[modeln]]<-subset(models$resultspos[[modeln]], rank==1)$j
         
         subset(models$resultspos[[modeln]], rank==1)
         ##### manually check if the minbeta is positive, if minbeta<0, check the betas for covariables
         # subset(models$resultspos[[modeln]], rank==1)$minbetas >0
         
         ###this is the model
         models$summarybestmodel[[modeln]]<-models_candid[[models$indexbestmodel[[modeln]]]]
         
         ###here are the names
         models$namesbestmodel[[modeln]]<-names(pred)[unlist(models$indexbestmodel[1:(modeln)])]
         
         ####this is the r2 of that model
         models$R2bestmodel[[modeln]]<-subset(models$resultspos[[modeln]], rank==1)$R2
         
         ####this is the model object of the best model (can be used in Cook's D in car.infIndexPlot())
         pred_df = cbind(pred[,unlist(models$indexbestmodel[1:modeln])])
         data_df = cbind(POLL, pred_df)
         eq_lm <- as.formula(paste0('POLL~.')) #+ (1|station_european_code) + (1|year)
         models$bestmodel[[modeln]] <- lm(eq_lm, data=data_df) 
         
         models$summarybestmodel[[modeln]]
         models$namesbestmodel[[modeln]]
         models$R2bestmodel[[modeln-1]]
         models$R2bestmodel[[modeln]]
         models$summarybestmodel[[modeln]]$coefficients[(modeln+1),4]<0.1
         modeln <- modeln+1
         
      }
   }
   
   ###################
   # exclusion step  #
   ###################---------------------
   temp <- 0
   besti <- 0
   
   for (i in 1:20){
      print(paste("model", i, "R2", models$R2bestmodel[[i]]))
      if(((models$R2bestmodel[[i]]-temp)/temp)>= 0.01){   #R2 improvement > 1%
         temp <- models$R2bestmodel[[i]]
         besti <- i
      } else {
         n <- besti
         break
      }
   }
   
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
         if(names(pred)[i]==models$namesbestmodel[[n]][j])
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
         for (i in 1:j){
            if(vif(lastmodel)[i]==b){
               Final <- Final[c(-(i+1))]}}  # debug: because not all models will have predictors with VIF larger than 3
      }
      
      lastmodel <- lm(Final~., data=Final)
   }
   # Try to add x and y at the last step (some variable becomes insignificant)
   # lastmodel <- lm(Final~., data=cbind(Final, 
   #                                     x_trun=train_sub$x_trun, 
   #                                     y_trun=train_sub$y_trun))
   # 
   # lastmodel %>% summary
   # lastmodel %>% vif
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
   write.csv(summarymodel, 
             file= paste0("data/workingData/SLR_summary_model_", cv_n, ".csv") ,
             row.names=F)
   return(list(models,
               n,
               finalBestModel))
   
}
