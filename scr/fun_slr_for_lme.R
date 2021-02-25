slr_lme <- function(POLL, pred, stations, years, cv_n=1){
   # POLL = train_sub$obs
   # pred = train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame()
   # stations = train_sub$station_european_code
   # years = train_sub$year
   # cv_n = csv_name
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
               # modeltry2j<-lmer(POLL~pred[,j] + (1|stations))
               # modeltry2j_y<-lmer(POLL~pred[,j] + (1|stations) + (1|years))
               # as.numeric(performance(modeltry2j)[4])
               
               # modeltry<-lm(POLL~pred[,i])  
               modeltry<-lmer(POLL~pred[,i] + (1|stations) + (1|years))
               as.numeric(performance(modeltry)[4])
               # 
               R2<-rbind(R2, as.numeric(performance(modeltry)[4])) #R2 (marginal: only fixed effect)
               #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
               betaaddedvar<-rbind(betaaddedvar, summary(modeltry)$coefficients[modeln+1,1])##beta of added variable
               ####find out if there is a negative beta at pred. The intercept does not count [-1]!
               minbetas<-rbind(minbetas, min(summary(modeltry)$coefficients[-1,1]))#beta of added variable (for modlen=1)
               #pred[,i] is the row that's being tried out now. the name of that row can be found in  names(pred)[i]
               addedvar<-rbind(addedvar,names(pred)[i])
               j<-rbind(j, i)
               models_candid[[i]]<-summary(modeltry)
            }
         }
         # 9: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  ... :
         #                    Model failed to converge with max|grad| = 0.00618618 (tol = 0.002, component 1)
         # 10: Some predictor variables are on very different scales: consider rescaling
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
         # models$bestmodel[[modeln]] <- lm(POLL~pred[,models$indexbestmodel[[1]]]) 
         models$bestmodel[[modeln]] <- lmer(POLL~pred[,models$indexbestmodel[[modeln]]] + (1|stations) + (1|years))
         
         models$summarybestmodel[[1]]
         models$namesbestmodel[[1]]
         models$R2bestmodel[[1]]
         # models$summarybestmodel[[1]]$coefficients[(modeln+1),4]<0.1    #P-value
         # Use only fixed effect to calculate R2
         summary(lm(POLL~pred[,models$indexbestmodel[[1]]]) )$coefficients[(modeln+1),4]<0.1    #P-value
         
         # 
         # summary(lmer(POLL~pred[,models$indexbestmodel[[modeln]]] + (1|stations) + (1|years)))$coefficients
         # lmer(POLL~pred[,models$indexbestmodel[[modeln]]] + (1|stations) + (1|years)) %>% anova
         # library(nlme)
         # m1_df <- data.frame(POLL=POLL, pred=pred[,models$indexbestmodel[[modeln]]],
         #                     stations = stations,
         #                     years= years)
         # m1 <- lme(POLL~pred, data = m1_df, random = ~1|stations+years)
         # m1
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
               pred_df = as.matrix(cbind(pred[,unlist(models$indexbestmodel[1:(modeln-1)])], pred[,i]))
               data_df = cbind(POLL, pred_df)
               
               # modeltry<-lm(eq_lm, data=as.data.frame(data_df)) 
               modeltry<-lmer(POLL~pred_df+ (1|stations) + (1|years))
               
               # 
               R2<-rbind(R2, as.numeric(performance(modeltry)[4])) #R2 (marginal: only fixed effect)
               if(nrow(summary(modeltry)$coefficients)==(modeln+1)){
                  #the new covariate is supposed to be in the last row, therefore I can select the row based on the model number +1
                  betaaddedvar<-rbind(betaaddedvar, summary(modeltry)$coefficients[modeln+1,1])##beta of added variable
                  
               }else{
                  #fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
                  betaaddedvar<-rbind(betaaddedvar, 0)##beta of added variable
                  
               }
               
               ####find out if there is a negative beta at pred. The intercept does not count [-1]!
               minbetas<-rbind(minbetas, min(summary(modeltry)$coefficients[-1,1]))#beta of added variable (for modlen=1)
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
         pred_df = cbind(pred[,unlist(models$indexbestmodel[1:modeln])]) %>% as.matrix()
         # data_df = cbind(POLL, pred_df)
         # eq_lm <- as.formula(paste0('POLL~.')) #+ (1|station_european_code) + (1|year)
         # models$bestmodel[[modeln]] <- lm(eq_lm, data=data_df) 
         models$bestmodel[[modeln]] <- lmer(POLL~pred_df + (1|stations) + (1|years))
         
         models$summarybestmodel[[modeln]]
         models$namesbestmodel[[modeln]]
         models$R2bestmodel[[modeln-1]]
         models$R2bestmodel[[modeln]]
         # models$summarybestmodel[[modeln]]$coefficients[(modeln+1),4]<0.1 #p-value is not available, only t-value and std. error
         data_df = cbind(POLL, pred_df)
         eq_lm <- as.formula(paste0('POLL~.'))
         summary(lm(eq_lm, data=as.data.frame(data_df)))$coefficients[(modeln+1),4]<0.1 #p-value is not available, only t-value and std. error
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
   pred_df = as.matrix(pred[,unlist(models$indexbestmodel[1:n])])
   lastmodel_lme <- lmer(POLL~pred_df+ (1|stations) + (1|years))
   # lastmodel_slr <- lm(POLL~pred_df)
   lastmodel_slr <- lm(Final~., Final)
   
   # exclude variables whose p values are >0.1 from the model, one by one with the highest p
   for(j in (dim(Final)[2]-1):1){
      if(max(summary(lastmodel_slr)$coefficients[-1,4])>=0.1){
         a <- max(summary(lastmodel_slr)$coefficients[-1,4])
         for (i in 1:j){
            if(summary(lastmodel_slr)$coefficients[i+1,4]==a){
               Final <- Final[c(-(i+1))]}
         }
      }
      
      lastmodel_slr <- lm(Final~., data=Final)
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
      if(max(vif(lastmodel_slr))>=3){
         b <- max(vif(lastmodel_slr))
         for (i in 1:j){
            if(vif(lastmodel_slr)[i]==b){
               Final <- Final[c(-(i+1))]}}  # debug: because not all models will have predictors with VIF larger than 3
      }
      
      lastmodel_slr <- lm(Final~., data=Final)
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
   
   finalBestModel <- lastmodel_slr
   
   ###here are the names
   finalBestModelNames <-names(Final[-1])
   ####this is the r2 of that model     
   finalBestModelR2 <-summary(finalBestModel)$adj.r.squared
   
   # pred_df = as.matrix(pred[,finalBestModelNames])
   # lastmodel_lme <- lmer(POLL~pred_df+ (1|stations) + (1|years))
   # performance(lastmodel_lme)[4] %>% as.numeric()
   # performance(finalBestModel)[4] %>% as.numeric()
   
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
             file= paste0("data/workingData/LME_summary_model_", cv_n, ".csv") ,
             row.names=F)
   return(list(models,
               n,
               finalBestModel))
   
}
