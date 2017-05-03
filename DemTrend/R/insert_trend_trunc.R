#'Adding a trend to survival rates in different stages
#'
#'This function adds a trend to stage 2 survival
#'in the survival matrices for a stage or age structured population.
#'
#'@param surv_mat an array of randomized survival matrices
#'@param beta dictates how large the trend is to be introduced to survival matrices. A value of -beta will be added to survival each year
#'@param nstage is the number of stages represented in the matrix data
#'@param stage2mod the index of the stage for which survival is to be modified
#'@param years number of years in the simulation
#'
#'@return surv_trend An array of matrices with a trend in survival 
#'@return surv_zero a count of the number of times survival goes to zero after a trend is inserted
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu


insert_trend_trunc<- function(surv_rand, beta, nstage, stage2mod, years, verbose=FALSE){
  
  surv_trend <- surv_rand
  survival <- apply(surv_trend, 3, sum)
  #if (verbose) print(surv_trend)
  #if stage2mod is a vector, and we are adding a trend to survival in multiple stages
  if(is.vector(stage2mod)){
  
    for (i in years){
      for (j in stage2mod){
          surv_trend[,j,i] <- surv_trend[,j,i] * (survival[i]-beta*i)/survival[i]
          surv_min<- apply(surv_trend, 3, min)
          if (surv_min[i]<0){
            surv_nonzero<- (surv_trend[,j,i]!=0)*surv_min[i]
              surv_trend[,j,i]<-surv_trend[,j,i]-surv_nonzero
            }
           }
      }
    
      } else {
      for (i in years){
      survival <- sum(surv_trend[,stage2mod,i])
      surv_trend[,stage2mod,i] <- surv_trend[,stage2mod,i] * (survival-beta*i)/survival
      surv_min<- min(surv_trend[,,i])
      if (surv_min[i]<0){
        surv_nonzero<- (surv_trend[,stage2mod,i]!=0)*surv_min[i]
        surv_trend[,j,i]<-surv_trend[,stage2mod,i]-surv_nonzero
      }
    }
  }
  
  return(surv_trend)
}
