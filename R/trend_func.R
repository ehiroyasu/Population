#'Adding a trend to survival rates in different stages
#'
#'This function adds a trend to stage 2 survival
#'in the survival matrices for a stage or age structured population.
#'
#'@param surv_rand A randomly sampled array of survival matrices
#'@param beta dictates how large the trend is to be introduced to survival matrices
#'@param nstage is the number of stages represented in the matrix data
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu


insert_survival_trend<- function(surv_rand, beta, nstage, stage2mod){
  
  surv_trend <- surv_rand
  
    for (i in years){
      for (j in nstage){
        surv_trend[j,stage2mod,i]<- surv_rand[j,stage2mod,i]-beta*i
        if (surv_trend[j,stage2mod,i]<0){surv_trend[j,stage2mod,i]=0}
      }
    }
  return(surv_trend)
}

