#'Adding a trend to survival rates in different stages
#'
#'This function adds a trend to stage 2 survival
#'in the survival matrices for a stage or age structured population.
#'
#'@param surv_mat an array of survival matrices
#'@param beta dictates how large the trend is to be introduced to survival matrices. A value of -beta will be added to survival each year
#'@param nstage is the number of stages represented in the matrix data
#'@param stage2mod the index of the stage for which survival is to be modified
#'@param years number of years in the simulation
#'
#'@return An array of matrices with a trend in survival
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu


insert_survival_trend<- function(surv_mat, beta, nstage, stage2mod, years){
  
  surv_trend <- surv_mat
  for (i in years){
    survival <- sum(surv_trend[,stage2mod,i])
    surv_trend[,stage2mod,i] <- surv_trend[,stage2mod,i] * (survival-beta*i)/survival
  }
  surv_trend[is.na(surv_trend)]<-0
  surv_trend[surv_trend<0]<-0
  return(surv_trend)
}

