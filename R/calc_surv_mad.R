#'calc_surv_mad
#'
#'Calculating the absolute deviation centered around the mean in survival in the stage of interest (stage2mod)
#'
#'
#'@param surv_rand an array of randomized survival matrices
#'@param stage2mod the index of the stage for which survival is to be modified
#'@param years number of years in the simulation
#'
#'@return a vector of variances for each survival matrix
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu


calc_surv_mad<-function(surv_rand, stage2mod, years){
 
  survival<-matrix(nrow=length(stage2mod), ncol=length(years)) 
    
  for (i in stage2mod){
    survival[i,]<-apply(surv_rand[,stage2mod[i],], 2, sum)
    #calculating the absolute deviation centered around the mean   
    surv_mad[i]<- mad(survival[i,], center=mean(survival[i,]))
  }
  return(surv_mad)
}
