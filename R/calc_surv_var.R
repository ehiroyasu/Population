#'Caculating the initial variance in survival in the stage of interest (stage2mod)
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


calc_surv_var<-function(surv_rand, stage2mod, years){
  for (i in years){
    surv_var[i]<-var(surv_rand[,2,i])
  }
  if(is.vector(stage2mod)){
   for(j in years){
     for(k in stage2mod){
       surv_var[k,j]<-var(surv_rand[,k,j])
     }
   }
     
  }
  return(surv_var)
}
