#'Calculating abundances with adjusted survivals
#'This function uses matrix multiplication to calculate abundances from
#'transition matrices and N0 data
#'
#'@param surv_trend calculated from trend function
#'@param N0_rand a random matrix of 10 years of initial population data
#'@param fert_mat fertility matrix from COMPADRE database
#'@param trans_mat is an array of transition matrices from the COMPADRE database
#'@param years is the number of years in simulation
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu


calc_abundance<- function(N0_data, surv_trend, fert_rand, clon_rand, trans_mat, years){
  trans_mat_trend<- surv_trend + fert_rand + clon_rand
  abundance <- matrix(nrow=length(years)+1, ncol=(dim(surv_trend)[1]))
  abundance[1,] <- gen_N0(trans_mat)
  for (i in years){
   
    abundance[i+1,]<-trans_mat_trend[,,i]%*%abundance[i,]
  }
  return(abundance)
}
