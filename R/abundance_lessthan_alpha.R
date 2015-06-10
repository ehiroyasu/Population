#'abundance_lessthan_alpha
#'
#'counting the number of times the p-value for survival and the p-value for abundance are les than alpha, this will 
#'allow us to assess power.
#'
#'@param abundance_pv is a vector of p-values for the regression of survival over time
#'@param alpha is a vector of alpha values over which to test if survival is less than
#'
#'@return demog a vector of counts of the number of times the survival p-value is less than the alpha value
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

abundance_lessthan_alpha<- function(abundance_pv, alpha){
  
  N_pv<-vector(length=length(alpha))
  
  for (a in 1:length(alpha)){
    N_pv[a]<-sum(abundance_pv<alpha[a])
  }
  return(N_pv)
}

