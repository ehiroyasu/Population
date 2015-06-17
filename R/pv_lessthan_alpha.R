#'pv_lessthan_alpha
#'
#'counting the number of times p-value for abundance is less than alpha, this will 
#'allow us to assess power.
#'
#'@param pvalue is a vector of p-values 
#'@param alpha is a vector of alpha values over which to test if abundance is less than
#'
#'@return prop_N a vector of proportional counts of the number of times the abundance p-value is less than the alpha value
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

pv_lessthan_alpha<- function(pvalue, alpha){
  
  prop<-vector(length=length(alpha))
  
  for (a in 1:length(alpha)){
    prop[a]<-(sum(pvalue<alpha[a]))/length(pvalue)
  }
  return(prop)
}

