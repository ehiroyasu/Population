#'lambda_lessthan_alpha
#'
#'counting the number of times the p-value for lambda is less than alpha, this will 
#'allow us to assess power.
#'
#'@param lambda_pv is a vector of p-values for the regression of lambda over time
#'@param alpha is a vector of alpha values over which to test if lambda is less than
#'
#'@return prop_N a vector of proportional counts of the number of times the abundance p-value is less than the alpha value
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

lambda_lessthan_alpha<- function(lambda_pv, alpha){
  
  prop_lambda<-vector(length=length(alpha))
  
  for (a in 1:length(alpha)){
    prop_lambda[a]<-(sum(lambda_pv<alpha[a]))/length(alpha)
  }
  return(prop_lambda)
}
