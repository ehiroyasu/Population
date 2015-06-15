#'surv_lessthan_alpha
#'
#'counting the number of times the p-value for survival and the p-value for abundance are les than alpha, this will 
#'allow us to assess power.
#'
#'
#'@param survival_pv is a vector of p-values for the regression of survival over time
#'@param alpha is a vector of alpha values over which to test if survival is less than
#'
#'@return prop_demog a vector of proportional counts of the number of times the survival p-value is less than the alpha value
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

surv_lessthan_alpha<- function(survival_pv, alpha){

  prop_demog<-vector(length=length(alpha))
   
  for (a in 1:length(alpha)){
      prop_demog[a]<-(sum(survival_pv<alpha[a]))/length(alpha)
    }
  return(prop_demog)
  }
