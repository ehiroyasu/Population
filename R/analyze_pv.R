#'analyze_pv
#'
#'Analyzes the output p-values from the calc_pv function. This function can be used to analyze multiple
#'species at a time.
#'
#'@param alpha is a vector of alpha values to compare p-values to and to assess power. Default alpha values are alpha=seq(from=0.01, to=0.2, by=delta) 
#'@param return_pv is list with p-values returned from regression analysis
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

analyze_pv<-function(alpha, return_pv){
  
  survival_pv<-unlist(return_pv[1,])
  abundance_pv<-unlist(return_pv[2,])
  lambda_pv<-unlist(return_pv[3,])
  
  prop_demog<-pv_lessthan_alpha(pvalue=survival_pv, alpha)
  prop_N<-pv_lessthan_alpha(pvalue=abundance_pv,alpha)
  prop_lambda<-pv_lessthan_alpha(pvalue=lambda_pv, alpha)

  prop_demog_0.05<-pv_lessthan_alpha(pvalue=survival_pv, alpha=0.05)
  prop_lambda_0.05<-pv_lessthan_alpha(pvalue=lambda_pv, alpha=0.05)
  
  return(list(survival_pv, abundance_pv, lambda_pv, prop_demog, prop_N, prop_lambda, prop_demog_0.05, prop_lambda_0.05))
