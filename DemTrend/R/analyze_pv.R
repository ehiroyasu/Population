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
  log_abundance_pv<-unlist(return_pv[3,])
  lambda_pv<-unlist(return_pv[4,])
  
  prop_demog<-pv_lessthan_alpha(pvalue=survival_pv, alpha)
  prop_N<-pv_lessthan_alpha(pvalue=abundance_pv,alpha)
  prop_log_N<-pv_lessthan_alpha(pvalue=log_abundance_pv, alpha)
  prop_lambda<-pv_lessthan_alpha(pvalue=lambda_pv, alpha)
  
  surv_zero<-unlist(return_pv[5,])
  surv_na<-unlist(return_pv[6,])
  surv_var<-unlist(return_pv[7,])
  abundance_var<-unlist(return_pv[8,])
  lambda_var<-unlist(return_pv[9,])
  mean_lambda<-unlist(return_pv[10,])
  
  acf1_abundance<-unlist(return_pv[11,])
  acf1_lambda<-unlist(return_pv[12,])

  
  return(list('survival_pv'=survival_pv, 'abundance_pv'=abundance_pv, 'log_abundance_pv'=log_abundance_pv,
              'lambda_pv'=lambda_pv, 'prop_demog'=prop_demog, 'prop_N'=prop_N, 'prop_log_N'=prop_log_N,
              'prop_lambda'=prop_lambda, 'surv_zero'=surv_zero, 'surv_na'=surv_na, 'surv_var'=surv_var, 'abundance_var'=abundance_var, 
              'lambda_var'=lambda_var, 'mean_lambda'=mean_lambda, 'acf1_abundance'=acf1_abundance, 'acf1_lambda'=acf1_lambda))
}
