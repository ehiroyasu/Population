#'autocorrelation_test
#'
#'Examines autocorrelation in the regression models from the DemographicTrend package
#'
#'@param mod is the linear model, for example, this can be the lm_surv, lm_lambda, or lm_abundance
#'
#'@author Elizabeth Hiroyasu
#'
#'
#'




autocorrelation_test<-function(mod){

  res<-mod$residuals
  autocorrelation_plot<-acf(res)
  n=length(res)
  lm_res<- lm(res[-n]~res[-1])
  res_plot<-plot(res[-n]~res[-1])
  
  return(list(res, autocorrelation_plot, lm_res, res_plot))
  
}