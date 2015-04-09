#'surv_regression
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified survival matrices
#'
#'@param surv_trend array of survival matrices with inserted trend beta
#'@param years the number of years in the simulation
#'
#'@author Elizabeth Hiroyasu
#'

surv_regr<- function(surv_trend, years, stage2mod){
  stage_surv<- apply(surv_trend,c(2,3), sum)
  lm_surv<- summary(lm(stage_surv[stage2mod,]~years))
  return(lm_surv)
}
