#'surv_regression
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified survival matrices
#'
#'@param surv_trend array of survival matrices with inserted trend beta
#'
#'@author Elizabeth Hiroyasu
#'

surv_regr<- function(surv_trend, active_stages){
  survival<- apply(surv_trend[,active_stages,], c(2,3), sum)
  test_zeros <- apply(survival, 1, sum)
  survival <- survival[test_zeros != 0,]
  #print(survival)
  
  library(reshape2)
  survival<-melt(survival, varnames=c("stage", "year"))

  lm_surv<- summary(lm(survival$value~survival$year+survival$stage))
  
  return(lm_surv)
}
