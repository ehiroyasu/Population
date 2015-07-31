#'surv_regression
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified survival matrices
#'
#'@param surv_trend array of survival matrices with inserted trend beta
#'
#'@author Elizabeth Hiroyasu
#'

surv_regr<- function(surv_trend, active_stages, abundance){
  survival<- apply(surv_trend[,active_stages,], c(2,3), sum)
  test_zeros <- apply(survival, 1, sum)
  survival <- survival[test_zeros != 0,]
  #print(survival)
  
  for (i in 1:length(test_zeros)){
    if(test_zeros[i]==0){
      abundance<-abundance[,-i]
    }
  }
  
  library(reshape2)
  survival<-melt(survival, varnames=c("stage", "year"))
#  abundance<-melt(abundance[-1,], varnames=c("year", "stage"))
  abundance<-melt(abundance[-1,active_stages], varnames=c("year", "stage"))
  merge<-merge(survival, abundance, by=c("stage", "year"))
  colnames(merge)<-c("stage", "year", "survival", "abundance")
  lm_surv<- summary(lm(merge$survival~merge$year+merge$stage, weights=merge$abundance))
  
  return(lm_surv)
}
