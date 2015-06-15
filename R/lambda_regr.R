#'lambda_regr
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified lambda matrices
#'
#'@param lambda is a 3d array of lambda matrices
#'
#'@author Elizabeth Hiroyasu
#'



lambda_regr<- function(lambda){
  lambda_total<- apply(lambda,c(2,3), sum)
  library(reshape2)
  lambda_df<-melt(lambda_total, varnames=c("stage", "year"))

  lm_lambda <- summary(lm(lambda_df$value~lambda_df$year+lambda_df$stage))
  
  return(lm_lambda)
}
