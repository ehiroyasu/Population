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
  lm_lambda <- summary(lm(lambda~years))
  
  return(lm_lambda)
}
