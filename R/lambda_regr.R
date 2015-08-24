#'lambda_regr
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified lambda matrices
#'
#'@param lambda is a 3d array of lambda matrices
#'@param years number of years in simulation
#'
#'@author Elizabeth Hiroyasu
#'



lambda_regr<- function(lambda, years){
  
  lm_lambda <- lm(lambda~years)
  
  return(lm_lambda)
}
