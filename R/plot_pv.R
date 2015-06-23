#'plot_pv
#'
#'Plots different output values, and creates histograms of pvalues. This function can also be used to loop through a list of different outputs
#'
#'@param alpha is a vector of alpha values to compare p-values to and to assess power. Default alpha values are alpha=seq(from=0.01, to=0.2, by=delta) 
#'@param output is a list of different values, including survival p-values, abundance p-values, and lambda p-values
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

plot_pv<-function(output, alpha){
  plot(output$prop_demog~alpha, type='l',col='blue', xlab='alpha', ylab='freq p<alpha', main=output$names, xlim=c(0,0.2), ylim=c(0,1))
  lines(output$prop_lambda~alpha, col='red')
  legend("topleft", c("Survival p-values", "Lambda p-values"), col=c("blue","red"), lty=c(1,1))
  
  plot(output$survival_pv~output$lambda_pv, main='Survival p-value vs. Lambda p-value', xlab='lambda p-values', ylab='survival p-values')
  abline(0,1)
  
  hist(output$lambda_pv-output$survival_pv, main='Difference of lambda_pv minus survival_pv at beta=0.01')
  hist(output$lambda_pv, main='Histogram of lambda p-values')
  hist(output$survival_pv, main='Histogram of survival p-values')
}


