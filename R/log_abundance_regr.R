#'log_abundance_regr
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified log abundance matrices
#'
#'@param abundance array of abundance matrices calculated from transition matrices
#'with modified survival
#'@param years the number of years in the simulation
#'@param active_stages is the stages of the species that are active (as opposed to dormant or propagule)
#'
#'@author Elizabeth Hiroyasu
#'

log_abundance_regr<- function(abundance, years, active_stages){
  active_abundance<- abundance[,active_stages]
  tot_abundance <- apply(active_abundance, 1, sum)
  log_abundance<-log(tot_abundance)
  lm_log_abundance <- summary(lm(log_abundance ~ poly(years, 2)))
  
  if (lm_log_abundance$coefficients[3,4]=='NaN'){
    warning('p-value=NaN')
  }
  
  return(lm_log_abundance)
}