#'abundance_regression
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified abundance matrices
#'
#'@param abundance array of abundance matrices calculated from transition matrices
#'with modified survival
#'@param years the number of years in the simulation
#'
#'@author Elizabeth Hiroyasu
#'

abundance_regr<- function(abundance, years, stage2mod){
  stage_abundance<- abundance[,stage2mod]
  lm_abundance<- summary(lm(stage_abundance~poly(years,stage2mod)))
  return(lm_abundance)
}
