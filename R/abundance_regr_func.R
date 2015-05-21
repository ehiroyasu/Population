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
  tot_abundance <- apply(abundance, 1, sum)
#  lm_abundance<- summary(lm(stage_abundance~poly(years,stage2mod)))
  lm_abundance <- summary(lm(tot_abundance ~ poly(years, 2)))
  return(lm_abundance)
}
