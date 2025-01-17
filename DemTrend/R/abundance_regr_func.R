#'abundance_regression
#'
#'Runs a regression analysis examining the power of detecting a trend in
#'modified abundance matrices
#'
#'@param abundance array of abundance matrices calculated from transition matrices
#'with modified survival
#'@param years the number of years in the simulation
#'@param active_stages is the stages of the species that are active (as opposed to dormant or propagule)
#'
#'@author Elizabeth Hiroyasu
#'

abundance_regr<- function(abundance, years, active_stages){
  active_abundance<- abundance[,active_stages]
  tot_abundance <- apply(active_abundance, 1, sum)

  lm_abundance <- lm(tot_abundance ~ poly(years, 2))

#   if (lm_abundance$coefficients[3,4]=='NaN'){
#     warning('p-value=NaN')
#   }

  return(lm_abundance)
}
