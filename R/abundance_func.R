#'Calculating abundances with adjusted survivals
#'This function uses matrix multiplication to calculate abundances from
#'transition matrices and N0 data
#'
#'@param surv_trend calculated from trend function
#'@param N0_rand a random matrix of 10 years of initial population data
#'@param fert_mat fertility matrix from COMPADRE database
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'@references Martha M. Ellis, Jennifer L. Williams, Peter Lesica, Timothy J. Bell, Paulette Bierzychudek, Marlin Bowles, 
#'  Elizabeth E. Crone, Daniel F. Doak, Johan Ehrlen, Albertine Ellis-Adam, Kathryn McEachern, Rengaian Ganesan, 
#'  Penelope Latham, Sheila Luijten, Thomas N. Kaye, Tiffany M. Knight, Eric S. Menges, William F. Morris, 
#'  Hans den Nijs, Gerard Oostermeijer, Pedro F. Quintana-Ascencio, J. Stephen Shelly, Amanda Stanley, Andrea Thorpe, 
#'  Tamara Ticktin, Teresa Valverde, and Carl Weekley. 2012. Matrix population models from 20 studies of perennial 
#'  plant populations. Ecology 93:951.
#'
#'@author Elizabeth Hiroyasu


abundance_func<- function(N0_rand, surv_trend, fert_mat){
  trans_mat<- surv_trend + fert_mat
  for (i in years){
    abundance<-N0_rand
    abundance[i,]<-trans_mat[,,i]%*%N0_rand[i,]
  }
  return(abundance)
}
