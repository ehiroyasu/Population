#'convert_N0
#'Converting N0 data from transition matrices to usable R code.
#'
#'@param nx_str is the subsetted N0 strings from the Ellis et al. data.
#'
#'@references Martha M. Ellis, Jennifer L. Williams, Peter Lesica, Timothy J. Bell, Paulette Bierzychudek, Marlin Bowles, 
#'  Elizabeth E. Crone, Daniel F. Doak, Johan Ehrl?n, Albertine Ellis-Adam, Kathryn McEachern, Rengaian Ganesan, 
#'  Penelope Latham, Sheila Luijten, Thomas N. Kaye, Tiffany M. Knight, Eric S. Menges, William F. Morris, 
#'  Hans den Nijs, Gerard Oostermeijer, Pedro F. Quintana-Ascencio, J. Stephen Shelly, Amanda Stanley, Andrea Thorpe, 
#'  Tamara Ticktin, Teresa Valverde, and Carl Weekley. 2012. Matrix population models from 20 studies of perennial 
#'  plant populations. Ecology 93:951.
#'
#'@references code is adapted from Ellis et al. (2012) metadata
#'
#'@author Elizabeth Hiroyasu

convert_nx <- function(nx_str){
  nx_str<-substr(nx_str, 2, nchar(nx_str)-1)
  nx_str<-gsub(' ', ',', nx_str)
  nx_str<-paste('c(',nx_str,')',sep='')
  nx <- eval(parse(text=nx_str))
  nx<-matrix(nx, nrow=length(nx), byrow=T)
  nx<-as.vector(nx)
  return(nx) 
}
