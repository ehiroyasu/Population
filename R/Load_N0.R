#'load_N0
#'Loading N0 vectors from Ellis et al.
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

load_N0 <-  function(){
  url <-"http://esapubs.org/Archive/ecol/E093/083/Transition_Matrices.txt"
  if ( !file.exists("Transition_Matrices.txt") )
    download.file(url, "Transition_Matrices.txt")
  N0_data <- read.table("Transition_Matrices.txt", as.is=c(F,F,F,T,T,T),
                     header=TRUE, sep="\t", check.names=FALSE,
                     na.strings="-999")
  return(N0_data)
}


