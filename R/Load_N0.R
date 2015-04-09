#'Load_N0
#'Loading N0 vectors from Ellis et al.
#'
#'@param g growth rate for adjusted stage
#'
#'@references Martha M. Ellis, Jennifer L. Williams, Peter Lesica, Timothy J. Bell, Paulette Bierzychudek, Marlin Bowles, 
#'  Elizabeth E. Crone, Daniel F. Doak, Johan Ehrlén, Albertine Ellis-Adam, Kathryn McEachern, Rengaian Ganesan, 
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

  ##subsetting and converting to usable R code
  ##calculating abundances
  convert_nx <- function(nx_str){
    nx_str<-substr(nx_str, 2, nchar(nx_str)-1)
    nx_str<-gsub(' ', ',', nx_str)
    nx_str<-paste('c(',nx_str,')',sep='')
    nx <- eval(parse(text=nx_str))
    nx<-matrix(nx, nrow=length(nx), byrow=T)
    nx<-as.vector(nx)
    return(nx) 
  }
  
  ##abundance data by year
  
  nx_out <- function (nx_str){
    nx <- matrix(NA, nrow = length(years), ncol=3)
    for (i in years) {
      nx[i,] <- matrix(convert_nx(nx_str[i]))
    }
    return(nx)
  }

