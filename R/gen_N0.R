#'Generate a matrix of N0 vectors to use in demographic trend simulations.
#'
#' Generate one or more abundance vectors from the stochastic stable
#' distribution of a collection of matrices.
#'
#'@param trans_mat an array of transition matrices 
#'@param n_return number of abundance vectors to be returned
#'@param n_converge number of time steps to simulate before recording abundance
#'
#'@return A matrix of population vectors. Even if \code{n_return = 1}, a
#'  1xnstage matrix is returned, for compatibilty with legacy code elsewhere (I
#'  hope this can be updated)
#'
#' @details Starts the population at the stable stage distribution of the mean matrix. Then the population is iterated for \code{n_converge} years, drawing a matrix at random each year. Finally, the iteration is continued for \code{n_return} years, again with a random draw each year.
#' 
#' The default value of \code{n_converge} is currently arbitrary, and may be too small to guarantee convergence to the stochastic stable distribution
#' 
#' If \code{n_return > 1}, the vqlues will \emph{not} be indepdendent, as they are subsequent values from a single stochastic simulation.
#' 
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu and Bruce Kendall

gen_N0<- function(trans_mat, n_return=1, n_converge=100){
  n_mat <- dim(trans_mat)[3] # Number of matrices
  
  # Start from the stable stage distribution of the mean matrix 
  mean_TransMat<- apply(trans_mat, 1:2, mean)
  require(popbio)
  N0 <- stable.stage(mean_TransMat)
  
  # Do the convergence iterations, without saving intermediate steps
  Ntemp <- N0
  index <- sample(n_mat, size=n_converge, replace=TRUE) 
  for (i in 1:n_converge) {
    Ntemp <- trans_mat[,,index[i]] %*% Ntemp
   # print(Ntemp)
  }
  
  Nt <- matrix(data=0,nrow=(n_return+1),ncol=(dim(trans_mat)[2]))
  
  #calculating Nt
  Nt[1,]<-Ntemp
  index <- sample(n_mat, size=n_return, replace=TRUE)
  for (i in 1:n_return){
    Nt[i+1,] <- trans_mat[,,index[i]] %*% Nt[i,]
  }
  Nt<-Nt[-1,, drop=FALSE]
  return(Nt)
}






