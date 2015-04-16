#'gen_N0
#'
#'Generating a matrix of N0 vectors to use in demographic trend simulations.
#'This function creates a matrix that is the average of given transition matrices from the COMPADRE plant matrix database. 
#'It then #'calculates the population over a ten year period using this average transition matrix and an N0 vector of 
#'ones.
#'
#'@param N0 this is a vector of ones, and its dimensions are calculated from the dimensions of the transition matrix
#'@param trans_mat an array of transition matrices from the COMPADRE plant matrix database.
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu

gen_N0<- function(N0, trans_mat){
  #creating new N0 vectors from transition matrices
  N0 <- rep(1, dim(trans_mat)[1])
  #taking the average of the transition matrices
  mean_TransMat<- apply(trans_mat, 1:2, mean)
  
  #calculating Nt
  Nt <- matrix(0,11,5)
  Nt[1,]<-N0
  for (i in 1:10){
    Nt[i+1,] <- mean_TransMat %*% Nt[i,]
  } 
  return(Nt)
}






