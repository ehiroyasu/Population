#'test_fert
#'
#'Tests for fertility matrices of zero
#'
#'@param fert_mat
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu
#'

test_fert<-function(fert_mat){
  for (i in 1:dim(fert_mat)[3]){
    test[i]<-which(fert_mat[,,i]==matrix(0, nrow=dim(fert_mat)[1], ncol=dim(fert_mat)[2]))
    #test_zero<-which(test=="TRUE")
    
  }
}







#apply(fert_mat, 3, which)
