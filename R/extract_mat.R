#'extract_mat
#'
#'Extracts survival matrices from the COMPADRE matrix data.
#'
#'@param MatrixData matrix population data from the COMPADRE matrix data
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu
#'

extract_mat<- function(MatrixData){
  #extracting survival matrices
  surv_mat<-array(1, dim=c(dim(MatrixData[[1]]$matU)[1], dim(MatrixData[[1]]$matU)[2], dim(MatrixData)))
  for (i in 1:dim(MatrixData)){
    surv_mat[,,i]<-MatrixData[[i]]$matU
  }
  ##extracting fertility matrices
  fert_mat<-array(1, dim=c(dim(MatrixData[[1]]$matF)[1], dim(MatrixData[[1]]$matF)[2], dim(MatrixData)))
  for (i in 1:dim(MatrixData)){
    fert_mat[,,i]<-MatrixData[[i]]$matF
  }
  
  ##extracting clonal matrices
  clon_mat<-array(1, dim=c(dim(MatrixData[[1]]$matC)[1], dim(MatrixData[[1]]$matC)[2], dim(MatrixData)))
  for (i in 1:dim(MatrixData)){
    clon_mat[,,i]<-MatrixData[[i]]$matC
  }
  
  ##extracting transition matrices
  trans_mat<-array(1, dim=c(dim(MatrixData[[1]]$matA)[1], dim(MatrixData[[1]]$matA)[2], dim(MatrixData)))
  for (i in 1:dim(MatrixData)){
    trans_mat[,,i]<-MatrixData[[i]]$matA
  }
  return(list("survival matrices"=surv_mat, "fertility matrices"=fert_mat, "clonal matrices" = clon_mat, "transition matrices"=trans_mat))
}
