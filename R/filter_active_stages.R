#'Filter Active Stages
#'This function filters matrix data to output matrices that have two or more active stages
#'
#'@param tempMatClass pre-filtered compadre data. To see other parameters the data is filtered by see the function Load_Compadre_Data.R
#'@param tempMatrixData prefiltered compadre matrices.
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for 
#'Demographic Research (Germany). Available at www.compadre-db.org 
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu

filter_active_stages<-function(tempMatClass, tempMatrixData){
  active_stages<-list(NULL)
  active_stages_lessthan2<-NULL
  
  for (k in 1:length(tempMatClass)){
    active_stages[[k]]<-subset(tempMatClass[[k]]$MatrixClassNumber, tempMatClass[[k]]$MatrixClassOrganized=='active' & tempMatClass[[k]]$MatrixClassAuthor != "Seedling")
    active_stages_lessthan2[[k]]<-length(active_stages[[k]])<2
  }
  
  save<-which(!active_stages_lessthan2)
  excluded<-which(active_stages_lessthan2)
  active_stages<-active_stages[save]
  MatClass<-tempMatClass[save]
  MatrixData<-tempMatrixData[save]
  
  return(list(active_stages, MatClass, MatrixData, excluded))
}


