#'Load_Data
#'
#'Loading compadre matrix data
#'This function loads data from the compadre data set
#'
#'@references Code is adapted from Owen Jones & Rob Salguero-Gomez https://github.com/jonesor/compadreDB/
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for
#'Demographic Research (Germany). Available at www.compadre-db.org
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu
#'

Load_Data <- function(){
  load("COMPADRE_10_11_2014_version_3.0.RData")
  ##subset to all the metadata first
  metadata<- subset(compadre$metadata)

  ##then refine by specified components
  tempMetadata <- subset(metadata, MatrixComposite == "Individual" & MatrixTreatment == "Unmanipulated"
                         & GrowthType =="Herbaceous perennial" & StudyDuration >= 10 & MatrixDimension >= 3 &
                           MatrixCaptivity =="W" & MatrixSplit == "Divided")
  keep <- as.numeric(rownames(tempMetadata))
  tempMetadata$tempMat <- compadre$mat[keep]

  return(tempMetadata)
}

