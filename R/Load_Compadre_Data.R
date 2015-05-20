#' Read in a subset of the Compadre database
#'
#'This function loads data from the compadre data set and subsets the data of interest
#'
#'
#'@param Compadre_file The name of the Compadre data file, in Rdata format. If the file is not in the working directory then this variable should also include the relative or absolute path.
#'@return A list with metadata and mat slots following the same structure as the Compadre database but with only a subset of the data
#'  
#'  Notice that the first set of matrices (from Crone and Lesica 2004) appear to be 
#'  unreliable, as the fecundity terms are all in the last column.
#'
#'@references Code is adapted from Owen Jones & Rob Salguero-Gomez \url{https://github.com/jonesor/compadreDB/}
#'
#'@references COMPADRE Plant Matrix Database. Max Planck Institute for
#'Demographic Research (Germany). Available at www.compadre-db.org
#'(data downloaded on [1/1/2014]).
#'
#'@author Elizabeth Hiroyasu
#'

Load_Compadre_Data <- function(Compadre_file){
  load(Compadre_file)
  ##subset to all the metadata first
  metadata<- subset(compadre$metadata)

  ##then refine by specified components
  tempMetadata <- subset(metadata, MatrixComposite == "Individual" & MatrixTreatment == "Unmanipulated"
                         & GrowthType =="Herbaceous perennial" & StudyDuration >= 10 & MatrixDimension >= 3 &
                           MatrixCaptivity =="W" & MatrixSplit == "Divided")
  keep <- as.numeric(rownames(tempMetadata))
#  tempMetadata$tempMat <- compadre$mat[keep]

  return(list(metadata=tempMetadata, mat=compadre$mat[keep]))
}
