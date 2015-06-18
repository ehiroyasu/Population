



pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
num_pops <- dim(pop_list)[1]
num_pops <- 2 # for debugging

for (k in 1:num_pops) {
  temp<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
  
  #extracting CiPi Matrices
  save<-as.numeric(rownames(temp))
  MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
  MatClass<-mydata$mat_class[save]

  active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

  ##pulling out CiPi survival matrices
  surv_mat<-(extract_mat(MatrixData))$"survival matrices"
  
  ##pulling out CiPi fertility matrices
  fert_mat<-(extract_mat(MatrixData))$"fertility matrices"
  
  ##pulling out CiPi transition matrices
  trans_mat<-(extract_mat(MatrixData))$"transition matrices"
  
  #calculating the number of stages in the matrix:
  nstage<-1:dim(surv_mat)[1]
  
  N0_data<-gen_N0(trans_mat)
  
  return_pv<-replicate(10, calc_pv(surv_mat, fert_mat, N0_data, nstage, years=1:10, stage2mod=1:3, beta=0.01))
  
  print(return_pv[k,])  
  
}
