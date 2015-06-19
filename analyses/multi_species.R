library(DemographicTrend)

CompadreFile <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(CompadreFile)



pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
num_pops <- dim(pop_list)[1]
num_pops <- 2 # for debugging

for (k in 1:num_pops) {
  temp<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
  
  #extracting Matrices
  save<-as.numeric(rownames(temp))
  MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
  MatClass<-mydata$mat_class[save]

  active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

  ##pulling out survival matrices
  surv_mat<-(extract_mat(MatrixData))$"survival matrices"
  
  ##pulling out fertility matrices
  fert_mat<-(extract_mat(MatrixData))$"fertility matrices"
  
  ##pulling out transition matrices
  trans_mat<-(extract_mat(MatrixData))$"transition matrices"
  
  #calculating the number of stages in the matrix:
  nstage<-1:dim(surv_mat)[1]
  
  N0_data<-gen_N0(trans_mat)
  
  return_pv<-replicate(10000, calc_pv(surv_mat, fert_mat, N0_data, nstage, years=1:10, stage2mod=1:3, beta=0.01))
  
  delta<-0.001
  alpha<-seq(from=0.01, to=0.2, by=delta)
  
  
  survival_pv<-unlist(return_pv[1,])
  abundance_pv<-unlist(return_pv[2,])
  lambda_pv<-unlist(return_pv[3,])
  
  prop_demog<-pv_lessthan_alpha(pvalue=survival_pv, alpha)
  prop_N<-pv_lessthan_alpha(pvalue=abundance_pv,alpha)
  prop_lambda<-pv_lessthan_alpha(pvalue=lambda_pv, alpha)
  
  diff<-(prop_lambda-prop_demog)
  
  plot(prop_demog~alpha, type='l',col='blue', xlab='alpha', ylab='freq p<alpha', main=pop_list[k,], xlim=c(0,0.2), ylim=c(0,1))
  lines(prop_lambda~alpha, col='red')
  legend("bottomright", c("Survival p-values", "Lambda p-values"), col=c("blue","red"), lty=c(1,1))
  
  
  plot(survival_pv~lambda_pv, col=c("blue", "red"), main='Survival p-value vs. Lambda p-value')
  abline(0,1)
  legend("topleft", c("Survival p-values", "Lambda p-values"), col=c("blue","red"), lty=c(1,1))
  
  hist(diff, main='difference of lambda_pv minus survival_pv at beta=0.01')
  
  print(pop_list[k,])
  
}
