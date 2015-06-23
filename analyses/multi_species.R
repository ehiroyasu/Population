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

  #extracting matrices:
  temp<- extract_mat(MatrixData)
  surv_mat<-temp$"survival matrices"
  fert_mat<-temp$"fertility matrices"
  trans_mat<-temp$"transition matrices"
  
  #calculating the number of stages in the matrix:
  nstage<-1:dim(surv_mat)[1]
 
  
  #Running simulations
  return_pv<-replicate(10000, calc_pv(surv_mat, fert_mat, trans_mat, N0_data, nstage, years=1:10, stage2mod=active_stages, beta=0.01, active_stages = active_stages))
  
  #Analysis
  delta=0.001
  alpha=seq(from=0.01, to=0.2, by=delta)
  output<-analyze_pv(alpha, return_pv)
  
  #Visualizing the data

  plot(output$prop_demog~alpha, type='l',col='blue', xlab='alpha', ylab='freq p<alpha', main=poplist[k,], xlim=c(0,0.2), ylim=c(0,1))
  lines(output$prop_lambda~alpha, col='red')
  legend("topleft", c("Survival p-values", "Lambda p-values"), col=c("blue","red"), lty=c(1,1))
  
  
  plot(output$survival_pv~output$lambda_pv, main='Survival p-value vs. Lambda p-value', xlab='lambda p-values', ylab='survival p-values')
  abline(0,1)
  
  hist(output$lambda_pv-output$survival_pv, main='Difference of lambda_pv minus survival_pv at beta=0.01')
  hist(output$lambda_pv, main='Histogram of lambda p-values')
  hist(output$survival_pv, main='Histogram of survival p-values')
  
}

