#'simulation function
#'
#'brings together processes for analyzing an inserted survival trend in order to run simulations
#'
#'@param surv_rand array of randomized survival matrices from COMPADRE database
#'@param fert_rand array of randomized fertility matrices from COMPADRE database
#'@param N0_rand matrix of randomized N0 vectors
#'@param nstage number of stages in matrix data
#'@param surv_trend array of survival matrices with inserted trend beta
#'
#'@author Elizabeth Hiroyasu
#'


trend_sim<-function(surv_rand, fert_rand, N0_rand, nstage, surv_trend){
  #sampling 10 random survival and fertility matrices, and 10 random N0 vectors
  surv_rand <- surv_mat[,,sample(dim(surv_mat)[3])]
  fert_rand<-fert_mat[,,sample(dim(fert_mat)[3])]
  N0_rand <- N0_data[sample(dim(N0_data)[1]),]
  
  ##Adding a trend to survivals:
  #first, calculate the number of stages in our system:
  nstage<-1:dim(surv_rand)[1]
  
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta = 0.1, nstage, stage2mod=4)
  
  ##calculating new abundance matrices
  abundance<- abundance_func(N0_rand, surv_trend, fert_rand)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, years, stage2mod=4)
  pv_surv<-(lm_surv$coefficients[2,4])
  
  ##abundance regression
  lm_abundance<- abundance_regr(abundance, years, stage2mod=4)
  pv_abundance<- (lm_abundance$coefficients[2,4])
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance))
}
