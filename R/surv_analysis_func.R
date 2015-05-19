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
  surv_rand <- surv[,,sample(10)]
  cipi_f_rand<-fert[,,sample(10)]
  N0_rand <- N0_data[sample(10),]
  
  ##Adding a trend to survivals:
  #first, calculate the number of stages in our system:
  nstage<-1:dim(surv_rand)[1]
  
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta = 0.1, nstage, stage2mod=2)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, years, stage2mod=2)
  pv<-(lm_surv$coefficients[2,4])
  return(pv)
}









