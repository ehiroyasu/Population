#'calc_pv
#'
#'Calculates the p-values of survival and abundance regressions over time after a trend is inserted into survival.
#'
#'@param surv_mat array of survival matrices from COMPADRE database
#'@param fert_mat array of fertility matrices from COMPADRE database
#'@param N0_data matrix of N0 vectors
#'@param nstage number of stages in matrix data
#'@param years number of years in simulation
#'@param stage2mod stage that is modified by inserted survival trend
#'@param beta is the amount by which survival will be modified in a stage
#'
#'@author Elizabeth Hiroyasu
#'


calc_pv<-function(surv_mat, fert_mat, N0_data, nstage, years, stage2mod, beta){
  #sampling 10 random survival and fertility matrices, and 10 random N0 vectors
  surv_rand <- surv_mat[,,sample(dim(surv_mat)[3])]
#<<<<<<< HEAD
  fert_rand<-fert_mat[,,sample(dim(fert_mat)[3])]
#=======
  fert_rand<-fert_mat[,,sample(dim(fert_mat)[3])]
#>>>>>>> LoBr
  N0_rand <- N0_data[sample(dim(N0_data)[1]),, drop=FALSE]
  
  ##Adding a trend to survivals:
  #first, calculate the number of stages in our system:
  nstage<-1:dim(surv_rand)[1]
  
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta, nstage, stage2mod)
  
  ##calculating new abundance matrices
  abundance<- calc_abundance(N0_rand, surv_trend, fert_rand)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, years, stage2mod)
  pv_surv<-(lm_surv$coefficients[2,4])
  
  ##abundance regression
  lm_abundance<- abundance_regr(abundance, years, stage2mod)
  pv_abundance<- (lm_abundance$coefficients[3,4])
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance))
}
