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
  #sampling 10 random survival and fertility matrices, and random N0 vectors
  
  surv_rand <- array(surv_mat, dim=c(length(nstage), length(nstage), length(years)))
  surv_rand <- surv_rand[,,sample(dim(surv_rand)[3], replace=T)]

  fert_rand <- array(fert_mat, dim=c(length(nstage), length(nstage), length(years)))
  fert_rand <- fert_rand[,,sample(dim(fert_rand)[3], replace=T)]
  
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years)
  ##to print the number of times the survival goes to zero in the stages where a trend has been inserted
  surv_zero <- surv_trend[2]
  
  ##calculating new abundance matrices
  abundance<- calc_abundance(N0_data, surv_trend[1], fert_rand)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, years, stage2mod)
  pv_surv<-(lm_surv$coefficients[2,4])
  
  ##abundance regression
  lm_abundance<- abundance_regr(abundance, years, stage2mod)
  pv_abundance<- (lm_abundance$coefficients[3,4])
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance, surv_zero))
}
