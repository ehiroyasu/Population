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
  #sampling random survival matrices and fertility matrices with n years in simulation
  i_rand <- sample(1:dim(surv_mat)[3], size=length(years), replace=T)
  surv_rand <- surv_mat[,,i_rand]
  fert_rand <- fert_rand[,,i_rand]
  
  #calculating the variance in survival:
  #surv_var<- calc_surv_var(surv_rand, stage2mod, years)
   
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years)[[1]]
  ##to print the number of times the survival goes to zero in the stages where a trend has been inserted
  surv_zero <- surv_trend[[2]]
  
  ##calculating new abundance matrices
  abundance<- calc_abundance(N0_data, surv_trend, fert_rand)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, years, stage2mod)
  pv_surv<-(lm_surv$coefficients[2,4])
  
  ##abundance regression
  lm_abundance<- abundance_regr(abundance, years, stage2mod)
  pv_abundance<- (lm_abundance$coefficients[3,4])
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance, "n surv=0"=surv_zero, "survival variance"=surv_var))
}
