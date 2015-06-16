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


calc_pv<-function(surv_mat, fert_mat, N0_data, nstage, years, stage2mod, beta, active_stages){
  #sampling random survival matrices and fertility matrices with n years in simulation
  i_rand <- sample(1:dim(surv_mat)[3], size=length(years), replace=T)
  surv_rand <- surv_mat[,,i_rand]
  fert_rand <- fert_mat[,,i_rand]
  
  
  #calculating the variance in survival:
  #surv_mad<- calc_surv_mad(surv_rand, stage2mod, years)
   
  ##to modify stage 2 survival, use trend function
  surv_trend <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years)[[1]]
  ##to print the number of times the survival goes to zero in the stages where a trend has been inserted
  surv_zero <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years)[[2]]
  
  ##calculating new abundance matrices
  abundance<- calc_abundance(N0_data, surv_trend, fert_rand)
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend)
  pv_surv<-(lm_surv$coefficients[2,4])
  
  #calculating survival variance
  surv_var<-var(resid(lm_surv))
  
  ##abundance regression
  lm_abundance<- abundance_regr(abundance[-1,], years, active_stages)
  pv_abundance<- (lm_abundance$coefficients[3,4])
  
  #calculating lambda:
  N_active <- abundance[,active_stages]
  Nt <- apply(N_active, 1, sum)
  lambda<- Nt[-1]/Nt[-length(Nt)]
    lambda[is.na(lambda)]<-0
  
  
  ##lambda regression:
  lm_lambda<-lambda_regr(lambda)
  pv_lambda<-(lm_lambda$coefficients[2,4])
  
  #lambda regression variance
  lambda_var<-var(resid(lm_lambda))
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance, "lambda p-value" = pv_lambda, 
              "n surv=0"=surv_zero, "survival variance"=surv_var, "lambda variance"=lambda_var))
}
