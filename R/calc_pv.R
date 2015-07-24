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


calc_pv<-function(surv_mat, fert_mat, trans_mat, N0_data, nstage, years, stage2mod, beta, active_stages, verbose=FALSE){
  #sampling random survival matrices and fertility matrices with n years in simulation
  i_rand <- sample(1:dim(surv_mat)[3], size=length(years), replace=T)
  surv_rand <- surv_mat[,,i_rand]
  fert_rand <- fert_mat[,,i_rand]


  ##inserting a trend into survival
  temp <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years, verbose)
  surv_trend <- temp[[1]]
  ##to print the number of times the survival goes to zero in the stages where a trend has been inserted
  surv_zero <- temp[[2]]
  surv_na<-temp[[3]]
  
  ##calculating new abundance matrices
  abundance<- calc_abundance(N0_data, surv_trend, fert_rand, trans_mat, years)
  if (verbose) {
    print(abundance)
    plot(abundance[,active_stages])
  }
 
  
  ##survival regression
  lm_surv<- surv_regr(surv_trend, active_stages, abundance)
  pv_surv<-(lm_surv$coefficients[2,4])
  

  ##abundance regression
  lm_abundance<- abundance_regr(abundance[-1,], years, active_stages)
  pv_abundance<- (lm_abundance$coefficients[3,4])
  
  ##log abundance regression
  lm_log_abundance<-log_abundance_regr(abundance[-1,], years, active_stages)
  pv_log_abundance<-(lm_log_abundance$coefficients[3,4])
  
  #calculating lambda:
  N_active <- abundance[,active_stages]
  Nt <- apply(N_active, 1, sum)
  lambda<- Nt[-1]/Nt[-length(Nt)]
    lambda[is.na(lambda)]<-0
    
  mean_lambda<-mean(lambda)
  
    if(verbose) {
      plot(lambda)
      matplot(t(apply(surv_trend,c(2:3),sum)))
    }
  
  ##lambda regression:
  lm_lambda<-lambda_regr(lambda, years)
  pv_lambda<-(lm_lambda$coefficients[2,4])
  
  #calculating variances
  surv_var<-var(resid(lm_surv))
  abundance_var<-var(resid(lm_abundance))
  lambda_var<-var(resid(lm_lambda))

  #testing for autocorrelation
  autocorrelation_abundance<-autocorrelation_test(mod=lm_abundance)
  acf1_abundance<-autocorrelation_abundance[[2]]$acf[2]
  
  autocorrelation_lambda<-autocorrelation_test(mod=lm_lambda)
  acf1_lambda<-autocorrelation_lambda[[2]]$acf[2]
  
  
  return(list("survival p-value"=pv_surv, "abundance p-value"=pv_abundance, "log abundance p-value"=pv_log_abundance, "lambda p-value" = pv_lambda, 
              "n surv=0"=surv_zero, "n surv=na"=surv_na, "survival variance"=surv_var, "abundance variance" = abundance_var, 
              "lambda variance"=lambda_var, "mean_lambda"=mean_lambda, "acf1_abundance"=acf1_abundance, "acf1_lambda"=acf1_lambda))
}
