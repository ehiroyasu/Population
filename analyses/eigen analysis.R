load("../Pop_dem_Output_data.Rdata")
num_sp <- length(output)
bad_lambda <- data.frame(species=NULL, site=NULL, prop05=NULL, imPart=NULL, imFrac=NULL, rho=NULL)
for (i in 1:num_sp) {
  if (!is.null(output[[i]])) {
    if (is.complex(output[[i]]$eigenvalues.values[2])) {
      imPart <- Im(output[[i]]$eigenvalues.values[2])
      imFrac <- imPart/(imPart+Re(output[[i]]$eigenvalues.values[2]))
    } else {
      imPart <- 0
      imFrac <- 0
    }
    prop05 <- sum(output[[i]]$lambda_pv < 0.1) / length(output[[i]]$lambda_pv)
    rho <- with(output[[i]], Re(eigenvalues.values[1])/ sqrt(Re(eigenvalues.values[2])^2 + Im(eigenvalues.values[2])^2))
    acf1 <- with(output[[i]], mean(acf1_lambda))
    bad_lambda <- rbind(bad_lambda, data.frame(species=output[[i]]$names[1], 
                                               site=output[[i]]$names[2],
                                               prop05=prop05, imPart=imPart,
                                               imFrac=imFrac, rho=rho, acf1))
  }
}
plot(prop05~acf1, data=bad_lambda, ylim=c(0,1))


power10 <- data.frame(species=NULL, site=NULL, surv10=NULL, lambda10=NULL)
for (i in 1:num_sp) {
  if (!is.null(output[[i]])) {
    lambda10 <- sum(output[[i]]$lambda_pv < 0.1) / length(output[[i]]$lambda_pv)
    surv10 <- sum(output[[i]]$survival_pv < 0.1) / length(output[[i]]$survival_pv)
    
    power10 <- rbind(power10, data.frame(species=output[[i]]$names[1], 
                                               site=output[[i]]$names[2],
                                               surv10=surv10, lambda10=lambda10))
  }
}
plot(lambda10~surv10, data=power10)
abline(0,1)



load("../Pop_dem_Output_data.Rdata")

CompadreFile <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(CompadreFile)
pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
kk <- 1:dim(pop_list)[1]

library(popbio)
num_sp <- length(output)
sens.df <- data.frame(species=NULL, site=NULL, lambda10=NULL, logN10=NULL, surv10=NULL, diff10=NULL, surv_sens=NULL, surv_elas=NULL, gen.time=NULL, rank=NULL, fert_var=NULL, surv_var=NULL)
for (i in 1:num_sp) {
  if (!is.null(output[[i]])) {
    lambda10 <- sum(output[[i]]$lambda_pv < 0.1) / length(output[[i]]$lambda_pv)
    logN10 <- sum(output[[i]]$log_abundance_pv < 0.1) / length(output[[i]]$log_abundance_pv)
    surv10 <- sum(output[[i]]$survival_pv < 0.1) / length(output[[i]]$survival_pv)
    diff10 <- surv10 - lambda10
    k <- kk[pop_list[,1]==output[[i]]$names[1] & pop_list[,2]==output[[i]]$names[2]]
    temp1<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
    
    MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
    
    save<-as.numeric(rownames(temp1))
    MatClass<-mydata$mat_class[save]
    
    active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

        temp<- extract_mat(MatrixData)
    surv_mat<-temp$"survival matrices"
    trans_mat<-temp$"transition matrices"
    fert_mat<-temp$"fertility matrices"
    all_zero <- apply(trans_mat, 3, mean) == 0
    surv_mat <- surv_mat[,,!all_zero]
    trans_mat <- trans_mat[,,!all_zero]
    fert_mat <- fert_mat[,,!all_zero]
    mean_trans_mat<-apply(trans_mat, c(1,2), mean)
    mean_surv_mat<-apply(surv_mat, c(1,2), mean)
    mean_fert_mat<-apply(fert_mat, c(1,2), mean)
    sens_mat <- sensitivity(mean_trans_mat)*(mean_surv_mat > 0)
    surv_elas <- sum(elasticity(mean_trans_mat)*(mean_surv_mat > 0))
    surv_stage <- apply(mean_surv_mat, 2, sum)
    sens_mat <- sens_mat[,surv_stage>0]
    mean_surv_mat <- mean_surv_mat[,surv_stage>0]
    surv_stage <- surv_stage[surv_stage>0]
    surv_sens <- sum((apply(sens_mat*mean_surv_mat,2,sum)/surv_stage)[active_stages])
    gen.time <- generation.time(mean_trans_mat, r=mean_fert_mat)
    rank <- dim(mean_trans_mat)[1]
    fert_var <- var(apply(fert_mat, 3, sum))
    surv_var <- var(apply(surv_mat, 3, sum))
    sens.df <- rbind(sens.df, data.frame(species=output[[i]]$names[1], 
                                          site=output[[i]]$names[2],
                                          lambda10, logN10, surv10, diff10, surv_sens, surv_elas, gen.time, rank, fert_var, surv_var))
  }
}

summary(lm(diff10 ~ surv_sens, data=sens.df))
summary(lm(lambda10 ~ surv_sens, data=sens.df))
plot(lambda10 ~ surv_sens, data=sens.df)
plot(surv10 ~ surv_sens, data=sens.df)
summary(lm(logN10 ~ surv_sens, data=sens.df))
plot(logN10 ~ surv_sens, cex=log(gen.time), data=sens.df)
summary(lm(logN10 ~ gen.time+surv_sens, data=sens.df))
plot(logN10 ~ gen.time, data=sens.df, log='x')
plot(logN10 ~ surv_sens, cex=rank-2, data=sens.df)
plot(logN10 ~ surv_elas, cex=10*surv_var, data=sens.df)
plot(logN10 ~ surv_var, data=sens.df, log='x', cex=surv_sens)
plot(surv10~logN10, data=sens.df, cex=5*log(surv_var+1))
abline(0,1)

plot(diff10~surv_sens, data=sens.df)
plot(diff10~surv_elas, data=sens.df)
plot(diff10~surv_var, data=sens.df)
plot(diff10~fert_var, data=sens.df, log='x')
summary(lm(diff10~log(fert_var+0.0001), data=sens.df))
plot(diff10~gen.time, data=sens.df, log='x')
