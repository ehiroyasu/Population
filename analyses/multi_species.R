library(DemographicTrend)

CompadreFile <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(CompadreFile)

pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
num_pops <- dim(pop_list)[1]
#num_pops <- 50 # for debugging
delta=0.001
alpha=seq(from=0.01, to=0.2, by=delta)

output <- list(NULL)
output_alpha.1 <- list(NULL)
for (k in 1:num_pops) {
  print(k)
  temp1<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
  
  MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
  
  save<-as.numeric(rownames(temp1))
  MatClass<-mydata$mat_class[save]
  
  active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

  #extracting matrices:
  temp<- extract_mat(MatrixData)
  surv_mat<-temp$"survival matrices"
  fert_mat<-temp$"fertility matrices"
  trans_mat<-temp$"transition matrices"
  
  # Remove all-zero years, as that causes problems
  all_zero <- apply(trans_mat, 3, mean) == 0
  surv_mat <- surv_mat[,,!all_zero]
  fert_mat <- fert_mat[,,!all_zero]
  trans_mat <- trans_mat[,,!all_zero]
  
  # The following populations have really strange matrices: 68
  # The following matrices have some years of zero survival and others of zero fertility: 212
  if(length(dim(trans_mat)) > 2){
  if (mean(fert_mat) > 0 & k != 68 & k != 212 & dim(trans_mat)[3] >= 5 & max(temp1$SurvivalIssue) <= 1) {
  #calculating the number of stages in the matrix:
  nstage<-1:dim(surv_mat)[1]
  
  #calculating the mean transition matrices and eigenvalues
  mean_trans_mat<-apply(trans_mat, c(1,2), mean)
  eigenvalues<-eigen(mean_trans_mat)
  dom_eigenvalue<-eigenvalues$values[1]
  
  #Running simulations
  return_pv<-replicate(10000, calc_pv(surv_mat, fert_mat, trans_mat, N0_data, nstage=nstage, years=1:10, stage2mod=active_stages, beta=0.01, active_stages = active_stages, verbose=FALSE))
  
  # (surv_mat, fert_mat, trans_mat, N0_data, nstage, years, stage2mod, beta, active_stages, verbose=FALSE)
  #Analysis
  output[[k]]<-analyze_pv(alpha, return_pv)
  output[[k]]<-c(output[[k]], names=list(pop_list[k,]), eigenvalues=eigenvalues)


  }
  }
  
}

save(output, file="Pop_dem_Output_data.Rdata")


##to examine for a specific alpha value
prop_demog_0.1<-matrix(data=NA)
prop_N_0.1<-matrix(data=NA)
prop_lambda_0.1<-matrix(data=NA)
names0.1<-matrix(data=NA)
for (i in 1:length(output)){
  prop_demog_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$survival_pv, alpha=0.1))
  prop_N_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$abundance_pv, alpha=0.1))
  prop_lambda_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$lambda_pv, alpha=0.1))
  names0.1[i]<-paste(output[[i]]$names, collapse="  ")
  prop_0.1<-data.frame(prop_demog_0.1, prop_N_0.1, prop_lambda_0.1, names0.1)
  prop_0.1<-prop_0.1[complete.cases(prop_0.1), ]
}

#scatterplot for all species at a specific alpha value
alpha0.1_plot<-qplot(prop_lambda_0.1, prop_demog_0.1, data=prop_0.1, size=3, color=names0.1)
alpha0.1_plot<-alpha0.1_plot+geom_abline()+scale_size_identity(guide="none")+theme_bw()+xlab("Lambda Power")+
  ylab("Survival Power")+ggtitle("Power of Lambda vs Power of Survival for all Species")+theme(legend.position="none")

##plotting for a single species:
#plots<-plot_pv(output[[1]], alpha)

#plotting for multiple species:
#first create a vector of names
names<-vector(length=length(pop_list[,1]))
for(i in 1:length(names)){
  names[i]<-paste(pop_list[i,], collapse="  ")
}

#plot to pdf of individual files each
#  for (k in 1:num_pops){
# 
#  pdf(paste("plot", names[k], ".pdf", sep=" "), height=20)
#  plots[[k]]<-plot_pv(output=output[[k]], alpha)
#  dev.off()
# }


#plot into single pdf:
plots<-list(NULL)

pdf(paste("allplots.pdf"), height=20)
for (k in 1:num_pops){
  print(k)
  if ( !is.null(output[[k]]) ) {
    plots[[k]]<-plot_pv(output=output[[k]], alpha)
  }
}

dev.off()
