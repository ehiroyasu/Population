library(DemographicTrend)

CompadreFile <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(CompadreFile)

pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
num_pops <- dim(pop_list)[1]
#num_pops <- 2 # for debugging
delta=0.001
alpha=seq(from=0.01, to=0.2, by=delta)

#output <- list(NULL)

for (k in 212:num_pops) {
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
 
  
  #Running simulations
  return_pv<-replicate(100, calc_pv(surv_mat, fert_mat, trans_mat, #fert_zero, 
                                    N0_data, nstage=nstage, years=1:10, stage2mod=active_stages, beta=0.01, active_stages = active_stages, verbose=FALSE))
  
  # (surv_mat, fert_mat, trans_mat, N0_data, nstage, years, stage2mod, beta, active_stages, verbose=FALSE)
  #Analysis
  output[[k]]<-analyze_pv(alpha, return_pv)
  output[[k]]<-c(output[[k]], names=list(pop_list[k,]))
  }
  }
  
}

save(output, file="Pop_dem_Output_data.Rdata")

##plotting for a single species:
#plots<-plot_pv(output[[1]], alpha)

#plotting for multiple species:
#first create a vector of names
names<-vector(length=length(pop_list[,1]))
for(i in 1:length(names)){
  names[i]<-paste(pop_list[i,], collapse="  ")
}

#plot to pdf of individual files each
# for (k in 1:num_pops){
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
