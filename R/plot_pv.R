#'plot_pv
#'
#'Plots different output values, and creates histograms of pvalues. This function can also be used to loop through a list of different outputs
#'
#'@param alpha is a vector of alpha values to compare p-values to and to assess power. Default alpha values are alpha=seq(from=0.01, to=0.2, by=delta) 
#'@param output is a list of different values, including survival p-values, abundance p-values, and lambda p-values
#'
#'@references data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
#'@author Elizabeth Hiroyasu

plot_pv<-function(output, alpha){
  library(ggplot2)
  library(reshape2)
  library(grid)
  library(gridExtra)
  
  ##P-value plots: extracting the p-values from the output list
  pvalues<-as.data.frame(output[1:3], header=TRUE)
  #for now, remove abundance
  #pvalues<-pvalues[,-2]
  pvalues$diff<-cbind(pvalues$lambda_pv-pvalues$survival_pv)
  
  
  #scatterplot
  pv_plot<-qplot(unlist(lambda_pv), unlist(survival_pv), data=pvalues[1:100,], size=1)
  pv_plot<-pv_plot+geom_abline()+scale_size_identity(guide="none")+theme_bw()+xlab("Lambda p-values")+
    ylab("Survival p-values")+ggtitle(paste(output$names, collapse="  "))
  
  #histograms
  lambda_hist<- ggplot(pvalues, aes(x=lambda_pv)) + geom_histogram(binwidth=0.1, colour="black", fill="white") + 
    ggtitle("Lambda p-value distribution") + xlab("Lambda p-values")
  
  survival_hist<-ggplot(pvalues, aes(x=survival_pv)) + geom_histogram(binwidth=0.1, colour="black", fill="white") + 
    ggtitle("Survival p-value distribution") + xlab("Survival p-values")
  
  diff_hist<-ggplot(pvalues, aes(x=diff)) + geom_histogram(binwidth=0.1, colour="black", fill="white") + 
    ggtitle("Lambda p-value minus Survival p-value distribution at beta = 0.01") + xlab("Difference")
  
  
  
  ##Plotting the proportional counts vs alpha
  #prop_pv<-melt(as.data.frame(output[4:6], header=TRUE))
  #leaving out abundance for now
  prop_pv<-as.data.frame(output[4:6], header=TRUE)
  #prop_pv<-prop_pv[,-2]
  alpha<-rep(alpha, dim(prop_pv)[2])
  prop_pv<-melt(prop_pv)
  prop_pv<-cbind(prop_pv, alpha)
  
  prop_plot<-ggplot(data=prop_pv, aes(y=value, x=alpha)) +geom_line(aes(colour=variable))+
    ggtitle(paste(output$names, collapse="  "))+xlab("Alpha")+ylab("Frequency p-value is less than alpha")
  
  plots<-grid.arrange(pv_plot, lambda_hist, survival_hist, diff_hist, prop_plot, ncol=1)
  
  return(plots)

}


