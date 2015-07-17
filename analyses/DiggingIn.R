# Interactive file for looking into a particular population

library(DemographicTrend)

CompadreFile <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(CompadreFile)

pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
kk <- 1:dim(pop_list)[1]

# Arenaria_bolosii Puig de Massanella: much better lambda P values

k <- kk[pop_list[,1]=="Arenaria_bolosii" & pop_list[,2]=="Puig de Massanella"]

# Adenocarpus_gibbsianus Palos de la Frontera: Much better N p values
k <- kk[pop_list[,1]=="Adenocarpus_gibbsianus" & pop_list[,2]=="Palos de la Frontera"]

# Adenocarpus_gibbsianus Palos de la Frontera: Much better N p values
k <- kk[pop_list[,1]=="Adenocarpus_gibbsianus" & pop_list[,2]=="Donana"]

### Extract the matrices
temp1<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
#extracting Matrices
save<-as.numeric(rownames(temp1))
MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
MatClass<-mydata$mat_class[save]

active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

#extracting matrices:
temp<- extract_mat(MatrixData)
surv_mat<-temp$"survival matrices"
fert_mat<-temp$"fertility matrices"
trans_mat<-temp$"transition matrices"
#calculating the number of stages in the matrix:
nstage<-1:dim(surv_mat)[1]

####################################################

### Look at a few things
trans_mat
temp1
active_stages
MatClass

calc_pv(surv_mat, fert_mat, trans_mat, N0_data, nstage, years=1:10, stage2mod=active_stages, beta=0.01, active_stages = active_stages,verbose = TRUE)

#### Run the analysis



#Running simulations
return_pv<-replicate(10000, calc_pv(surv_mat, fert_mat, trans_mat, N0_data, nstage, years=1:10, stage2mod=active_stages, beta=0.01, active_stages = active_stages))

#Analysis
output<-analyze_pv(alpha, return_pv)
output<-c(output, names=list(pop_list[k,]))

