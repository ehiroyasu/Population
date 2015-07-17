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
#k <- kk[pop_list[,1]=="Adenocarpus_gibbsianus" & pop_list[,2]=="Donana"]

# Cheirolophus_metlesicsii Barranco de Anavingo: Demog is by far the worst
k <- kk[pop_list[,1]=="Cheirolophus_metlesicsii" & pop_list[,2]=="Barranco de Anavingo"]

# Erodium_paularense Cañamares I - has survival > 1
k <- kk[pop_list[,1]=="Erodium_paularense" & pop_list[,2]=="Cañamares I"]

# Petrocoptis_pseudoviscosa_2 Abi

k <- 68
k <- 92
k <- 105
k <- 212

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

all_zero <- apply(trans_mat, 3, mean) == 0
surv_mat <- surv_mat[,,!all_zero]
fert_mat <- fert_mat[,,!all_zero]
trans_mat <- trans_mat[,,!all_zero]

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

