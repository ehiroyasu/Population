##Autocorrelation exploration of Actaea spicata, Site A population

# Data loading
#Load in the Compadre data and take the default selection
#The data file needs to be in the working directory. It can be downloaded from http://www.compadre-db.org.

library(DemographicTrend)
Compadre_file <- "COMPADRE_10_11_2014_version_3.0.Rdata"
mydata <- Load_Compadre_Data(Compadre_file)

# Subsetting the data to the focal species
AcSp<-subset(mydata$metadata, SpeciesAuthor=="Actaea_spicata" & Population =='Site A')

#pulling out AcSp Matrices
save<-as.numeric(rownames(AcSp))
MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor=="Actaea_spicata" & mydata$metadata$Population=='Site A'])

MatClass<-mydata$mat_class[save]

#create vector of active stages
active_stages<-subset(MatClass[[1]]$MatrixClassNumber, MatClass[[1]]$MatrixClassOrganized=='active')

#Extract the matrices
temp<- extract_mat(MatrixData)

##extracting AcSp survival matrices
surv_mat<-temp$"survival matrices"

##extracting AcSp fertility matrices
fert_mat<-temp$"fertility matrices"

##extracting AcSp transition matrices
trans_mat<-temp$"transition matrices"

i_rand <- sample(1:dim(surv_mat)[3], size=length(years), replace=T)
surv_rand <- surv_mat[,,i_rand]
fert_rand <- fert_mat[,,i_rand]

##inserting a trend into survival
temp <- insert_survival_trend(surv_rand, beta, nstage, stage2mod, years)#, verbose)
surv_trend <- temp[[1]]

##calculating new abundance matrices
abundance<- calc_abundance(N0_data, surv_trend, fert_rand, trans_mat, years)

#calculating lambda:
N_active <- abundance[,active_stages]
Nt <- apply(N_active, 1, sum)
lambda<- Nt[-1]/Nt[-length(Nt)]
lambda[is.na(lambda)]<-0

##abundance regression
lm_abundance<- abundance_regr(abundance[-1,], years, active_stages)

#testing for autocorrelation
temp<-autocorrelation_test(mod=lm_abundance)
abundance_res<- temp[[1]]
abundance_acf<- temp[[2]]
lm_abundance_res<- temp[[3]]
summary(lm_abundance_res)
abundance_res_plot<-temp[[4]]

#DurbinWatson Test for autocorrelation
library(car)
dwt(abundance_res)
dwt(lm_abundance_res)

##lambda regression:
lm_lambda<-lambda_regr(lambda, years)
temp<-autocorrelation_test(mod=lm_lambda)
lambda_res<- temp[[1]]
lambda_acf<- temp[[2]]
lm_lambda_res<- summary(temp[[3]])
lambda_res_plot<-temp[[4]]

