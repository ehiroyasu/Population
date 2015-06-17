

species_names<-c("Lomatium_Bradshawii", "Cirsium_dissectum", "Cimicifuga_elata")
species<-vector("list", length(species_names))
names(species)<-species_names


for (k in length(species)){
  plant<-species
    plant[k]<-subset(mydata$metadata, SpeciesAuthor==species[k])
}


pop_list <- unique(cbind(mydata$metadata$SpeciesAuthor,mydata$metadata$Population))
num_pops <- dim(pop_list)[1]
num_pops <- 2 # for debugging

for (k in 1:num_pops) {
  temp<-subset(mydata$metadata, SpeciesAuthor==pop_list[k,1] & Population==pop_list[k,2])
  
  #extracting CiPi Matrices
  save<-as.numeric(rownames(temp))
  MatrixData<- as.array(mydata$mat[mydata$metadata$SpeciesAuthor==pop_list[k,1] & mydata$metadata$Population==pop_list[k,2]])
  MatClass<-mydata$mat_class[save]
  print(pop_list[k,])
  
  # Insert rest of analysis
}
