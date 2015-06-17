

species_names<-c("Lomatium_Bradshawii", "Cirsium_dissectum", "Cimicifuga_elata")
species<-vector("list", length(species_names))
names(species)<-species_names


for (k in length(species)){
  plant<-species
    plant[k]<-subset(mydata$metadata, SpeciesAuthor==species[k])
}



