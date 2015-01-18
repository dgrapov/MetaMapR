#load as package from github
# install.packages('radiant',repos=c("https://github.com/dgrapov/MetaMapR", CRAN = "http://cran.rstudio.com"), dependencies = TRUE) 
# shiny::runApp(system.file('MetaapR', package='radiant'), port = 8100)


library(shiny)
#if no internet load local devium scripts
# source.local.dir("C:/Users/dgrapov/Dropbox/Devium/devium/R")

#load MetaMapR
runApp("C:\\Users\\D\\Dropbox\\Software\\MetaMapR") # home PC
runApp("C:\\Users\\think\\Dropbox\\Software\\MetaMapR") # laptop
runApp("C:\\Users\\Dmitry\\Dropbox\\Software\\MetaMapR") # laptop
runApp("/Users/dgrapov/Dropbox/Software/MetaMapR") # OSX

#read
cat(readLines('d3label.html'))



# #test metabolomic data
{
setwd("C:/Users/D/Dropbox/Software/MetamapR/data")
x<-read.csv("example data.csv")
spectra <- x$Mass_Spectra
CID <-x$PubChem_Index
kegg.id<-x$KEGG_Index

#KEGG
reaction.DB<-get.KEGG.pairs(type="main")
edge.list<-tryCatch(get.Reaction.pairs(kegg.id,reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE),error=function(e){NULL})
index<-kegg.id
#create shared index to combine different edge ids
edge.names<-data.frame(index, network.id = c(1:length(index)))
kegg.edges<-data.frame(make.edge.list.index(edge.names,edge.list),type="KEGG",weight=2)
type<-factor(kegg.edges$type,labels=unique(kegg.edges$type),levels=unique(kegg.edges$type),ordered=TRUE)
kegg.edges$type<-type
res<-clean.edgeList(data=kegg.edges)

#pubchem
tani.edges<-CID.to.tanimoto(cids=fixlc(CID), cut.off = .7, parallel=FALSE)
#create shared index between different edge ids
edge.names<-data.frame(CID[,2], network.id = c(1:length(CID[,1])))
tani.edges[,1:2]<-make.edge.list.index(edge.names,tani.edges)
tani.edges$type<-1
clean.edgeList(tani.edges)

#mass spec
library(reshape2)
spec.edges<-get.spectral.edge.list(spectra = spectra, known = NULL, cutoff = 0, edge.limit = 2,retention.index=NULL,retention.cutoff=NULL)						
spec.edges<-data.frame(as.matrix(spec.edges[,1:2]),type = "m/z", weight = spec.edges[,3])
edge.names<-data.frame(index, network.id = c(1:length(spectra)))
spec.edges[,1:2]<-make.edge.list.index(edge.names,spec.edges)
spec.edges<-data.frame(as.matrix(spec.edges[,1:2]),type = "M/Z", weight = spec.edges[,3,])
values$edgelist.error.message$mz<-NULL							


}



