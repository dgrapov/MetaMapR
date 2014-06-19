
library(shiny)
#if no internet load local devium scripts
source.local.dir("C:/Users/dgrapov/Dropbox/Devium/devium/R")

#load MetaMapR
runApp("C:\\Users\\D\\Dropbox\\Software\\MetaMapR") # home PC
runApp("C:\\Users\\Node\\Dropbox\\Devium\\devium\\Shiny\\MetaMapR") # home laptop
runApp("C:\\Users\\dgrapov\\Dropbox\\Software\\MetaMapR") # work
runApp("/Users/dgrapov/Dropbox/Software/MetaMapR") # OSX


# #test data for spectra and correlation
# # setwd("C:/Users/D/Desktop")
# # setwd("C:\\Users\\Node\\Dropbox\\Devium\\devium\\Shiny\\MetaboMapR")
# # setwd("C:/Users/D/Dropbox/Devium/devium/Shiny/MetaMapR")
# # setwd("C:/Users/dgrapov/Dropbox/Devium/devium/Shiny/MetaMapR")
metabolomics.data<-x<-read.csv("example data.csv")
spectra <- x$mass.spec
CID <-x$PubChem.id

#KEGG translations
kegg.id<- TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
reaction.DB<-get.KEGG.pairs(type="main")
edge.list<-get.Reaction.pairs(kegg.id,reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE)
index<-kegg.id
#create shared index to combine different edge ids
edge.names<-data.frame(index, network.id = c(1:length(index)))
kegg.edges<-data.frame(make.edge.list.index(edge.names,edge.list),type="KEGG",weight=2)

#Tanimoto calculations
library(CTSgetR)
CID<-CTSgetR(id = fixlc(kegg.id), from="KEGG",to="PubChem CID", async=FALSE,limit.values=TRUE)
tani.edges<-CID.to.tanimoto(cids=fixlc(CID[,2]), cut.off = .7, parallel=FALSE)
#create shared index between different edge ids
edge.names<-data.frame(CID[,2], network.id = c(1:length(CID[,1])))
tani.edges[,1:2]<-make.edge.list.index(edge.names,tani.edges)

#spectra connections with known and 
spectra <- x$Mass_Spectra
known<-x$Main
retention<-x$Retention_Index
index<-spectra
	
spec.edges<-get.spectral.edge.list(spectra = index, known = known,#known, 
		cutoff = 0.7, edge.limit = 2,retention.index=NULL,retention.cutoff=10000)
		
tmp<-spec.edges
filtered<-edge.list.filter(edge.list=tmp[,1:2],value=(fixln(tmp[,3])), max.edges=1, separate=FALSE, decreasing=TRUE)
tmp[filtered,]
		
		
#get correlation based connections
id<-agrep("Sample",colnames(x))
data<-x[,id]
tmp.data<-t(data)
colnames(tmp.data)<-1:nrow(data)
tmp<-devium.calculate.correlations(tmp.data,type="spearman", results = "edge list")            

#filter total number of connections per node
filtered<-edge.list.filter(edge.list=tmp[,1:2],value=(fixln(tmp[,3])), max.edges=1, separate=TRUE, decreasing=TRUE)
filtered<-edge.list.filter2(edge.list=tmp[,1:2],weight=(fixln(tmp[,3])), max.edges=1)
tmp[filtered,]


edge.list<-matrix(c(1,2,2,3,4,5,5,1,3,4,3,1),ncol=2,byrow=TRUE)
weight<-c(1:nrow(edge.list))

id<-edge.filter(edge.list,weight,max.edges=1)
cbind(edge.list,weight)[id,]

edge.list.filter2(edge.list,weight,max.edges=1)

full<-full[order(full[,3],decreasing=TRUE),]
#interleave filtering of connected source nodes
nodes<-unique(fixlc(edge.list[,1]))
id<-fixlc(edge.list[,2])%in%nodes
id2<-id&fixlc(edge.list[,1])%in%nodes


id1<-edge.source.filter(edge.list[id,],weight[id],max.edges=1)
full[id,][id1,]
id2<-edge.source.filter(edge.list[!id,],weight[!id],max.edges=1)
full[id,][id2,]
id3<-unique()
rbind(full[id,][id1,],full[id,][id2,])
#limit to X top edges per node use --> full.edge.filter

edge.source.filter<-function(edge.list,weight,max.edges=1){
	nodes<-unique(fixlc(edge.list[,1]))
	id<-fixlc(edge.list[,2])%in%nodes
	#flip source target (expecting undirected edges) to get all of one index on one side
	#test what happens when source nodes are connected
	tmp<-as.matrix(edge.list[,1:2])
	tmp[id,1:2]<-as.matrix(edge.list[id,2:1])
	tmp<-data.frame(tmp)
	#return positions of top edges
	#add index
	tmp$tmp.id<-c(1:nrow(tmp))
	tmp$tmp.weight<-weight
	
	tmp2<-split(tmp,as.factor(tmp[,1]))
	top.edges<-do.call("rbind",lapply(1:length(tmp2),function(i){
			obj<-tmp2[[i]][order(tmp2[[i]][,3],decreasing=TRUE),]
			obj[c(1:nrow(obj))<=max.edges,]
		}))
	return(	top.edges$tmp.id)
}

#limit to X top edges per node allow > more than max.edges to connect all nodes with strongest relationships
edge.filter2<-function(edge.list,weight,max.edges=1){
	nodes<-unique(fixlc(edge.list))
	id<-fixlc(edge.list[,2])%in%nodes
	#flip source target (expecting undirected edges) to get all of one index on one side
	#test what happens when source nodes are connected
	tmp<-as.data.frame(edge.list)
	#add index
	tmp$tmp.id<-c(1:nrow(tmp))
	tmp$tmp.weight<-weight
	filter<-lapply(1:length(nodes),function(i){
		id<-tmp[,1]%in%nodes[i] | tmp[,2]%in%nodes[i]
		obj<-tmp[id,]
		obj<-obj[order(obj[,3],decreasing=TRUE),]
		obj[c(1:nrow(obj))<=max.edges,]
	})
	unique(do.call("rbind",filter)[,3])
}

