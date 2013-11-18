
library(shiny)
runApp("C:\\Users\\D\\Dropbox\\Devium\\devium\\Shiny\\MetaMapR") # home PC
runApp("C:\\Users\\Node\\Dropbox\\Devium\\devium\\Shiny\\MetaMapR") # home laptop
runApp("C:\\Users\\dgrapov\\Dropbox\\Devium\\devium\\Shiny\\MetaMapR") # work



# #test data for spectra and correlation
# # setwd("C:/Users/D/Desktop")
# # setwd("C:\\Users\\Node\\Dropbox\\Devium\\devium\\Shiny\\MetaboMapR")
# # setwd("C:/Users/dgrapov/Dropbox/Devium/devium/Shiny/MetaMapR")
metabolomics.data<-x<-read.csv("example data.csv")
spectra <- x$mass.spec
CID.id <-x$PubChem.id
KEGG
input<-list()
input$network_spec_primary_nodes<-known<-x$known
e<-get.spectral.edge.list(spectra, known = c(1,2), cutoff = 0.7, edge.limit = max(1:length(spectra)))
data<-x[,10:12]
type<-input$network_index_type_cor <- "spearman"

test translations
kegg.id<-CTSgetR(id = fixlc(x$PubChem.id), from="PubChem CID",to="KEGG", async=FALSE,limit.values=FALSE)


#functions
#function to rencode edge.list index 
make.edge.list.index<-function(edge.names, edge.list){
	#need to replace old ids with ne code in multiple edge.lists
	e1<-translate.index(id= matrix(fixlc(edge.list[,1])), lookup = edge.names)
	e2<-translate.index(id= matrix(fixlc(edge.list[,2])), lookup = edge.names)
	data.frame(source = e1, target = e2)
}


#get edge list for testing
x<-TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
names<-CTSgetR(id = fixlc(x), from="KEGG",to="Chemical Name", async=FALSE,limit.values=TRUE)
names<-KEGGtoNames(x)
#
kegg.id<- x
reaction.DB<-get.KEGG.pairs(type="main")
edge.list<-get.Reaction.pairs(kegg.id,reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE)
index<-kegg.id
edge.names<-data.frame(index, network.id = c(1:length(index)))
edge.list<-data.frame(make.edge.list.index(edge.names,edge.list),type="KEGG",weight=2)
node.attributes<-edge.names
node.names<-as.character(node.attributes$index)


