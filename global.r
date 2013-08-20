# avoid breaks in R-output print and show JSON packets transferred
# over websockets
options(width = 150, shiny.trace=TRUE)
# options(width = 150)

# #get Devium R-scripts for analysis functions
# source("http://pastebin.com/raw.php?i=JVyTrYRD")
#

#accesory fxn helpful for adding defaults (none = NULL) to sring options
#convert vector to named list
# namel<-function (vec){
		# tmp<-as.list(vec)
		# names(tmp)<-as.character(unlist(vec))
		# tmp
	# }

#all fails above if pastebin is down
#source directly git repo
sourceGitDirectory<-function(url="https://github.com/dgrapov/devium/tree/master/Shiny/Devium/tools/analysis", user="dgrapov"){
	obj<-RCurl::getURL(url, ssl.verifypeer=FALSE)
	#can't use XML::xmlParse(contents) or xmlToList(contents)... so hack
	tmp<-strsplit(obj,'href=\"/')
	tmp2<-unlist(strsplit(as.character(unlist(tmp)),'class'))
	#look for .R scripts
	s1<-agrep("dgrapov",tmp2)
	s2<-agrep(".R/",tmp2)
	keep<-s1[s1%in%s2][-1] # the first entry is always wrong?
	tmp3<-tmp2[keep]
	
	scripts<-paste0("https://raw.github.com/",gsub('\" ',"",tmp3)) # fix formatting
	scripts<-gsub("blob/","",scripts)
	sapply(1:length(scripts),function(i)
		{
			tryCatch( eval( expr = parse( text = RCurl::getURL(scripts[i],
						   ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",scripts[i]))})
		})
	
}

sourceGitDirectory(url="https://github.com/dgrapov/devium/tree/master/R", user="dgrapov") 
#

# options(repos = c("http://cran.rstudio.com/"))
libs <- c("tools","CTSgetR","lsa","igraph","reshape2","network","sna","Hmisc","graph","ggplot2")
 #"RJSONIO", "shiny", "car", "AER", "Ecdat", "foreign", "tools", "ggplot2", 
	#"gridExtra", "reshape2", "plyr", "markdown", "R.utils", "psych", "rela", "arm", "xts")
available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}

# #load CTSgetR for translations
# install.packages("devtools")
# install.packages("RJSONIO")
# library(devtools);library(RJSONIO)#;library(RCurl) 
# install_github(repo = "CTSgetR", username = "dgrapov")
library(CTSgetR)

values <- reactiveValues()
TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
# TCA.CID<-CTSgetR(TCA.kegg, from="KEGG", to = "PubChem CID")
TCA.CID <- c("[]","51", "440649","[]", "439161",   "1110",    "972",      "1198",     "970",      "6302",     "222656",   "643757",  "19782904", "1060",     "440568"  ,"[]", "21883788" ,"[]","1005" )
values[["Citric Acid Cycle"]] <-data.frame(KEGG = TCA.kegg, CID = TCA.CID )
# values[["Citric Acid Cycle"]] <-"Citric_Acid_Cycle"
datasets<-"Citric Acid Cycle"
values$clipboard<-""
values$network_state<-""
# #test data for spectra and correlation
# # setwd("C:/Users/D/Desktop")
# metabolomics.data<-x<-read.csv("example data.csv")
# spectra <- x$mass.spec
# CID.id <-x$PubChem.id
# input<-list()
# input$network_spec_primary_nodes<-known<-x$known
# e<-get.spectral.edge.list(spectra, known = c(1,2), cutoff = 0.7, edge.limit = max(1:length(spectra)))
# data<-x[,10:12]
# type<-input$network_index_type_cor <- "spearman"