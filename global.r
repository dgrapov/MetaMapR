# avoid breaks in R-output print and show JSON packets transferred
# for debugging
options(width = 150, shiny.trace=TRUE)


# options(repos = c("http://cran.rstudio.com/"))
#options(repos ="http://www.stats.ox.ac.uk/pub/RWin")
libs <- c("tools","igraph","reshape2","network","sna","Hmisc","ggplot2","RJSONIO","RCurl")

available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}

#index translations
if(!require("CTSgetR")){
install.packages("devtools")
library(devtools)
install_github(repo = "CTSgetR", username = "dgrapov")
library(CTSgetR)
}

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

#set demo data
#----------------------
values <- reactiveValues()
TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
# TCA.CID<-CTSgetR(TCA.kegg, from="KEGG", to = "PubChem CID")
TCA.CID <- c("[]","51", "440649","[]", "439161",   "1110",    "972",      "1198",     "970",      "6302",     "222656",   "643757",  "19782904", "1060",     "440568"  ,"[]", "21883788" ,"[]","1005" )
TCA.names<-c('Enzyme N6-(dihydrolipoyl)lysine',	'2-Oxoglutarate',	'3-Carboxy-1-hydroxypropyl-ThPP',	'Enzyme N6-(lipoyl)lysine',	'Succinyl-CoA',	'Succinate',	'Oxalosuccinate',	'Isocitrate',	'Oxaloacetate',	'Acetyl-CoA',	'(S)-Malate',	'cis-Aconitate',	'Citrate',	'Pyruvate',	'2-(alpha-Hydroxyethyl)thiamine diphosphate',	'[Dihydrolipoyllysine-residue succinyltransferase] S-succinyldihydrolipoyllysine',	'Fumarate',	'[Dihydrolipoyllysine-residue acetyltransferase] S-acetyldihydrolipoyllysine',	'Phosphoenolpyruvate')
values[["Citric Acid Cycle"]] <-data.frame(metabolite=TCA.names,KEGG = TCA.kegg, CID = TCA.CID )
# values[["Citric Acid Cycle"]] <-"Citric_Acid_Cycle"
datasets<-"Citric Acid Cycle"

#metabolomics example small data set
values[["metabolomics data"]]<-read.csv("example data.csv",row.names=1)

datasets<-c("Citric Acid Cycle", "metabolomics data")

#initialize states
values$clipboard<-""
values$network_state<-""


