# avoid breaks in R-output print and show JSON packets transferred
# for debugging
options(width = 150, shiny.trace=TRUE)



#load devium functions
#source local directory to load devium fxns 
source.local.dir<-function(wd){
	o.dir<-getwd()
	setwd(wd)
	files<-dir()[unique(c(agrep(".r",dir()),agrep(".R",dir())))]
	lapply(1:length(files),function(i) {tryCatch(source(files[i]),error=function(e){paste0("can't load-->",files[i])})
	})
	setwd(o.dir)
}
#development
# source.local.dir(Sys.glob(file.path("C:","Users","*","Dropbox","Devium","devium","R")))# for local devium fxns
# for App
source.local.dir(paste0(getwd(),"/R")) # app
#global storage objects
values <- reactiveValues()

#load packages
options(repos = c('http://cran.us.r-project.org',"http://cran.rstudio.com/"))
options("BioC_mirror" = "http://www.bioconductor.org")
#options(repos ="http://www.stats.ox.ac.uk/pub/RWin")
libs <- c("rjson","igraph","graph","reshape2","network","sna","Hmisc","ChemmineR","impute","WGCNA",
"ggplot2","jsonlite","RCurl","plyr","d3Network","grid","gridSVG","XML","tools","whisker")
#load all packages
check.get.packages(libs)
#lapply(1:length(libs), function(i) {library(libs[i],character.only = TRUE)})

# #use packrat (FAILS!!!)
# check.get.packages("packrat")
# packrat::restore()



#CTS
#identifier translations (load package)
# if(!require("CTSgetR")){
# install.packages("devtools")
# library(devtools)
# install_github(repo = "CTSgetR", username = "dgrapov")
# library(CTSgetR)
# }

#CTS options
# values$CTS.options<-CTS.options()# web query needs packages
# load("Data/CTS.options")
values$CTS.options<-CTS.options()



#set demo data
#----------------------
# values <- reactiveValues()
TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
# TCA.CID<-CTSgetR(TCA.kegg, from="KEGG", to = "PubChem CID")
TCA.CID <- c("[]","51", "440649","[]", "439161",   "1110",    "972",      "1198",     "970",      "6302",     "222656",   "643757",  "19782904", "1060",     "440568"  ,"[]", "21883788" ,"[]","1005" )
TCA.names<-c('Enzyme N6-(dihydrolipoyl)lysine',	'2-Oxoglutarate',	'3-Carboxy-1-hydroxypropyl-ThPP',	'Enzyme N6-(lipoyl)lysine',	'Succinyl-CoA',	'Succinate',	'Oxalosuccinate',	'Isocitrate',	'Oxaloacetate',	'Acetyl-CoA',	'(S)-Malate',	'cis-Aconitate',	'Citrate',	'Pyruvate',	'2-(alpha-Hydroxyethyl)thiamine diphosphate',	'[Dihydrolipoyllysine-residue succinyltransferase] S-succinyldihydrolipoyllysine',	'Fumarate',	'[Dihydrolipoyllysine-residue acetyltransferase] S-acetyldihydrolipoyllysine',	'Phosphoenolpyruvate')
values[["Citric Acid Cycle"]] <-data.frame(metabolite=TCA.names,KEGG = TCA.kegg, CID = TCA.CID )
# values[["Citric Acid Cycle"]] <-"Citric_Acid_Cycle"
datasets<-"Citric Acid Cycle"

#metabolomics example small data set
values[["metabolomics data"]]<-read.csv("Data/example_data.csv",row.names=1)
datasets<-c("Citric Acid Cycle", "metabolomics data")
values$datasets<-datasets # used to dynamically add data sets

#initialize states
values$clipboard<-""
values$network_state<-""



#alternative busy message
# HTML('<script type="text/javascript">
        # $(document).ready(function() {
          # $("#DownloadButton").click(function() { # where DownloadButton = action button and Download = output$
            # $("#Download").text("Loading...");
          # });
        # });
      # </script>
# ')


#create empty html place holder for network svg
emptyHTML<-function(file){
		htmlhead <- 
			'<!DOCTYPE html>
			<head>
			  <meta charset = "utf-8">
			
			</head>

			<body>
			'
		sink(paste0(file,".html"))
		cat(htmlhead)
		#close our file
		sink(file=NULL)
	}

emptyHTML(file="SVGnetwork")
