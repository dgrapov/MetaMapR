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
source.local.dir(Sys.glob(file.path("C:","Users","*","Dropbox","Devium","devium","R")))# for local devium fxns
# for App
# source.local.dir(paste0(getwd(),"/R")) # app
#global storage objects
values <- reactiveValues()



# options(repos = c("http://cran.rstudio.com/"))
#options(repos ="http://www.stats.ox.ac.uk/pub/RWin")
libs <- c("tools","igraph","graph","reshape2","network","sna","Hmisc","ggplot2","jsonlite","RCurl","plyr","d3Network")
#load all packages
check.get.packages(libs)

#CTS
#identifier translations (load package)
# if(!require("CTSgetR")){
# install.packages("devtools")
# library(devtools)
# install_github(repo = "CTSgetR", username = "dgrapov")
# library(CTSgetR)
# }

#for some people loading CTSgetR causes errors
# bundle all the code with MetaMapR until CTSgetR is on Cran
source("CTS.R")
#options for translations
# CTS.options<-CTS.options()
# save(CTS.options,file="CTS.options")
values$CTS.options<-CTS.options()


#source directly git repo (should also bundle with the app)...
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

			scripts<-paste0("https://raw.githubusercontent.com/",gsub('\" ',"",tmp3)) # fix formatting
			scripts<-gsub("blob/","",scripts)
			sapply(1:length(scripts),function(i)
				{
					tryCatch( eval( expr = parse( text = RCurl::getURL(scripts[i],
								   ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",scripts[i]))})
				})

		}

tryCatch(sourceGitDirectory(url="https://github.com/dgrapov/devium/tree/master/R", user="dgrapov"), error=function(e){message("Can't access")}) 

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
values[["metabolomics data"]]<-read.csv("example data.csv",row.names=1)
datasets<-c("Citric Acid Cycle", "metabolomics data")
values$datasets<-datasets # used to dynamically add data sets

#initialize states
values$clipboard<-""
values$network_state<-""


#options for translations
# CTS.options<-CTS.options()
# save(CTS.options,file="CTS.options")
values$CTS.options<-CTS.options()



#alternative busy message
# HTML('<script type="text/javascript">
        # $(document).ready(function() {
          # $("#DownloadButton").click(function() { # where DownloadButton = action button and Download = output$
            # $("#Download").text("Loading...");
          # });
        # });
      # </script>
# ')

