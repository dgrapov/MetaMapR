CTSgetR<-function(id,from,to,async=FALSE,limit.values=TRUE,progress=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert"){ 

	opts<-CTS.options()
	if(!to%in%opts|!from%in%opts) {
	
	stop(paste0("The supplied to or from name is not in the list of available options.","\n",
	"Did you mean from = '", opts[agrep(from,opts,ignore.case = TRUE)],"' and to = '",opts[agrep(to,opts,ignore.case = TRUE)],"'?", "\n",
	"See CTS.options() for all available options.","\n"))
	}

	
	
	if(progress) cat("Getting translations...","\n")
	if(async){
		out<-unlist(CTS.translate.async(server=server,from=from,to=to,id=id,...))
		out<-lapply(1:length(out),function(i){out[i]})
	} else{
		out<-CTS.translate(server=server,from=from,to=to,id=id,progress=progress) 
	}
	
	if(limit.values){ 
		parser<-function(x){
			tmp<-fromJSON(x)
			tryCatch(tmp[,"result"]<-strsplit(fixlc(tmp[,"result"]),"/,")[[1]],error=function(e) {NULL})
			return(tmp)
		}
		
		res<-do.call("rbind",lapply(out,parser))
		#format output
		res$result<-sapply(1:length(res[,"result"]),function(i){
				if(length(res[,"result"][[i]])==0) { ""
				} else {
					res[,"result"][[i]]
				}
			})
						
	} else {
		res<-do.call("rbind",lapply(out,fromJSON))
		#format output, could collapse on comma, not sure which is more useful?
		res<-do.call("rbind",lapply(1:length(res[,"result"]),function(i){
				if(length(res[,"result"][[i]])==0) { 
					expand.grid(res[,"searchTerm"][[i]],"")
				} else {
					expand.grid(res[,"searchTerm"][[i]],res[,"result"][[i]])
				}
			}))
		id<-res$Var1
		res$result<-res$Var2 # getting ugly	
			
	}
		
	out<-data.frame(from=id,to=res$result)
	colnames(out)<-c(from,to)	
	return(out)
	
}
	
CTS.translate<-function(server,from,to,id,progress=TRUE){ #arguably parallel, seems more connection stable than asynchronous
		#require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		if(progress) pb <- txtProgressBar(min = 0, max = length(id), style = 3)
		content<-lapply(1:length(id), function(i)
			{
				if(progress) setTxtProgressBar(pb, i)
				readLines(url[i])
			})
		if(progress) close(pb)
		return(content)
}

#asynchronous, need to debug
CTS.translate.async<-function(server,from,to,id,async.limit=100){ 
		require("RCurl")
		# results are returned as JSON encoded strings
		# limit controls the maximum number of request per call to the server
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
	
		options(RCurlOptions = list(verbose = TRUE,
                              followlocation = TRUE,
                              autoreferer = TRUE,
                              nosignal = TRUE))
		curl = getCurlHandle()	
		
		if(is.null(async.limit)){
			getURL(url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl) #not stable missing some args
		} else {
			if(async.limit>=length(url)){async.limit<-length(url)-1}
			cuts<-cut(1:length(url),breaks=seq(1,length(url),length.out=ceiling(length(url)/async.limit)+1),include.lowest = TRUE)
			url.list<-split(url,cuts)
			lapply(1:length(url.list), function(i) {
				tmp.url<-url.list[[i]]
				getURL(tmp.url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl) # unstable for unclear reasons
			} )
		}


}

# get possible translations from CTS
CTS.options<-function(){
		options(warn=-1)	
		url<-readLines("http://cts.fiehnlab.ucdavis.edu/service/convert/toValues")
		fromJSON(url)
	}

#from one to multiple translations
multi.CTSgetR<-function(id, from, to ,...) {
  obj<-lapply(1:length(to),function (i)
  {  
    CTSgetR(id,from,to[i],...)[,2,drop=FALSE]
  })
  tmp<-cbind(id,do.call("cbind",obj))
  colnames(tmp)<-c(from,to)
  tmp  
}

#get InchIKey, KEGG and CID from CTS, KEGG and PubChem from name
NametoKeyID<-function(name){
	#get InchIKey, KEGG and CID from CTS, KEGG and PubChem
	# fill in missing using InChI to key translations via CTS
	# replace KEGG drug with compound
	message(cat("Getting InChIKey \n"))
	keys<-NametoInchI(name)
	keyid<-fixlc(keys$id)
	res<-as.matrix(multi.CTSgetR(id=keyid,from="InChIKey", to=c("KEGG","PubChem CID")))

	#try to get missing IDs for missing InChIKeys
	#KEGG
	get.kegg<-c(1:nrow(res))[res[,"KEGG"]=="error"]
	if(length(get.kegg)>0){
		message(cat("Getting KEGG ID \n"))
		kegg2<-NametoKEGG(name=name[get.kegg])
		res[get.kegg,"KEGG"]<-fixlc(kegg2$KID)
	}
	#CID
	get.cid<-c(1:nrow(res))[res[,"PubChem CID"]=="error"]
	if(length(get.cid)>0){
			message(cat("Getting PubChem CID \n"))
		cid2<-NametoPubChem(name=name[get.cid],ID="cids", limit=TRUE)
		res[get.cid,"PubChem CID"]<-fixlc(cid2$id)
	} 
	
	#try to switch KEGG DRUG IDs to KIDs
	is.D<-c(1:nrow(res))[grep("D",fixlc(res[,"KEGG"]))]
	if(length(is.D)>0){
		message(cat("Translating KEGG DRUG ids \n"))
		kegg3<-NametoKEGG(name=name[is.D])
		res[is.D,"KEGG"]<-fixlc(kegg3$KID)
	}	
	
	return(data.frame(start.name=keys$start.name,matched.name=keys$name,res))
}


#convenience fxns
fixln<-function(obj){as.numeric(as.character(unlist(obj)))}
fixlc<-function(obj){as.character(unlist(obj))}

test<-function(){
	id<-c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
	from<-"KEGG" 
	to<-"PubChem CID"
	CTSgetR(id,from,to)
	
	#asynchronous query of CTS (can be unstable)
	CTSgetR(id,from,to,async=TRUE,async.limit=10)
	
	#multiple translations
	id<-"KPGXRSRHYNQIFN-UHFFFAOYSA-N"
	from<-"InChIKey"
	to<-c("PubChem CID","KEGG")
	multi.CTSgetR(id,from,to)
	
	#get all values for the translation
	id <- c("QNAYBMKLOCPYGJ-REOHCLBHSA-N")
	from <- "Human"
	to <- "Chemical Name"
	CTSgetR(id,from,to,limit.values=FALSE)
	
}	
