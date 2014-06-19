CTSgetR<-function(id,from,to,parallel=FALSE,async=FALSE,limit.values=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert"){ 
		
		options(warn=-1) 
		
		#get URL contents control the number of urls sent to avoid issues
		message(cat("Getting translations...","\n"))
		if(async){
			tryCatch(out<-CTS.translate.async(server=server,from=from,to=to,id=id) ,error=function(e){stop("Check that CTS server is working")})
		} else{
			out<-CTS.translate(server=server,from=from,to=to,id=id,parallel=parallel) 
		}
		
		#fxn to parse JSON		
		.parseJSON<-function(obj,limit)
			{
					if(any(obj=="[]")) # no value from server
					{
							final<-data.frame(matrix("error",nrow=4))
					} else {
							if(limit==TRUE) #only return first answer
							{ 
								final<-tryCatch(data.frame(RJSONIO::fromJSON(obj))[1,,drop=FALSE],error=function(e){data.frame(matrix("error",ncol=4))})
							} else {
								final<-tryCatch(data.frame(RJSONIO::fromJSON(obj)),error=function(e){data.frame(matrix("error",ncol=4))})
								if(nrow(final)>1) # combine multiple answers in a comma separated string
									{
										tmp<-paste(as.matrix(final[,4,drop=F]),collapse=",")
										final[,4]<-tmp
										final<-final[1,,drop=FALSE]
									}
							}
					}
				colnames(final)<-""
				return(final)	
			}
			
		message(cat("\n","Formatting output...","\n"))
		if(parallel==TRUE){ # add progress bar
				
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				out<-foreach(i=c(1:length(out))) %dopar% .parseJSON(out[i],limit=limit.values)
				stopCluster(cl.tmp)	
		} else {
				 
				out<-lapply(1:length(out),function(i,pb = txtProgressBar(min = 0, max = length(id), style = 3)){
					setTxtProgressBar(pb, i)
					.parseJSON(obj=out[i],limit=limit.values)})
			}
			
		#parse  into a triple
		tmp<-do.call("rbind",out)
		tmp<-as.data.frame(cbind(from=id,result=as.character(unlist(tmp[,4,drop=FALSE]))))
		colnames(tmp)<-c(from,to)
		return(data.frame(tmp))
	}

CTS.translate<-function(server,from,to,id,parallel=FALSE){ #arguably parallel, seems more connection stable than asynchronous
		#require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
		if(parallel==TRUE){ # not clear if actuall improves speed, why RCurls was tried first
				library(snow);library(doSNOW);library(foreach)
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				content<-as.character(unlist(foreach(i=c(1:length(id))) %dopar% readLines(url[i])))
				stopCluster(cl.tmp)		
				
		} else {
			content<-as.character(unlist(sapply(1:length(id), function(i, pb = txtProgressBar(min = 0, max = length(id), style = 3))
				{
					setTxtProgressBar(pb, i)
					tryCatch(readLines(url[i]),error=function(e){"error"})
				})))
		}
			return(content)
}

#asynchronous
CTS.translate.async<-function(server,from,to,id){ # sometimes will not work due to loss of connection when webservices are overwhelemed
		require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
		# options(RCurlOptions = list(verbose = TRUE,
                              # followlocation = TRUE,
                              # autoreferer = TRUE,
                              # nosignal = TRUE))
		curl = getCurlHandle()					  				  
		x<-getURL(url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl, followlocation = TRUE) #not stable missing some ar
		# issue on OSX with R locking up

		options(RCurlOptions = list(verbose = TRUE,
                              followlocation = TRUE,
                              autoreferer = TRUE,
                              nosignal = TRUE))
		curl = getCurlHandle()					  				  
		getURL(url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl) #not stable missing some args
		# con = multiTextGatherer(url)
		# getURIAsynchronous(url, write = con)

}

# get possible translations from CTS
CTS.options<-function(){
	options(warn=-1)	
	url<-readLines("http://cts.fiehnlab.ucdavis.edu/service/convert/toValues")
	RJSONIO::fromJSON(url)
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

# #test
# id=c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
# from="KEGG" 
# to="KEGG"
# multi.CTSgetR(id,from,to)
# CTSgetR(id,to,from)
