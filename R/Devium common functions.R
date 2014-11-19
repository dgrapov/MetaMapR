
#list to character
fixlc<-function(obj){as.character(unlist(obj))}

#list to numeric
fixln<-function(obj){as.numeric(as.character(unlist(obj)))} # should encode text as factors firstƒ

#factor to numeric
fixlf<-function(obj){as.numeric(as.factor(unlist(obj)))} # should encode text as factors first

#convert all columns of a data.frame or matrix to numeric (encoding characters)
afixln<-function(a,keep.factors=FALSE){
	obj<-do.call("cbind",lapply(1:ncol(a),function(i){
				tmp<-a[,i]
				if(is.factor(tmp)|is.character(tmp)){
					as.numeric(as.factor(tmp))
				} else {
					tmp
				}
			})
		)	
		#maintain object class and dimnames
		pclass<-paste0("as.",class(a))
		obj<-do.call(pclass,list(obj))
		dimnames(obj)<-dimnames(a)
		return(obj)
}


#convert all columns of a data.frame or matrix to numeric (encoding characters) optional preserving factors
afixlnf<-function(a,factors=TRUE){
		
		tmp<-a
		is.num<-sapply(1:ncol(a),function(i){all(is.na(as.numeric(as.character(a[,i]))))})
		tmp<-data.frame(do.call("cbind",lapply(tmp,function(i) {as.numeric(as.character(i))}))) # make sure dims are maintained
		if(factors){
			tmp2<-a[,is.num,drop=FALSE]
			tmp[,is.num]<-data.frame(do.call("cbind",lapply(1:ncol(tmp2),function(i) {data.frame(factor(tmp2[,i]))})))
		} else {
			tmp[,is.num]<-data.frame(sapply(a[,is.num,drop=FALSE],function(i) {as.numeric(factor(i))}))
		}
			
		#maintain object class and dimnames
		pclass<-paste0("as.",class(a))
		obj<-do.call(pclass,list(tmp))
		dimnames(obj)<-dimnames(a)
		return(obj)
}

#convert all columns of a data.frame or matrix to character 
afixlc<-function(a){
	obj<-do.call("cbind",lapply(1:ncol(a),function(i){
				fixlc(as.factor(a[,i]))
			})				
		)	
		#maintain object class and dimnames
		pclass<-paste0("as.",class(a))
		obj<-do.call(pclass,list(obj))
		dimnames(obj)<-dimnames(a)
		return(obj)
}

#check data.frame and remove or return factor or character columns
fixlr<-function(a,.remove=TRUE){
	id<-sapply(a,is.character)|sapply(a,is.factor)
	if(.remove){a[,!id,drop=FALSE]} else {a[,id,drop=FALSE]}
}

#transpose data and fix numeric/factors
fixlt<-function(obj) {
	tmp<-t(obj)
	#check what can be numeric
	# fct<-apply(apply(apply(tmp,2,as.numeric),2,is.na),2,all) # a better way must exist?
	fct<-sapply(1:ncol(tmp),function(i){all(is.na(as.numeric(tmp[,i])))})

	#set everything else to factor
	tmp2<-as.data.frame(apply(sapply(data.frame(tmp),as.character),2,as.numeric))
	tmp2[,fct]<-data.frame(tmp[,fct,drop=FALSE])
	return(tmp2)
}

#remove unused levels in factors
fixfactors<-function(obj) {
	#check what can be numeric
	# fct<-apply(apply(apply(tmp,2,as.numeric),2,is.na),2,all) # a better way must exist?
	fct<-sapply(obj,is.factor)
	tmp<-obj[,fct,drop=FALSE]
	#set everything else to factor
	tmp2<-data.frame(apply(sapply(data.frame(tmp),as.character),2,as.factor))
	obj[,fct]<-data.frame(tmp2)
	return(obj)
}


#import from clipboard
read.excel <- function(type="with.dimnames") {
						  #assume row and colnames are included but may not be unique
						  tmp<-read.table("clipboard-128",sep="\t")
						  devium.data.format(tmp, type)
						}

#write to clipboard 
write.to.clipboard<-function(obj,type="no.name"){

	#obj = to be written to clipboard
	#type determines dimensions and their names
	
	switch(type,
	with.dimnames = .local<-function(obj)
							{
								obj.names<-list(rownames = rownames(obj), colnames = colnames(obj))
								obj<-obj
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
							
		with.name = .local<-function(obj)
							{
								obj.names<-list(1:((length(as.character(unlist(obj))))-1),as.character(unlist(obj)[1]))
								obj<-as.data.frame(as.character(unlist(obj))[-1])
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
		no.name = .local<-function(obj)
		{
			obj.names<-list(1:((length(as.character(unlist(obj))))),"NA")
			obj<-as.data.frame(as.character(unlist(obj)))
			is.error<-NULL
			is.error<-tryCatch(dimnames(obj)<-obj.names,
			error=function(e)
					{ 
						NULL
					})#need to set as protected 
			if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
		})
		
	tmp<-.local(obj) 
	#bind row names and col names with obj
	tmp.obj<-cbind(c("",rownames(tmp)),rbind(colnames(tmp),tmp))
	write.table(as.matrix(tmp.obj),"clipboard-128",sep="\t",row.names=FALSE,col.names=FALSE)
}

#format data objects: identify headers and meta data, and give sepearately 
devium.data.format<-function(obj, type){
	#type can be with.dimnames
	# with.meta.data
	
	switch(type,
		with.dimnames = .local<-function(obj)
							{
								obj.names<-list(rownames = as.character(unlist(obj[-1,1])), colnames = as.character(unlist(obj[1,-1])))
								#obj<-as.data.frame(matrix(as.numeric(as.character(unlist(obj[-1,-1]))),nrow=length(obj.names$rownames), ncol=length(obj.names$colnames))) # as.numeric
								obj<-as.data.frame(matrix(as.character(unlist(obj[-1,-1])),nrow=length(obj.names$rownames), ncol=length(obj.names$colnames)))
								
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
							
		with.name = .local<-function(obj)
							{
								obj.names<-list(1:((length(as.character(unlist(obj))))-1),as.character(unlist(obj)[1]))
								obj<-as.data.frame(as.character(unlist(obj))[-1])
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
		no.name = .local<-function(obj)
		{
			obj.names<-list(1:((length(as.character(unlist(obj))))),"NA")
			obj<-as.data.frame(as.character(unlist(obj)))
			is.error<-NULL
			is.error<-tryCatch(dimnames(obj)<-obj.names,
			error=function(e)
					{ 
						NULL
					})#need to set as protected 
			if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
		},
		with.meta.data = .local<-function(obj)
							{
							#look for "" and set headers based on first non "" row and column (add other patterns here)
							object<-obj
							
							#identify boundaries of data the rest is meta data
							sample.start<-max(which(object[1,]=="")+2)
							variable.start<-max(which(object[,1]=="")+2)

							#data
							drows<-nrow(object)
							dcols<-max(c(1:ncol(object))[!sapply(1:ncol(object),function(i){sum(is.na(object[,i]))==drows})])
							tmp.data<-as.data.frame(as.matrix(object[c(variable.start:drows),c(sample.start:dcols)]))
							.names<-dimnames(tmp.data)
							
							#make sure all factors converted to numeric 
							tmp.data<-as.data.frame(matrix(as.numeric(as.character(unlist(tmp.data))),nrow=nrow(tmp.data), ncol=ncol(tmp.data)))
							dimnames(tmp.data)<-.names
							
							#get variable and sample meta data
							row.meta<-as.data.frame(t(as.matrix(object[c(1:(variable.start-2)),c(sample.start:dcols)])))
							col.meta<-as.data.frame(as.matrix(object[c(variable.start:drows),c(1:(sample.start-1))]))
							
							#attempt to set colnames
							tryCatch(colnames(row.meta)<-as.character(unlist(obj[1:(variable.start-2),(sample.start-1)])),errro=function(e){})
							tryCatch(colnames(col.meta)<-as.character(unlist(obj[(variable.start-1),1:(sample.start-1)])),errro=function(e){})
							
							
							#convert data to all numeric prior to return
							data<-do.call("cbind",unclass(tmp.data));dimnames(tmp.data)<-.names # need to break factors

							#return results as a list
							list(data=tmp.data,row.metadata=row.meta,col.metadata=col.meta)
							})					
	.local(obj)
		
}

#format binbase output (data with meta data structure)
format.binbase.output<-function(data){

		#data = name as string
		object<-get(data)#need to adjust for first row taken as column names
		#format data object
		#use row 1 and column 1 to figure out meta data space
		row.id<-max(c(1:ncol(object))[is.na(object[1,])|object[1,]==""])
		col.id<-max(c(1:nrow(object))[is.na(object[,1])|object[,1]==""])
		
		# sample.start<-which(colnames(object)=="file.id")+1
		# variable.start<-which(object[,1]=="BinBase name")+1
	
		sample.start<-row.id+2
		variable.start<-col.id+2

		#data
		drows<-nrow(object)
		dcols<-max(c(1:ncol(object))[!sapply(1:ncol(object),function(i){sum(is.na(object[,i]))==drows})])
		tmp.data<-as.data.frame(t(as.matrix(object[c(variable.start:drows),c(sample.start:dcols)])))
		row.names<-rownames(tmp.data)
		#make sure all factors are converted to numeric 
		tmp.data<-as.data.frame(matrix(as.numeric(as.character(unlist(tmp.data))),nrow=nrow(tmp.data), ncol=ncol(tmp.data)))
		tmp.data<-do.call("cbind",unclass(tmp.data)) 
		dimnames(tmp.data)<-list(1:nrow(tmp.data),1:ncol(tmp.data))
		
		#get variable and sample meta data
		#row meta (everything is transposed ... bad)
		row.meta<-as.data.frame(t(as.matrix(object[c(1:(variable.start-2)),c(sample.start:dcols)]))) # skip empty line above data
		colnames(row.meta)<-object[c(1:(variable.start-2)),sample.start-1]
		
		#column meta
		col.meta<-as.data.frame(as.matrix(object[c(variable.start:drows),c(1:(sample.start-1))]))
		# colnames(col.meta)<-object[row.id,1:(variable.start)] 
		colnames(col.meta)<-object[variable.start-1,1:sample.start-1]# why does above work some times?
		
		#convert data to all numeric prior to return
		# need to break factors
		
		#return results as a list
		list(data=data.frame(tmp.data),row.metadata=data.frame(row.meta),col.metadata=data.frame(col.meta))
	}

#return objects to excel	
return.to.Excel<-function(workbook.path="new",return.obj.list,return.name=names(return.obj.list),workbook.name=NULL)
	{
		check.get.packages(c("XLConnect"))
		
		#load workbok
		if (!workbook.path=="new")
			{
				#connect to worksheet
				old.dir<-getwd()
				wd<-dirname(workbook.path)
				workbook.name<-basename(workbook.path)
				setwd(wd)
				wb = loadWorkbook(workbook.name, create = FALSE)
				
			}else{
			
				if(is.null(workbook.name)){workbook.name<-paste('Workbook',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"),"xls", sep = ".")}
				wb = loadWorkbook(workbook.name, create = TRUE)
			}
			
		#place objects in workbook
		sapply(1:length(return.name),function(i)
			{
				obj.name<-rename(x=return.name[i],pattern=c(" ","\\$"))
				#create sheet
				createSheet(wb, name = obj.name)
				
				#delete name is if exists
				tryCatch(removeName(wb,obj.name),error=function(e){})
				createName(wb, name = obj.name, formula = paste(obj.name, "$A$1", sep = "!"))
				
				#bind rownames else not visible
				return<-cbind(rownames(return.obj.list[[i]]),return.obj.list[[i]]);colnames(return)[1]<-"Variables"
				writeNamedRegion(wb, data = return, name = obj.name, header = TRUE)
				
				#add excel auto filters need to avoid text and mean +/- stdev column else all all broken
				corners = getReferenceCoordinates(wb, obj.name)
				#don't include first column of variable names
				corners[1,2]<-(corners[2,2]-(corners[2,2]-2))
				corners[2,2]<-corners[2,2]-1
				setAutoFilter(wb, sheet = obj.name, reference = aref(corners[1,], corners[2,]))
			})
			
			saveWorkbook(wb)
	}

#functions to calculate ranges for excel placement
list.placement.full<-function(data.list,list.names,direction,start.col,start.row,spacer){
	set.1<-list.object.dim.full(data.list,list.names)
	col.i<-matrix(paste(rbind(matrix(LETTERS,ncol=1),matrix(paste(rep(LETTERS,each=length(LETTERS)),rep(LETTERS,length(LETTERS)),sep=""),ncol=1)),"$",sep=""))
	row.i<-matrix(1:1e6,ncol=1)
	place.row<-matrix()
	place.col<-matrix()
	place.range<-matrix()
	columns<-matrix()
	rows<-matrix()
	n<-dim(set.1)[1]
	i<-1
		for(i in 1:n){
			place.row[i]<-start.row+sum(unlist(set.1[1:i,3]))+spacer*(i-1)-unlist(set.1[i,3])
			place.col[i]<-col.i[start.col+sum(unlist(set.1[1:(i),2]))+spacer*(i-1)-unlist(set.1[i,2])]
			if(direction=="vertical"){
			place.range[i]<-matrix(paste(col.i[start.col],place.row[i],sep=""),ncol=1)} else{
			if(direction=="horizontal"){
			place.range[i]<-matrix(paste(place.col[i],start.row,sep=""),ncol=1)}}}
			ex.range<-as.data.frame(cbind(set.1,place.range))
			ex.range}
			
# accesory fxn to list.placement.full
list.object.dim.full<-function(data.list,list.names)
	{
		l.dim<-list()
		n<-length(data.list)
		i<-1
		for(i in 1:n)
		{
		tmp.list<-as.data.frame(data.list[[i]])
		height<-dim(tmp.list)[1]
		width<-dim(as.data.frame(tmp.list))[2]
		l.dim[[i]]<-as.data.frame(matrix(cbind(width,height),ncol=2))
		}
			out<-do.call("rbind",l.dim)
			out<-cbind(list.names,out)
			colnames(out)<-c("objects","width","height")
			out
	}

#write object to an excel sheet based on positional location of top left corner
multi.object.XL<-function(workbook.path="new",placement.list,workbook.name=NULL,sheet.name="Output")
	{
		check.get.packages(c("XLConnect"))
		
		#load workbok
		if (!workbook.path=="new")
			{
				#connect to worksheet
				old.dir<-getwd()
				wd<-dirname(workbook.path)
				workbook.name<-basename(workbook.path)
				setwd(wd)
				wb = loadWorkbook(workbook.name, create = FALSE)
				
			}else{
			
				if(is.null(workbook.name)){workbook.name<-paste('Workbook',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"),"xls", sep = ".")}
				wb = loadWorkbook(workbook.name, create = TRUE)
			}
		
		#create worksheet
		createSheet(wb, name = sheet.name)
		
		#create named regions and return objects
		sapply(1:nrow(placement.list),function(i)
			{
				print(i)
				name<-fixlc(placement.list[i,1])
				#delete name is if exists
				tryCatch(removeName(wb,name),error=function(e){})
				#fix formula for colukn position
				name.location<-paste(sheet.name,paste("$",unlist(placement.list[i,4]),sep=""),sep="!") # too lazy too rewrite depnedancy must accept uglness for now
				createName(wb, name = name, formula = name.location)
				
				#bind rownames else not visible
				return<-get(name)
				writeNamedRegion(wb, data = return, name = name, header = TRUE)
				
			})
			
			saveWorkbook(wb)
	}

#get object from EXCEL	
get.from.Excel <- function(workbook.path=NULL,get.object.sheet=NULL,get.obj.name=NULL,return=TRUE,environment=.GlobalEnv)
				{
					check.get.packages(c("XLConnect"))
					old.dir<-getwd() # original working directory

					#check to see if workbook exists
					get.workbook<-function(workbook.path)
						{
							wd<-dirname(workbook.path)
							setwd(wd)
							loadWorkbook(basename(workbook.path))
						}
					cat("Loading workbook","\n")	
					workbook<-tryCatch(get.workbook(workbook.path), error=function(e) {NULL})	
					
					if (is.null(workbook) )
						{
							return(cat("Could not load workbook","\n"))
						} else {
								#check if named range exists
								obj<-tryCatch(readNamedRegion(workbook, name =  get.obj.name , header = FALSE),error= function(e) {NULL})
								
								if(is.null(obj))
									{
										#check if worksheet exists
										obj<-tryCatch(readWorksheet(workbook, sheet = get.object.sheet),error=function(e) {NULL})
										if(is.null(obj))
											{
												return(cat("the object can't be loaded", "\n"))
											} else {
													#assign sheet to environment
													tmp.obj<-get.object.sheet
											}
									} else {
											#assign named range to environment
											tmp.obj<-get.obj.name
									}

						}
					setwd(old.dir)
					#return obj
					#assign
					assign(tmp.obj,obj,envir=environment)
					if(return){get(tmp.obj,envir=environment)}
}						
				
#collapse columns as strings (why am I not using paste collapse?)
join.columns<-function(obj=formatted$row.metadata[,4:6],char="|",quote.last=FALSE)
{
		
				if(class(obj)=="list"){obj<-as.matrix(obj[[1]])} else {obj<-as.matrix(obj)}
				#used to binf 2 columns try to generalize to more
				.local<-function(obj,char,quote.last)
						{
						
						if(length(obj)==0)
								{
										return(NULL)
										}else{
												if(ncol(as.matrix(obj))>=2)
														{
																		n<-ncol(obj)
																		out<-data.frame()
																		sobj<-obj
																		i<-1
																		for(i in 1:(n-1))
																		{
																						   if(quote.last==TRUE)
																						{
																												sobj[,i]<-paste(as.character(obj[,i]),paste("'",as.character(obj[,i+1]),"'",sep=""),sep=char)
																						} else {
																												sobj[,i]<-paste(as.character(obj[,i]),as.character(obj[,i+1]),sep=char)
																						}
																		}
																		sobj[,-n]
														}else{
																		obj
														}
								}
				}
		if (ncol(obj)>2)
				{
						tmp<-obj[,1:2]
						for(i in 1:(ncol(obj)-1)) {
								tmp2<-.local(tmp, char=char,quote.last=quote.last)
								if(i < (ncol(obj)-1)){
									tmp<-cbind(tmp2,obj[,(i+2),drop=FALSE])
								}
						}
						tmp2
						
				} else {
						.local(obj=obj,char=char,quote.last=quote.last)
				}
}

#accesory function to return position of first instance of unique object 
unique.id<-function(obj)
		{
			tmp<-as.factor(obj)
			id<-seq(along=obj)
			sapply(1:nlevels(tmp),function(i)
				{
					id[tmp==levels(tmp)[i]][1]
				})
		}

#function to check for packages and attempt to download if not found
check.get.packages<-function(pkg)
	{
		options(warn=-1)
		
		# #make sure bio conductor is one of the repositories
		# #will need a mechanism to make sure this stays upto date
		# if(!all(names(getOption("repos"))%in%"BioCsoft"))
			# {
				# r<-getOption("repos")
				# r["BioCsoft"]<-"http://www.bioconductor.org/packages/2.11/bioc" # needs to be specific version
				# options(repos = r)
				
				# #add layer to call 
				 # #source("http://bioconductor.org/biocLite.R")
				 # #biocLite("package to install")
				 
				 # if install fails
				# #r.version<-paste(sessionInfo()$R.version$major,sessionInfo()$R.version$minor, sep=".")
				# #bioc.url<-paste("http://www.bioconductor.org/packages/",r.version,"/bioc", sep="")
				# #r["BioCsoft"]<-bioc.url # needs to be specific to R version
				# #options(repos = r)
			# }	
		
		res<-character()
		
		need<-as.matrix(sapply(1:length(pkg),function(i)
			{
				
				if(require(pkg[i],character.only = TRUE)==FALSE)
					{
					 res<-c(res,pkg[i])
					}
			}))
			
			need<-as.character(unlist(need[!is.null(need)]))
			if(length(need)>0)
				{
					
					x<-sapply(need,install.packages,dependencies = TRUE)
					lib.fun<-function(need){
							sapply(1:length(need), function(i){
							out<-tryCatch(library(need[i], character.only= TRUE), error=function(e){need[i]})
							if(all(out==need[i])){need[i]}
							})
						}
						
					out<-as.character(unlist(lib.fun(need)))
					
					#try bioconductor
					if(length(out)>0){
					cat(paste("Package not found, trying Bioconductor..."),"\n")
					source("http://bioconductor.org/biocLite.R")
					lib.fun.bioc<-function(need){
							sapply(1:length(need), function(i){
							tryCatch( biocLite(need[i],ask=FAlSE), 
								error=function(e){need[i]})
							})
						}
						
					tmp<-lib.fun.bioc(out)
					final<-lib.fun(tmp)
					if(length(final)>0){cat(paste("could not find package: ",paste(as.character(unlist(final)),collapse=", "),sep=""),"\n")}
					}
					
				}

	}

#function to extract objects based on reference
extract.on.index<-function(database,index=database[,1,drop=FALSE],what,extract.on="row")
	{		
		# the merge function should be used instead
		if(extract.on=="col"){database<-t(database)}
		#assume top row are column names
		col.names<-database[1,]
		
		#first column of what and database are the index for extractions
		# cbind objects based on matching index values
		index.w<-as.character(what)
		ref<-as.data.frame(index)
		
		out<-lapply(1:ncol(ref),function(j)
			{
				index.d<-as.character(ref[,j])#as.character(database[,1])
				index.w<-index.w[index.w%in%index.d]
				tmp.database<-database
				out<-matrix(NA,length(index.w),ncol(tmp.database),1)
				e.index<-c(1:nrow(database))
				i<-1
				for(i in 1:length(index.w))
				{
					if(any(index.w[i]%in%index.d))
					{
						ext<-unique(e.index[index.w[i]==index.d])[1]
						out[i,]<-tmp.database[ext,]
					}
			
				}

				# placing rownames in a column to avoid duplicate error
				out<-as.matrix(cbind(index.w,out))
				
				if(extract.on=="col")
					{
						out<-cbind(col.names,as.data.frame(t(out)))
						#fix column names
						col.names<-as.character(unlist(out[1,]))
						out<-out[-1,]
						colnames(out)<-col.names
					}else{
						out<-as.data.frame(out)
						colnames(out)<-col.names
					}
			})
		return(out)
	}

#check object value or set default on condition
if.or<-function(object,if.value=NULL,default,environment=devium)
	{
		obj<-tryCatch(svalue(get(object,envir=environment)),error=function(e){NA})
		if(!any(obj%in%c(if.value,NA))){return(obj)}else{return(default)}
	}

#get unassigned variables from within data frame
gget<-function(obj)
{
	#break obj on $ 
	# [1] = data frame
	# [2] = variable name
	# return object
	tmp<-unlist(strsplit(obj,"\\$"))
	if(!length(tmp)==0) get(tmp[1])[,tmp[2]] else NULL
}
		
#function to connect to google docs
# GetGoogleDoc<-function(account,password,connection="new")
# 	{
# 		#returns list 
# 		# [1] = connection name
# 		# [2] = names of documents
# 		#  connection = as.character connection name if already made using this function 
# 		# and stored in the envir= googDocs
# 		
# 		#install RGoogleDocs if not available
# 		if(require("RGoogleDocs")==FALSE)
# 			{
# 				install.packages("RGoogleDocs", repos = "http://www.omegahat.org/R", type="source")
# 				library("RGoogleDocs")
# 			}
# 			
# 		if(connection == "new")
# 			{
# 					#make time stampped name for connection
# 					con.name<-con.name.txt<-paste('connection',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"), sep = ".")
# 				
# 					#set options to avoid ssl error 
# 					options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
# 					
# 					#assign to new envir
# 					assign("googDocs",new.env(),envir=.GlobalEnv)
# 					assign(con.name,getGoogleDocsConnection(getGoogleAuth(account, password, service ="wise")), envir = googDocs )	
# 			} else {
# 					con.name<-con.name.txt<-connection
# 					}		
# 					
# 		docs<-getDocs(tryCatch(get(con.name,envir=googDocs),error=function(e){stop(paste(connection, " does not exist","\n"))}))
# 		dnames<-names(docs)
# 		return(list(connection = con.name.txt , names = dnames))
# 	}

#function to view excel objects	
viewExcelObject<-function(obj.path)
	{
		#connect to file and view: 
		#worksheet names
		#named ranges
	
		if(require("XLConnect")==FALSE)
			{
				install.packages("XLConnect")
				library("XLConnect")
			}
		
		#load workbook
		old.dir<-getwd()
		wd<-dirname(obj.path)
		workbook<-basename(obj.path)
		setwd(wd)
		wb = loadWorkbook(workbook, create = FALSE)
		
		#get sheet names
		all.worksheets<-getSheets(wb)
		
		#get all valid named ranges
		all.named.ranges<-getDefinedNames(wb, validOnly=TRUE)
		setwd(old.dir)
		return(list(worksheets=all.worksheets,named.ranges=all.named.ranges))
1	}

#accesory functions based/from package pmg
#----------------------------------------------------
Paste<-function (..., sep = "", collapse = "") 
	{
		x = unlist(list(...))
		x = x[!is.na(x)]
		x = x[x != "NA"]
		paste(x, sep = sep, collapse = collapse)
	}

is.gWindow<-function (obj) 
	{
		is(obj, "gWindowRGtk")
	}

rpel<-function (string, envir = .GlobalEnv) 
	{
		eval(parse(text = string), envir = envir)
	}
	
#function to get gwidget svalues for assigned widgets
d.get<-function(object, main.object="devium.pca.object",envir=devium)
	{
		#check to see if main object exists else create
		sapply(1:length(object),function(i)
			{
				tmp<-svalue(get(object[i],envir=envir))
				d.assign(add.obj=object[i],value=tmp,main.object,envir=envir)
			})
	}
	
#function to get gwidget svalues for assigned widgets
#main.object and its envir as string
check.get.obj<-function(object, main.object="devium.pca.object",envir="devium")
	{
		#check to see if main object exists else create
		check.get.envir(main.object,envir)
		env<-get(envir)
		sapply(1:length(object),function(i)
			{
				tmp<-svalue(get(object[i],envir=get(envir)))
				d.assign(add.obj=object[i],value=tmp,main.object,envir=get(envir))
			})
	}	

#check to see if environment exists else create one
check.get.envir<-function(main.object,envir)
	{
				if(!exists(envir)){ assign(envir,new.env(),envir= .GlobalEnv)} 
				if(!exists(main.object,envir=get(envir))){assign(main.object,list(),envir=get(envir))}
	}
	
 #fxn to create the environment "devium" if it does not exist
 create.devium.env<-function()
	{
		if(!exists("devium"))
				{
				if(!is.environment("devium")){ assign("devium",new.env(),envir= .GlobalEnv)}
				
				#check for devium objects and set to null if they don't exist
				for (i in c("devium.helpBrowser.window", "devium.plotnotebook.window")) 
					{
					if(!exists(i))
						{
							assign(i, NULL, envir = devium)
						}
					}
				}
	}	
	  
 #function to make assignments to storage object
 d.assign<-function(add.obj,value,main.object,envir=devium) #main.object="devium.pca.object"
	{
		.local<-function()
			{
				tmp<-get(main.object,envir=envir)
				tmp[[add.obj]]<-value
				assign(get("main.object"),tmp,envir=devium)
			}
			tryCatch(.local(),error=function(e){})
	}
	
#from plyr: get as text
.<-function (..., .env = parent.frame()) 
{
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}

#fix character symbols
check.fix.names<-function(names,ok.chars=c(".","_",",","(",")",":"," "),replace.with="."){
				obj<-as.list(names)
				fixlc(lapply(1:length(obj), function(i)
				{
				#check and replace bad (not allowed as range names in Excel) characters
					tmp<-unlist(strsplit(as.character(obj[i]),""))
					check.index<-c(1:length(tmp))
					OK.char<-c(LETTERS,letters,ok.chars)
					replace.index<-check.index[!tmp%in%OK.char]
					is.number<-(check.index)[!is.na(tmp==fixln(tmp))]
					if(length(is.number)==0){is.number<-0}
					replace<-replace.index[!replace.index%in%is.number]
					tmp[replace]<-replace.with
					tmp<-paste(tmp,collapse="")
					obj<-paste(fixlc(strsplit(tmp," ")),collapse=replace.with)
				}))
			}
			
#get top triangle of possible pairwise interactions
#accessory function
all.pairs<-function(r,type="one"){       
				switch(type,
				one = list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))], second = rep(1:r, r)[lower.tri(diag(r))]),
				two = list(first = rep(1:r, r)[lower.tri(diag(r))], second = rep(1:r,rep(r,r))[lower.tri(diag(r))]))
}

#source local directory to load devium fxns
source.local.dir<-function(wd){
	o.dir<-getwd()
	setwd(wd)
	files<-dir()[unique(c(agrep(".r",dir()),agrep(".R",dir())))]
	lapply(1:length(files),function(i) {tryCatch(source(files[i]),error=function(e){paste0("can't load-->",files[i])})
	})
	setwd(o.dir)
}

##merge two data sets based on a common index column
## all unique levels of the index are preserved in the final object
## i.e. no variables are dropped
merge.data.cube<-function(data1,data2,ID="merge.ID",col.meta="col.metadata",row.meta="row.metadata"){
	# do full outer join on data
	# base dimnames for merge on ID name in row- and col meta data
	# do similar merge on the meta data
	
	#need to make a simple common colname ID between which is shared between the two data sets
	full<-unique(c(data1[[col.meta]][ ,ID],data2[[col.meta]][ ,ID]))
	tmp.id<-data.frame(id=1:length(full))
	rownames(tmp.id)<-full
	tmp.id2<-data.frame(id=full) #to resort columns
	
	#recode dimensions of both data sets based on row and col id
	df1<-data.frame(data1$data)
	dimnames(df1)<-list(data1[[row.meta]][ ,ID],tmp.id[data1[[col.meta]][ ,ID],])
	df2<-data.frame(data2$data)
	dimnames(df2)<-list(data2[[row.meta]][ ,ID],tmp.id[data2[[col.meta]][ ,ID],])
	
	#add row ID to make sure row meta is in the correct order
	df1$._merge_tmp_ID<-make.unique(fixlc(data1[[row.meta]][ ,ID]))
	df2$._merge_tmp_ID<-make.unique(fixlc(data2[[row.meta]][ ,ID]))
	
	#full outer join on the data
	merged.data<-merge(df1,df2,all=TRUE,sort=FALSE)
	id<-colnames(merged.data)%in%"._merge_tmp_ID"
	row.id<-merged.data[,id]
	merged.data<-merged.data[,!id]
	
	#row meta
	.row.meta<-merge(data1[[row.meta]],data2[[row.meta]],all=TRUE,sort=FALSE)
	rownames(.row.meta)<-make.unique(fixlc(.row.meta[,ID]))
	.row.meta<-.row.meta[row.id,]
	
	
	#column meta
	c1<-data1[[col.meta]]
	rownames(c1)<-data1[[col.meta]][,ID]
	c2<-data.frame(data2[[col.meta]])
	rownames(c2)<-data2[[col.meta]][,ID]
	.col.meta<-merge(c1,c2,all=TRUE)
	rownames(.col.meta)<-make.unique(.col.meta[,ID])

	
	return(list(data=merged.data,row.meta=.row.meta,col.meta=.col.meta[fixlc(tmp.id2[fixln(colnames(merged.data)),]),]))
}

# merge similar column names
# filling in each others missing values
col.merge.na<-function(obj,distance=1){
	# convert to matrix to avoid dealing with factor
	
	obj<-tmp.obj<-as.matrix(obj)
	
	#loop on column number 
	#exit loop when all non-unique columns are merged
	# inputting missing values in the first instance with non-missing values in the next
	res<-list()
	name<-colnames(obj)
	watcher<-1:length(name)
	i<-1
	while(length(watcher)>0){
		name<-colnames(tmp.obj)
		watcher<-1:length(name)
		if(is.null(distance)){ 
				id<-grep(name[1],name[-1],ignore.case = TRUE)+1  # account for avoiding self match
			} else {
				id<-agrep(name[1],name[-1],max.distance=distance,ignore.case = TRUE)+1  # account for avoiding self match
		}	
		if(length(id)==0) id<-1
		if(id>1) {
			out<-tryCatch(matrix(merge.na(tmp.obj[,1],tmp.obj[,id]),,1) ,error=function(e) {data.frame(rep("error",nrow(obj)))}) # >2 column merges not supported
			colnames(out)<-name[1]  
			fixed<-c(watcher[1],id)  
		} else {
			out<-obj[,i,drop=FALSE]
			fixed<-watcher[1]
		}
		res[[i]]<-as.matrix(out)
		watcher<-watcher[!watcher%in%fixed]
		tmp.obj<-tmp.obj[,watcher,drop=FALSE]
		# cat(paste(c("DROP--> ",colnames(obj)[(!colnames(obj)%in%colnames(tmp.obj))],"\n"),collapse=", "))
		# print(c("HAVE--> ",sapply(res,colnames,"\n")))
		i<-i+1
	}
	
	return(data.frame(do.call("cbind",res)))
	
}

#merge two vectors filling in NAs
merge.na<-function(obj1,obj2){
	tmp<-fixlc(obj1)
	tmp[is.na(tmp)]<-fixlc(obj2)[is.na(tmp)]
	tmp
}

#wrapper for for multiple column merge.na
multiple.merge.na<-function(obj,name.char="_",prefix="merged"){
	# obj is a data.frame
	# name.char and prefix are used to construct a name for the merged object
	tmp.obj<-obj
	while(ncol(tmp.obj)>1){
		tryCatch(tmp.obj[,1]<-matrix(merge.na(fixlc(tmp.obj[,1]),fixlc(tmp.obj[,2]))), error=function(e){})
		tmp.obj<-tmp.obj[,-2,drop=FALSE]
	}	
	
	#try to make a column name
	# break on name and take the last element
	tmp.n<-unlist(strsplit(colnames(tmp.obj), name.char))
	colnames(tmp.obj)<-paste0(prefix,name.char,tmp.n[length(tmp.n)])
	return(tmp.obj)
}

#trim trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

test<-function(){
#rudimentary unit tests

#merge.data.cube
#---------------------
#data
df1<-data.frame(matrix(1:10,10,10)) 
df2<-data.frame(matrix(10:20,10,10))
#col meta
df1.col<-cbind(merge.ID=letters[c(1:5,11:15)],info=1)
df2.col<-cbind(merge.ID=letters[c(1:5,21:25)],info=2,info2=3)
#rowmeta
df1.row<-cbind(merge.ID=c(1:10),info=1,info2=3)
df2.row<-cbind(merge.ID=c(11:20),info=2)
#create data cube
data1<-list(data=df1,row.metadata=df1.row,col.metadata=df1.col)
data2<-list(data=df2,row.metadata=df2.row,col.metadata=df2.col)
tryCatch(merge.data.cube(data1,data2,ID="merge.ID"),error=function(e) {cat("merge.data.cube failed\n")}) # add error report info


#multiple merge.na
#----------------------
obj<-matrix(1:10, 10,10,byrow=TRUE)
set.seed(123)
obj[sample(1:100,75)]<-NA
colnames(obj)<-paste0(1:ncol(obj),"_variable")

tryCatch(multiple.merge.na(obj),error=function(e){cat("multiple.merge.na<-->FAILED\n")})
}







