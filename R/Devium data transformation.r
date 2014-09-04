#test for normality
 normal.test<-function(var,test.method){
			check.get.packages("nortest")
                test<-switch(test.method,
				# add "Kolmogorov-Smirno","Shapiro-Francia"
                "Shapiro-Wilk"  = shapiro.test(var)$p.value,
                "Cramer-von Mises"   = cvm.test(var)$p.value,
                "Anderson-Darling" = tryCatch(ad.test(var)$p.value,error=function(var) 0))        
                if(is.na(test)){test<-0}
                return(test)    
        }

#transform to normal
transform.to.normal<-function(data,data.name="",test.method= "Anderson-Darling", alpha = 0.05 , force.positive=TRUE, transformation="none")
	{	
		check.get.packages(c("plyr","nortest","car"))
		#function to carry out the transformation
		trans<-function(var,transformation,test.method,alpha,force.positive)
			{
				if(force.positive){var<-var + abs(min(var))}
				p.val<-normal.test(var,test.method)
				if(p.val <= alpha) 
					{
							switch(transformation,
							"log"		= .local<-function(var){ list(data=log(var+1), transformation = transformation)},
							"none"		= .local<-function(var){list(data=var,transformation="none")},
							"BOX-COX" 	= .local<-function(var)
								{
									power<-tryCatch(as.numeric(unlist(powerTransform(na.omit(var))$lambda )),error=function(e){NA}) # for negatives family="yjPower"
									if(is.na(power)) { out<-var }else{ out<-var^power }
									list(data=out,transformation=paste(round(power,3),"power", sep=" "))
								})
							
					} else {
							.local<-function(var){list(data=var,transformation="none")}
							}
							
					obj<-.local(var)	
					return(list(data=as.data.frame(obj$data),p.value = p.val,transformation=obj$transformation))
			}
			
			
		#ignore non-numeric
		data<-data[sapply(1:ncol(data), function(i) {class(data[,i])=="numeric"})]
		
		obj<-colwise(trans) (data, transformation,test.method=test.method,alpha=alpha,force.positive)
		trans.data<-as.data.frame(as.list(obj[1,]));colnames(trans.data)<-colnames(data)
		diagnostics=as.data.frame(t(obj[-1,]));colnames(diagnostics)<-c("p-value","transformation")
		out<-list(trans.data, diagnostics=diagnostics);names(out)<-paste(data.name,c("transformed","transformed.diagnostics"),sep=".")
		return(out)
	}
	
#function to store or return transformed object
transform.to.normal.output<-function(obj,name="transformed.data", envir=devium)
	{
		#object stored: get("devium.data.transformation.results",envir)
		#diagnostics
		tmp<-obj[[2]]
		diagnostics<-as.data.frame(matrix(unlist(tmp),ncol=2));dimnames(diagnostics)<-dimnames(tmp)		# need to break list else gwidget doesn't show properly
		#data
		data<-obj[[1]]
		data.name<-names(obj)
		
		#Determine placement of output for EXCEL
		data.list<-list(data,diagnostics)
		list.names<-matrix(data.name)
		start.row<-1;spacer<-1;start.col<-1
		direction<-"horizontal"

		#assign complete object to envir = vdevium
		assign("devium.data.transformation.results",list(data=data, diagnostics=diagnostics,
		placement=list.placement.full(data.list,list.names,direction="horizontal",start.col,start.row,spacer)),envir=envir)
		
		#add results to global environment
		assign(names(obj)[1],as.data.frame(data),envir=.GlobalEnv)
		assign(names(obj)[2],data.frame(p.value=diagnostics[,1,drop=FALSE],transformation=as.factor(unlist(diagnostics[,2,drop=FALSE]))),envir=.GlobalEnv)
		}

#data scaling functions		
scale.data<-function(data, scale="uv", center=TRUE)
	{
		switch(scale,
		"uv" 			= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"pareto" 		= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"vector"		= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"none"			= .local<-function(){return(data)},
		"range scale" 	= .local<-function(){tmp<-sapply(1:ncol(data),function(i)
												{
													obj<-data[,i]
													tmp<-range(obj)
													(obj-tmp[1])/(tmp[2]-tmp[1])
													
												})
												colnames(tmp)<-colnames(data)
												return(tmp)
											})
		.local()						
	}

#scale data within  a range
#rescaling based on: http://cran.r-project.org/doc/contrib/Lemon-kickstart/rescale.R
 rescale<-function(x,newrange) {
	 if(nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
	  # if newrange has max first, reverse it
	  if(newrange[1] > newrange[2]) {
	   newmin<-newrange[2]
	   newrange[2]<-newrange[1]
	   newrange[1]<-newmin
	  }
	  xrange<-range(x)
	  if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
	  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
	  invisible(newrange[1]+(x-xrange[1])*mfac)
	 }
	 else {
	  cat("Usage: rescale(x,newrange)\n")
	  cat("\twhere x is a numeric object and newrange is the min and max of the new range\n")
	 }
}
	
#missing values static imputation 
impute.missing<-function(data, method="min", scalar=1, report=TRUE){
		# data should be a matrix or data frame (samples as rows)
		# methods can be any function
		# scalar will be used to multiply imputed value and can be length one or longer 
		# for example a random normal distribution can be used to generate a scalar to slightly randomize imputed value
		# report = TRUE will return the imputed data in a list and give a report of the number of missing values for each sample and variable as seprate list items
		
		na.id<-is.na(data)
		fill<-apply(data,2,method,na.rm=TRUE)*scalar
		
		#impute
		fixed<-do.call("cbind",lapply(1:ncol(data), function(i)
		{
			#what is being replaced
			obj<-data[,i]
			#id where replacements should go 
			obj[na.id[,i]]<-fill[i]
			obj
		}))
		
		colnames(fixed)<-colnames(data)
		
		if(report == TRUE) {
			row.missing <- matrix(round(apply(na.id,1,sum)/ncol(data)*100,0), ncol=1)
			col.missing <- t(matrix(round(apply(na.id,2,sum)/nrow(data)*100,0), nrow =1 ))
			colnames(row.missing)<-colnames(col.missing) <- "percent missing"
			rownames(row.missing)<-rownames(data)
			rownames(col.missing)<-colnames(data)
			# out<-cbind(c(NA,row.missing),rbind(col.missing,fixed))
			#rownames(out)[1]<-colnames(out)[1]<-"percent missing"
			return(list(imputed.data =fixed, prct.missing.in.row=row.missing, prct.missing.in.column=col.missing))
			} else {
				return(fixed)
			}
	}

#get non-redundant ratio of all variables relative to each other
all.ratios<-function(data){
	if(is.null(colnames(data))){colnames(data)<-c(1:ncol(data))}
	#accesory function
	all.pairs <- function(r)
  		list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))],
       second = rep(1:r, r)[lower.tri(diag(r))])
	id<-all.pairs(ncol(data)) 
	den<-split(id$second,id$first)
	num<-split(id$first,id$first)
	vars<-lapply(1:length(num),function(i){
		obj<-as.matrix(data[,num[[i]][1],drop=FALSE])
		tmp<-sweep(as.matrix(data[,den[[i]],drop=FALSE]),2,obj,"/")
		names<-paste0(colnames(data)[den[[i]]],"_ratio_",colnames(data)[num[[i]]])
		colnames(tmp)<-names 
		tmp
	}) 
	
	do.call("cbind",vars)	   
}	

#
