#function to convert pattern to a single char objects name
rename <- function(x, pattern, replace="_"){
		#strangely sapply will not work without effort here
		replace<-rep(replace,length(pattern))
		for(i in seq_along(pattern))
			{
				x<-gsub(pattern[i], replace[i], x)
			}
		return(x)	
}


# relative standard deviation
#redo calc.stat using dplyr
calc.rsd<-function(data,factor,sig.figs=2){
	d.list<-split(data,as.factor(factor))
	res<-do.call("cbind",lapply(1:length(d.list),function(i){
		obj<-d.list[[i]]
		means<-apply(obj,2,mean,na.rm=T)
		sd<-apply(obj,2,sd,na.rm=T)
		signif(sd/means,sig.figs)*100
	}))
	colnames(res)<-paste0("RSD-",names(d.list))
	return(as.data.frame(res))
}

#fold change of means
calc.FC<-function(data,factor,denom=levels(factor)[1],sig.figs=1,log=FALSE){
	#rel is the order of the level which will be in the denominator
	d.list<-split(data,as.factor(factor))
	res<-do.call("cbind",lapply(1:length(d.list),function(i){
		obj<-d.list[[i]]
		apply(obj,2,mean,na.rm=T)
	}))
	colnames(res)<-names(d.list)
	rel=match(denom,colnames(res))
	fc<-fold.change(res,log=log,rel=rel)
	colnames(fc)<-paste0("FC-",colnames(fc),"/",rep(denom,ncol(fc)))
	return(as.data.frame(round(fc[,-rel,drop=FALSE],sig.figs)))
}

# redo using dplyr!
calc.stat<-function(data,factor,stat,...)
	{
		d.list<-split(data,as.factor(factor))
		#function to calculate (ddply, stopped working after update? not obvious why)
		calc<-function(d.list,stat,...){
					out<-sapply(1:length(d.list),function(i)
						{
							obj<-d.list[[i]]
							apply(obj,2,get(stat),...) #to avoid NA's in stats
						})
					out<-data.frame(matrix(out,ncol=length(d.list)))	
					colnames(out)<-paste(stat, "-",names(d.list), sep="")	
					out
					}
		#wrapper to calculate stats from a list (fix later using all plyr fxns)
		output<-do.call("cbind",lapply(1:length(stat),function(i)
			{
				what<-stat[i]
				name.what<-paste(what,"-",sep="")
				calc(d.list,stat[i],...)	
			}))
			return(as.data.frame(output))
	}

# break string and get object into colummns by position in original string position 
str.get<- function(obj, sep=",",get=1)
	{
		do.call("cbind",lapply(1:ncol(obj),function(i)
			{
				tmp<-as.character(obj[,i])
				as.numeric(t(as.data.frame(strsplit(tmp,sep)))[,get])
			}))
	}

#calculate fold change relative to column
fold.change<-function(obj,rel=1,log=FALSE)
	{
		if(log==FALSE){
			rel<-obj[,rel]
			obj/rel
		} else {
			rel<-obj[,rel]
			obj-rel
		}
	}

# function to extract data based on non-missing in index
sub.data<-function(data,index)
	{
		#input = data object with rows the dimension to be split
		#index specifying groups for comparison with ordered same a sample rows
		#returns list [1] = index
		# [2] data
		keep.id<-!is.na(index)
		list(factor= index[keep.id],data=data[keep.id,])
	}

#match two data frames based on rownames	
match.data<-function(data1,data2)
	{
		#args
		#data1 and data2 are two data frames with rownames to be matched
		#return values
		#data1 whose rownames intersect with rownames of data2
		#data2 whose rownames intersect with rownames of data1
		#both sharing the same rowname order
		tmp1<-data1[rownames(data1)%in%rownames(data2),]
		tmp2<-data2[rownames(data2)%in%rownames(data1),]
		list(data1=tmp1[order(rownames(tmp1)),],data2=tmp2[order(rownames(tmp2)),])
	}

#anova
anova.formula.list<-function(data,formula,meta.data)
	{
	  tmp.data<-cbind(meta.data,data) # bind with data for easy scoping
	  tmp<-lapply(1:ncol(data),function(i)
		{
			#tryCatch(na.omit(as.data.frame(with(meta.data,anova(lm(as.formula(paste("data[,",i,"]~",formula,sep=""))))[,5,drop=FALSE]))), error=function(e){NULL})
			tmp<-tryCatch(na.omit(as.data.frame(anova(lm(as.formula(paste("data[,",i,"]~",formula,sep="")),data=tmp.data))[,5,drop=FALSE])), error=function(e){data.frame(1)})
			if(nrow(tmp)==0){tmp<-data.frame(1)} # all errors become 1
			tmp
		})
		if(is.null(tmp))
			{
					return(cat("Error in test","\n"))
			} else {
					tmp<-as.data.frame(tmp)
					colnames(tmp)<-colnames(data)
					as.data.frame(t(tmp))
			}
}

#get summary statistics should separate anova from summary
stats.summary <- function(data,comp.obj,formula,sigfigs=3,log=FALSE,rel=1,do.stats=TRUE,...){
		#summarise and make ANOVA from data based on formula 
		#check.get.packages(c("qvalue"))  using fdrtools instead to avoid random erros with initialization
		
		test.obj<-join.columns(comp.obj)
		#get summary by splitting data by each column of meta.data
		data.summary<-function(data,test.obj,log,sigfigs,...)
				{
					#split data
					tmp<-sub.data(data,test.obj)
					if(!is.factor(tmp[1])) {
						fct<-factor(as.character(unlist(tmp[1])))# create factors
					} else {
						fct<-tmp[1]
					}		
					tmp.data<-data.frame(tmp[[2]])
						
					# get means sd, fold change
					means<-calc.stat(tmp.data,factor=fct,stat=c("mean"),...)
					sds<-calc.stat(tmp.data,factor=fct,stat=c("sd"),...)
					fc<-fold.change(means,log=log,rel)
					colnames(fc)<-paste(colnames(fc),rep(colnames(fc)[rel],ncol(fc)), sep="/")

					#format output from means and sd
					names<-paste(unlist(as.data.frame(strsplit(colnames(means),"-"))[2,])," mean +/- std dev" , sep="")
					mean.sd<-matrix(paste(unlist(signif(means,sigfigs)), " +/- ", unlist(signif(sds,sigfigs-1)),sep=""), ncol=ncol(means))
					colnames(mean.sd)<-names
					#bind with fold change
					res<-data.frame(cbind(mean.sd,round(fc[,-rel,drop=FALSE],2)))
					tryCatch(rownames(res)<-colnames(data),error=function(e){NULL})
					return(res)
				}
		message(cat("Generating data summary...","\n"))
		stats.summary<-data.summary(data,test.obj,sigfigs=sigfigs,log=log,na.rm=TRUE)		

		#statistical tests (should add control for theses)
		if(do.stats){
			message(cat("Conducting tests...","\n"))
			p.values<-anova.formula.list(data,formula,meta.data=comp.obj)

			#multiple hypotheses tested adjustments	
			message(cat("Conducting FDR corrections...","\n"))
			adj.p<-data.frame(do.call("cbind",sapply(1:ncol(as.matrix(p.values)),function(i)
				{
					as.data.frame(p.adjust(as.matrix(p.values[,i]), method = "BH", n = nrow(p.values)))
				})))
			colnames(adj.p)<-paste(colnames(p.values),"adjusted.p.values",sep="_")	
			#estimate q-values	
			adjusted.q<-data.frame(sapply(1:ncol(as.matrix(p.values)),function(i)
				{
					#tryCatch(qvalue(as.matrix(p.values[,i]))$qvalues,error=function(e){matrix("Can not estimate",nrow=nrow(p.values),ncol=1)})
					FDR.adjust(as.matrix(p.values[,i]),type="pvalue",return.all=TRUE)$qval
				}))
				
				colnames(adjusted.q)<-paste(colnames(p.values),"q.values",sep="_")	
				colnames(p.values)<-paste(colnames(p.values),"p.values",sep="_")	
				res<-cbind(stats.summary,p.values,adj.p,adjusted.q)
		} else {
				res<-stats.summary
		}
		
		return(res)
	}

#function to carry out covariate adjustments
#-------------------------
covar.adjustment<-function(data,formula)
	{
	#set up that formula objects need to exists in the global environment --- fix this
	#data--> subjects as rows, measurements as columns
	#formula	<- ~ character vector
	#lm will be iteratively fit on each variable 
	#model residuals + preadjusted column median will be returned
	data<-as.data.frame(data)
	names(data)<-colnames(data)
	output<-list()
	n<-ncol(data)
	output<-lapply(1:n,function(i)
		{
			tryCatch(tmp<-as.formula(c(paste(paste("data$'",colnames(data)[i],"'~",sep=""),paste(formula,sep="+"),sep=""))),
			error= function(e){tmp<-as.formula(c(paste(paste("data[,i]","~",sep=""),paste(formula,sep="+"),sep="")))})
			fit<-lm(tmp,data=data)$residuals
			matrix(fit,,1)
		})
	out<-data.frame(do.call("cbind",output))
	tryCatch(dimnames(out)<-dimnames(data), error=function(e){NULL}) # no clue why this throws errors randomly
	#add back pre-adjustment column min to all
	min<-apply(out,2,min, na.rm=T)
	adj.out<-do.call("cbind",sapply(1:ncol(out),function(i)
		{
			out[,i,drop=F] + abs(min[i])
		}))
	return(adj.out)
	}

#helper function for getting statistics for making box plots
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE) 
	{
		require(plyr)

		# New version of length which can handle NA's: if na.rm==T, don't count them
		length2 <- function (x, na.rm=FALSE) {
			if (na.rm) sum(!is.na(x))
			else       length(x)
		}

		# This is does the summary; it's not easy to understand...
		datac <- ddply(data, groupvars, .drop=.drop,
					   .fun= function(xx, col, na.rm) {
							   c( N    = length2(xx[,col], na.rm=na.rm),
								  mean = mean   (as.numeric(as.matrix(xx[,col])), na.rm=na.rm),
								  sd   = sd     (xx[,col], na.rm=na.rm)
								  )
							  },
						measurevar,
						na.rm
				 )

		# Rename the "mean" column    
		#datac <- rename(datac, c("mean"= measurevar))

		datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

		# Confidence interval multiplier for standard error
		# Calculate t-statistic for confidence interval: 
		# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
		ciMult <- qt(conf.interval/2 + .5, datac$N-1)
		datac$ci <- datac$se * ciMult
		return(datac)
	}
	
#get poisson and quasi posson p-values for one-way comparison
prot.test<-function(data, group, type = c("poisson","quasi-poisson"), FDR="BH"){
	offset <- NULL
	#type<-match.arg(type)
	#group<-as.numeric(as.factor(group[,]))
	
	p.values<-sapply(1:ncol(data), function(i){
			
			tmp<-round(as.numeric(data[,i]),0)
			
			if(type=="poisson"){
				# poisson p-value
				g1a <- glm(tmp ~ group, family=poisson)
				g1 <- glm(tmp ~ 1, family=poisson)
				anovaP <- data.frame(anova(g1, g1a, test="Chisq"))
				out<-ifelse(anovaP[2,4] < 0.1e-15, 1, anovaP[2,5])
			}
			
			if(type=="quasi-poisson"){ 			
				# quasi p-value
				gquasi1a <- glm(tmp ~ group, offset=offset, family=quasi(link=log, variance=mu))
				gquasi1 <- glm(tmp ~ 1, offset=offset, family=quasi(link=log, variance=mu))
				anovaPq <- data.frame(anova(gquasi1, gquasi1a, test="F"))
				out<-ifelse(anovaPq[2,4] < 0.1e-15, 1, anovaPq[2,6])
			}
			return(out)
		})		
	# FDR 	
	adj.p<-as.data.frame(p.adjust(p.values, method =FDR, n = length(p.values))) 
	adjusted.q<-FDR.adjust(as.matrix(p.values),type="pvalue",return.all=TRUE)$qval
     
	data.frame(p.values=p.values, adjusted.p.values = adj.p , q.values = adjusted.q)	
 }
 
#t-test with FDR for many variables
multi.t.test<-function(data, factor,mu=NULL,paired=FALSE,progress=TRUE,FDR="BH",qvalue="storey",...){
	check.get.packages(c("qvalue","fdrtool"))
	
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
	p.vals<-sapply(1:ncol(data), function(i){
			if (progress == TRUE){setTxtProgressBar(pb, i)}
			if(is.null(mu)){
				#test if variance is equal
				e.var<-tryCatch(var.test(data[,i]~unlist(factor))$p.value, error=function(e){1})
				if(is.nan(e.var)|is.na(e.var)){e.var<-1}
				if(e.var<=0.05){equal.var<-FALSE} else {equal.var <-TRUE}
				val<-tryCatch(t.test(data[,i]~unlist(factor),paired = paired,var.equal = equal.var)$p.value ,error=function(e){1})
				if(is.nan(val)|is.na(val)){val<-1}
				val
			} else {
				if(!length(mu)==ncol(data)){mu<-unlist(matrix(mu,ncol(data),1));warning("mu didn't match the number of tests and was recycled")}
				val<-tryCatch(t.test(data[,i],mu=mu[i])$p.value ,error=function(e){warning("error in test");1})
				if(is.nan(val)|is.na(val)){val<-1}
				val
			}
	})
	if (progress == TRUE){close(pb); message("Calculating FDR adjustment and q-values")}
	adj.p<-as.data.frame(p.adjust(as.matrix(p.vals), method = FDR, n = length(p.vals)))
	if(qvalue=="fdrtools"){
		adjusted.q<-FDR.adjust(as.matrix(p.vals),type="pvalue",return.all=TRUE)$qval
	} else {
		library(qvalue)
		
		adjusted.q<-tryCatch(qvalue(as.matrix(p.vals),...)$qvalue,error=function(e){rep(1,length(p.vals))})
	}
	
	names<-paste("t.test",c("p.value","adjusted.p.value","q.value"),sep="_")
	out<-data.frame(p.vals,adj.p,adjusted.q)
	colnames(out)<-names
	rownames(out)<-colnames(data)
	return(out)
} 

#carry mann-whitney U test and FDR for data frames
multi.mann.whitney<-function(data,factor,progress=TRUE,FDR="BH",qvalue="story"){
		#mann-whitney U test with FDR for many variables

		check.get.packages(c("qvalue","fdrtool"))
	
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
		p.vals<-sapply(1:ncol(data), function(i){
				if (progress == TRUE){setTxtProgressBar(pb, i)}
				
					val<-tryCatch(wilcox.test(data[,i]~unlist(factor))$p.value ,error=function(e){1})
					if(is.nan(val)|is.na(val)){val<-1}
					val
		})
	if (progress == TRUE){close(pb); message("Calculating FDR adjustment and q-values")}
	adj.p<-as.data.frame(p.adjust(as.matrix(p.vals), method = FDR, n = length(p.vals)))
	if(qvalue=="fdrtools"){
		adjusted.q<-FDR.adjust(as.matrix(p.vals),type="pvalue",return.all=TRUE)$qval
	} else {
		library(qvalue)
		adjusted.q<-tryCatch(qvalue(as.matrix(p.vals),...)$qvalue,error=function(e){rep(1,length(p.vals))})
	}
	
	names<-paste("mann.whitney.U.test",c("p.value","adjusted.p.value","q.value"),sep="_")
	out<-data.frame(p.vals,adj.p,adjusted.q)
	colnames(out)<-names
	rownames(out)<-colnames(data)
	return(out)
} 

#simple mixed effects model for repeated measures
simple.lme<-function(data,factor,subject,FDR="BH", progress=TRUE,interaction=FALSE){
	library("lme4")
	library(car)
	tmp.data<-data.frame(data,factor,subject)
	
	if(interaction){
		r.formula<-paste(paste(colnames(factor),collapse=" + "),paste(colnames(factor),collapse=" : "),paste0("(1|",colnames(subject),")"),sep=" + ")
		.name<-c(colnames(factor),paste(colnames(factor),collapse="_"))
	} else {
		r.formula<-paste(paste(colnames(factor),collapse=" + "),paste0("(1|",colnames(subject),")"),sep=" + ")
		.name<-colnames(factor)
	}

	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
	lmer.p.values<-do.call("rbind",lapply(1:ncol(data), function(i){
			if (progress == TRUE){setTxtProgressBar(pb, i)}
			mod<-lmer(as.formula(paste0(paste0("data[,",i,"]~ "), r.formula)), data=tmp.data)
			res<-Anova(mod)
			res<-matrix(res$"Pr(>Chisq)",nrow=1)
			dimnames(res)<-list("p.value",.name)
			res
	}))
	colnames(lmer.p.values)<-paste(colnames(lmer.p.values),"p.value",sep="_")
	if (progress == TRUE){close(pb)}
	#FDR adjust 
	adj.p<-do.call("cbind",lapply(1:ncol(lmer.p.values),function(i){as.data.frame(p.adjust(as.matrix(lmer.p.values[,i]), method = FDR, n = length(lmer.p.values[,i])))}))
	colnames(adj.p)<-paste(.name,FDR,"adjusted.p.value",sep="_")
	adjusted.q<-do.call("cbind",lapply(1:ncol(lmer.p.values),function(i){FDR.adjust(as.matrix(lmer.p.values[,i]),type="pvalue",return.all=TRUE)$qval}))
	colnames(adjusted.q)<-paste(.name,"q.value",sep="_")

	return(data.frame(lmer.p.values,adj.p,adjusted.q))
} 
 
#multi-LME with formula interface (also see simple.lme) # note random term should be +(1|random.term)
formula.lme<-function(data,formula,FDR="BH", progress=TRUE){
		#data  = data.frame of values to test and test factors
		#factor = object to be tested 
		#subject = identifier for repeated measures
		check.get.packages(c("lme4","car"))
		#not sure how to ignore test factors in data, cause error in loop
		
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
		lmer.p.values<-do.call("rbind",lapply(1:ncol(data), function(i){
			if (progress == TRUE){setTxtProgressBar(pb, i)}
			mod<-tryCatch(lmer(as.formula(paste0("data[,",i,"]~", formula)), data=data),error=function(e){NULL})
			if(is.null(mod)){1} else {
				res<-Anova(mod)
				tmp<-t(data.frame(res$"Pr(>Chisq)"))
				colnames(tmp)<-rownames(res)
				tmp
			}
		}))
		if (progress == TRUE){close(pb)}
		#FDR adjust 
		p.vals<-as.matrix(lmer.p.values)
		padj<-do.call("cbind",lapply(1:ncol(p.vals),function(i){
			obj<-p.vals[,i]
			as.data.frame(p.adjust(obj, method = FDR, n = length(obj)))
		}))
		#q value estimate
		qval<-do.call("cbind",lapply(1:ncol(p.vals),function(i){
			obj<-p.vals[,i]
			FDR.adjust(obj,type="pvalue",return.all=TRUE)$qval
		}))
		
		names<-c(paste(colnames(lmer.p.values),"lme","p.value",sep="_"),
				paste(colnames(lmer.p.values),"lme",FDR,"adjusted.p.value",sep="_"),
				paste(colnames(lmer.p.values),"lme","q.value",sep="_"))
		out<-data.frame(lmer.p.values,padj,qval)
		dimnames(out)<-list(colnames(data),names)
		return(out)
} 
 
#calculate AUC for multiple treatments
 multi.group.AUC<-function(data,subject.id,sample.type, time){
	library(pracma)
	#too lazy to rename objects from older fxn
	subject.id<-as.factor(subject.id)
	fact<-as.factor(sample.type)	#sample type factor
	tme<-as.factor(time)	#time	
	
	#split objects
	tmp.data<-split(data,fact)
	tmp.time<-split(tme,fact)
	tmp.subs<-split(as.character(subject.id),fact)
	
	group.AUC<-lapply(1:nlevels(fact),function(i){
		ddata<-tmp.data[[i]]
		ttime<-tmp.time[[i]]
		subs<-tmp.subs[[i]]
		
		#calculate AUC
		AUC<-sapply(1:length(ddata),function(i)
		{
			
			obj<-split(as.data.frame(ddata[[i]]),subs)
			#subtract baseline for correct negative AUC
			base.obj<-lapply(1:length(obj),function(j)
				{
					tmp<-as.numeric(as.matrix(unlist(obj[[j]])))
					tmp-tmp[1]
				})
			tmp<-split(as.data.frame(ttime),subs)
			#x11()
			#plot(as.numeric(as.matrix(do.call("cbind",tmp))),as.numeric(as.matrix(do.call("cbind",base.obj))))
			out<-as.data.frame(sapply(1:length(obj),function(j)
			{
				x<-as.numeric(as.matrix(unlist(tmp[[j]])))
				o<-order(x) # need to be in order else AUC will be wrong!
				y<-as.numeric(as.matrix(unlist(base.obj[[j]])))
				trapz(x[o],y[o])
			}))
		colnames(out)<-colnames(data[i])
		out
		})
		tmp<-do.call("cbind",AUC)
		rownames(tmp)<-paste(levels(fact)[i],names(split(as.data.frame(ttime),subs)),sep="_")
		tmp
	})	
	do.call("rbind",group.AUC)
}

#trying to generalize baseline adjustment
two.factor.adj<-function(data,factor1,factor2,adj.factor,level=0,fxn="-"){
	#too lazy to rename objects from older fxns getting ugly
	# too lazy (no time) to generalize further
	factor1<-as.factor(factor1)	
	factor2<-as.factor(factor2)
	#sample type factor
	tme<-as.factor(adj.factor)	#adj.factor	
	
	#split objects
	tmp.data<-split(data,factor1)
	tmp.adj.factor<-split(tme,factor1)
	tmp.subs<-split(as.character(factor2),factor1)
	
	group.adj<-lapply(1:nlevels(factor1),function(i){
		ddata<-tmp.data[[i]]
		ttime<-tmp.adj.factor[[i]]
		subs<-tmp.subs[[i]]
		
		
		#calculate 
		results<-lapply(1:length(ddata),function(i)
		{
			tmp2.adj.factor<-split(ttime,subs)
			obj<-split(as.data.frame(ddata[[i]]),subs)
			#subtract baseline for correct negative AUC
			adj.obj<-matrix(unlist(lapply(1:length(obj),function(j)
				{
					id<-c(1:length(tmp2.adj.factor))[tmp2.adj.factor[[j]]==level]
					tmp<-as.numeric(as.matrix(unlist(obj[[j]])))
					do.call(fxn,list(tmp,tmp[id]))
				})),,1)
			colnames(adj.obj)<-colnames(data[i])
			adj.obj
		})	
		# results.meta<-do.call("cbind",lapply(1:length(ddata),function(i)
		# {
			# tmp2.adj.factor<-split(ttime,subs)
			# obj<-split(as.data.frame(ddata[[i]]),subs)
			#get meta data
			obj<-split(as.data.frame(ddata[[i]]),subs) # redundant from above
			tmp2.adj.factor<-split(ttime,subs) # redundant from above
			n1<-rep(names(tmp.adj.factor)[i],length(unlist(obj)))
			n2<-rep(names(tmp2.adj.factor),times=sapply(obj,dim)[1,])
			adj.n<-matrix(unlist(tmp2.adj.factor),,1)
			tmp.res<-data.frame(n1,n2,adj.n)
			r.names<-join.columns(tmp.res,"_")
			
		# }))
		tmp<-do.call("cbind",results)
		rownames(tmp)<-r.names
		tmp
	})	
	out<-do.call("rbind",group.adj)
}

#calculating qvalue and local FDR
FDR.adjust<-function(obj,type="pvalue",return.all=FALSE){
	check.get.packages("fdrtool")
	#adjust p-values for multiple hypothese tested
	#options for FDR for tests c("normal", "correlation", "pvalue", "studentt")\
	#methods for FDR c("fndr", "pct0", "locfdr")
	obj<-as.numeric(as.character(unlist(obj))) # just to be sure it is numeric
	obj<-fdrtool(obj, statistic=type,plot=FALSE, color.figure=FALSE, verbose=FALSE,cutoff.method="fndr",pct0=0.75)
	if(return.all==TRUE){return(obj)} else {return(as.numeric(as.character(unlist(obj$qval))))}
	}

#probability quantiles for a vector as a factors
get.quantiles<-function(x,probs=seq(0, 1, 0.25)){
	xx<-na.omit(fixln(x))
	if(length(xx)>0){
		tmp<-cut(xx,quantile(xx,probs=probs),include.lowest = TRUE)
		factor(tmp,labels=c(paste0("Q",1:nlevels(tmp))),levels(tmp))
	} else {
		x
	}	
}