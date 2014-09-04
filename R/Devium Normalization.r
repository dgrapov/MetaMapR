
# data overview and normalization functions
#function to calculate within and between batch precision for all variables
calc.mRSD<-function(data,batch=data.frame(1:nrow(data)),summary.range=seq(0,100,10),use="mean"){
	library(plyr)
	
	#bin summaries into range
	if(!is.factor(batch)){batch<-tryCatch(factor(batch[,],levels=unique(batch[,])),error=function(e){factor(batch,levels=unique(batch))}); message("Batch converted to factor.")}
	
	#main object
	tmp<-data.frame(batch=batch,data)
	
	#parametric summary
	b.m<-ddply(tmp,.(batch),colwise(mean))
	b.s<-ddply(tmp,.(batch),colwise(sd))
	b.rsd<-b.s/b.m*100
	b.rsd[,1]<-b.m[,1]
	
	
	# #non-parametric summary
	# b.m2<-ddply(tmp,.(batch),colwise(median))
	# b.s2<-ddply(tmp,.(batch),colwise(IQR))
	# b.rsd2<-b.s2/b.m2*100
	# med.rsd.np<-apply(b.rsd2[,-1],1,median,na.rm=T) # 
	
	#generate summary objects for analytes between all batches
	analyte.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],2,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],2,sd,na.rm=T)) 
	colnames(analyte.RSD)[1]<-use# RSD for variables over all batches
	analyte.RSD.summary<-split.bins(obj=analyte.RSD[,1],bins=seq(0,100,10)) # summary for variables over all batches
	analyte.RSD.summary$percent<-round(analyte.RSD.summary$count/sum(analyte.RSD.summary$count)*100,1)
	
	#generate summary objects for batches based on all analytes
	within.batch.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],1,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],1,sd,na.rm=T))
	rownames(within.batch.RSD)<-b.rsd[,1]
	colnames(within.batch.RSD)[1]<-use
	within.batch.RSD.summary<-split.bins(na.omit(within.batch.RSD[,1]),bins=seq(0,100,10)) # ,max(within.batch.RSD[,1]
	within.batch.RSD.summary$percent<-round(within.batch.RSD.summary$count/sum(within.batch.RSD.summary$count)*100,1)
	
	#return summary
	list(batch.means=b.m,batch.sd=b.s,all.batch.RSD=b.rsd,variable.RSD=analyte.RSD,batch.RSD=within.batch.RSD,variable.RSD.summary=analyte.RSD.summary,batch.RSD.summary=within.batch.RSD.summary)	

}

#create summary objects from object produced by calc.mRSD
summarize.performance<-function(obj=gc.perf.raw,sig.figs=2){
	#optionally bin objects into an interval range
	
	#batch summary
	batch.med.perf<-data.frame(median.RSD=signif(median(obj$batch.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$batch.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up batch summary
	tmp<-obj$batch.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	batch.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	batch.summary$cumulative.percent<-cumsum(batch.summary$percent)
	#analyte summary
	var.med.perf<-data.frame(median.RSD=signif(median(obj$variable.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$variable.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up analyte summary
	tmp<-obj$variable.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	var.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	var.summary$cumulative.percent<-cumsum(var.summary$percent)
	
	
	
	#return
	list(batch=batch.med.perf,batch.summary=batch.summary,variable=var.med.perf,variable.summary=var.summary)
}

#split vector based on probabilities and calculate counts
split.prob<-function(obj,probs=seq(0, 1, 0.25)){
	library(plyr)
	splits<-quantile(obj,probs)
	interval<-cut(obj,splits,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	ddply(tmp,.(interval),colwise(length))
}

#split object on bins
split.bins<-function(obj,bins=seq(10,100,10)){
	library(plyr)
	
	interval<-cut(obj,bins,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	tmp<-ddply(tmp,.(interval),colwise(length))
	#need a mechanism to group values over largest bin
	int<-fixlc(tmp$interval)
	int[is.na(int)]<-paste0(">",max(bins))
	tmp$interval<-int
	tmp
}

#identify samples with > quantile
over.prob<-function(val=tmp.data$leverage,prob=0.95){
	val>quantile(val,probs=prob)
}

#adjust data by batch scalar (add ability to get ratios using QCs and adjust samples)
scalar.batch.adjust<-function(obj,factor,use="median",train=NULL){
	#calculate ratio of median of each factor level to global median
	#return adjusted value and adjustment for each factor (should be numeric else levels could be broken)
	# train can be a logical specifying which subset of the data to use to calculate the ratios
	library(plyr)
	if(!class(obj)=="data.frame"){obj<-as.data.frame(obj)}
	if(is.logical(train)){
		full<-data.frame(obj)
		split.data<-split(full,factor(train))
		train.data<-split.data$"TRUE"
		train.factor<-unlist(split(as.data.frame(factor),factor(train))$"TRUE")
	} else {
		train.data<-obj
		train.factor<-factor	
	}
	
	#remove outliers
	
	
	global.med<-apply(train.data,2,use,na.rm=TRUE)
	#main object
	tmp<-data.frame(batch=train.factor,train.data)
	
	#summary for all batches and analytes
	b.m<-ddply(tmp,.(batch),colwise(use))
	med.ratio<-sweep(b.m[,-1,drop=F],2,unlist(global.med),"/")
	
	#return ratio adjusted data
	big.l<-split(obj,factor)

	res<-lapply(1:length(big.l),function(i){
		tmp<-big.l[[i]]
		rat<-unlist(med.ratio[i,])
		res<-sweep(tmp,2,rat,"/")
		res[is.na(res)]<-0 # for ratios with zero
		res
	})
	adjusted.data<-do.call("rbind",res)
	#clean of strange vars
	adjusted.data[adjusted.data=="Inf"]<-NA
	
	list(adjusted.data=adjusted.data,ratios=med.ratio)
}

#plot a single variable line plot
summary.lineplot<-function(val,groups=NULL,view.split=NULL,theme=NULL,se=FALSE,extra=NULL,span=0.75,print.plot=TRUE){
	library(ggplot2)
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	vis.data<-data.frame(value=unlist(unname(val)))
	vis.data$id<-1:length(vis.data$val)
	
	if(is.null(groups)){vis.data$groups<-1;vis.data$color<-1} else {vis.data$groups<-factor(as.matrix(groups))}
	if(is.null(view.split)){
		add.facet<-NULL
	} else {
			vis.data$facet<-vis.data$groups
		if(view.split=="y"){
			add.facet<-facet_grid(facet ~ .)
		} else {
			add.facet<-facet_grid( . ~ facet)
		}	
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=id)) + geom_point(aes(color=groups),alpha=.75,show_guide=FALSE)+
	stat_smooth(aes(group=groups,color=groups),method = "loess", size = 1,se=se,alpha=.1,span=span) + theme + add.facet + xlab(colnames(groups))+ylab(colnames(val))+ 
	guides(col = guide_legend(title = colnames(groups))) + extra
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#box plot for 2 factors with loess smoothing
summary.boxplot2<-function(val,groups=NULL,split.on=NULL,theme=NULL,se=FALSE,span=0.75,extra=NULL,print.plot=TRUE){
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	library(ggplot2)
	vis.data<-data.frame(value=unlist(val))

	if(is.null(groups)){
		vis.data$groups<-1;vis.data$color<-1
	} else {
		vis.data$groups<-factor(as.matrix(groups))
	}
	
	if(is.null(split.on)){
		vis.data$split.on<-""
		l.guide<-NULL
		# extra<-scale_fill_manual(values ="grey50")
		smooth<-NULL
	} else {
		vis.data$split.on<-factor(as.matrix(split.on))
		l.guide<-guides(fill = guide_legend(title = colnames(split.on)))
		# extra<-NULL
		smooth<-stat_smooth(aes(group=split.on,color=split.on),method = "loess", size = 1.25,se=se,alpha=.1,show_guide=FALSE,span=span)
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=groups)) + geom_boxplot(aes(fill=split.on),alpha=.75) +
	smooth + theme + xlab(colnames(groups))+ ylab(colnames(val))+ l.guide 
	p<-p+extra
	
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#create summary plot RSD% analyte mean
RSD.means.plot<-function(obj=list(gc.perf.raw,gc.raw.t1),name=c("Raw","FAME L2 norm"),size=3,alpha=.75,use.log=TRUE,se=FALSE,points=TRUE,theme=NULL,extra=NULL){
	
	library(ggplot2)
	#check if many or single object
	if(length(names(obj))==7){obj<-list(obj)}
	
	#obj can be a list of objects produced by calc.mRSD
	#name will be used for legends
	res<-lapply(1:length(obj),function(i){
		tmp<-obj[[i]]		
		res<-data.frame(method=name[i],RSD=tmp$variable.RSD[,1],mean=apply(tmp$batch.means[-1],2,mean,na.rm=TRUE))
		res$log.mean<-log(res$mean+1)
		res
	})
	
	vis.data<-do.call("rbind",res)
	legend<-TRUE
	if(use.log){
		p<-ggplot(vis.data,aes(x=log.mean,y=RSD,group=method,color=method,fill=method))
		if(points){p<-p+geom_point(alpha=alpha,size=size) ;legend<-FALSE}
		p<-p+stat_smooth(method = "loess", size = 1,show_guide=legend ,se = se,alpha=.75) +
		theme + xlab("log Mean")+ ylab("RSD")+extra #+scale_color_manual(values=rainbow(3))+
		print(p)
	
	} else {
		p<-ggplot(vis.data,aes(x=mean,y=RSD,group=method,color=method,fill=method))
		if(points){p<-p+geom_point(alpha=.75) ;legend<-FALSE }
		p<-p + stat_smooth(method = "loess", size = 1,show_guide=legend ,se = se,alpha=.75)+ 
		theme + xlab("Mean")+ ylab("RSD")+extra #+scale_color_manual(values=rainbow(3))+
		print(p)
	}
}	

#bar plot to summarize performance
RSD.counts.plot<-function(obj,show="variable",plot.obj="count",name="",theme=NULL,extra=NULL,ylabel="number of metabolites",barplot=TRUE){
	
	if(show=="variable"){
		#variables
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$variable.summary)
		})
	} else {
		#samples
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$batch.summary)
		})
	}	
	
	vis.data<-do.call("rbind",res)
	#hack to get the sort correct
	vis.data<-vis.data[order(fixlc(vis.data$RSD)),]
	fix<-grep(">",vis.data$RSD)
	if(length(fix)>0){
		tmp<-rbind(vis.data[-fix,],vis.data[fix,])
		vis.data<-tmp
	}	
	
	vis.data$interval<-factor(fixlc(vis.data$RSD), levels=unique(fixlc(vis.data$RSD)))

	#switch which variable in the data is plotted
	vis.data$plot.obj<-vis.data[,plot.obj]
	upper<-max(vis.data$plot.obj)
	ulim<-if(upper>=10){10} else {upper}
	dlim<-if(upper>=10){2} else {1}
	p<-ggplot(data=vis.data,aes(x=interval,y=plot.obj,fill=method,group=method))
	if(barplot){
		p<-p+geom_bar(position=position_dodge(),stat="identity") 
	} else {	
		p<-p+geom_bar(position=position_dodge(),stat="identity") 
	}
	p<- p+ theme +
	scale_y_continuous(minor_breaks = seq(0 , upper, dlim), breaks = seq(0, upper, ulim)) + xlab("RSD")+ylab(ylabel)+ extra #scale_fill_brewer(palette="Set1")
	print(p)
}

#conduct LOESS normalization on a data frame or matrix
loess.normalization<-function(x,y,subset=NULL,progress=TRUE,adjust="median",span=0.75,...){

	#subset = logical specifying which subset of the data to be used for fitting
	#adjust =  used to adjust post normalized data statistic to that of the pre normalized
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(x), style = 3)} else {pb<-NULL}
	span<-tryCatch(rep(span,length.out=ncol(x)),error=function(e){span}) # recycle
	res<-do.call("cbind",lapply(1:ncol(x),function(i){	
		tmp.x<-x[,i]
		fit<-loess(tmp.x~y,subset=subset,span=span[i],...)
		pred<-predict(fit,data.frame(tmp.x=tmp.x))
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(tmp.x-pred) # residuals for train and test
	}))
	if (progress == TRUE){close(pb)}
	if(!is.null(adjust)){
			scale1<-apply(x,2,adjust,na.rm=TRUE)
			tmp<-sweep(res,2,apply(res,2,min,na.rm=TRUE),"-") # get rid of negative values
			mins<-apply(x,2,min,na.rm=TRUE)
			tmp<-sweep(tmp,2,mins,"+")
			scale2<-apply(tmp,2,adjust,na.rm=TRUE)
			adjustment<-scale1/scale2
			res<-sweep(tmp,2,adjustment,"*")
	}
	return(res)
}

#cross-validation based tuning of LOESS
tune.loess<-function(data,y,folds=7,span.vals=seq(.25,1,by=.1),progress=TRUE){
	# X can be a data frame
	# wrapper for bisoreg::loess.wrapper
	#returns optimal span for each column in X
	library(bisoreg)
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
	res<-unlist(lapply(1:ncol(data),function(i){
		x<-tryCatch(loess.wrapper(x=data[,i], y=y, span.vals = span.vals, folds = folds)$pars$span, error=function(e){NA})
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(x)
	}))
	if (progress == TRUE){close(pb);cat("\n")}
	
	return(res)
}

#calculate RSD
calc.RSD<-function(x,...){sd(x,...)/mean(x,...)}

#calculate correlation between LOESS models for train and test data sets
#use spline interpolation to complete segments prior to correlation testing
#use fxn to test if loess normalization between train/ test set makes any sense
#if train test trend are correlated then loess make sense if not then no
loess.correlation<-function(data,y,train,span=0.75,n=nrow(data),plot=FALSE,cor.method="spearman",...){
	#fit loess models for each variable
	if(plot){par(mfrow=c(ceiling(ncol(data)/ceiling(ncol(data)/4)),ceiling(ncol(data)/4)))}
	do.call("rbind",lapply(1:ncol(data),function(i){
		x1<-data[train,i]
		y1<-y[train]
		x2<-data[!train,i]
		y2<-y[!train]
		fit1<-loess(x1~y1,span=span,...)
		fit2<-loess(x2~y2,span=span,...)
		
	
		#need to interpolate the fits to the same interval 
		#in order to carry out the correlation
		int.fit1<-spline(y1,fit1$fitted, n=n, method = "fmm", xmin =min(c(y1,y2)), xmax = max(c(y1,y2)),...)
		int.fit2<-spline(y2,fit2$fitted, n=n, method = "fmm", xmin =min(c(y1,y2)), xmax = max(c(y1,y2)),...)
		if(plot){
			plot(range(c(y1,y2)),range(c(x1,x2)), type="n",bty="n",xlab="y",ylab=colnames(data)[i])
			lines(int.fit1$x,int.fit1$y,col="red",lwd=2)
			lines(int.fit2$x,int.fit2$y,col="blue",lwd=2)
			legend("topright",col=c("red","blue"),legend=c("train","test"),lwd=2,bty="n")
		}	
		cor<-cor.test(int.fit1$y,int.fit2$y,method=cor.method,...)
		return(data.frame(cor=cor$estimate,p.value=cor$p.value))
		
		
		})
		
	)

}




#test
test<-function(){
tmp.data<-structure(list(x = c(1449, 1449, 1141, 1199, 1477, 1503, 1559, 
1606, 1582, 1447, 1474, 1644, 1478, 1565, 1489, 948, 1360, 1421, 
1197, 1358, 1240, 1175, 1248, 1330, 604, 760, 1002, 935, 1189, 
1049, 1398, 1285, 1646, 1371, 1589, 1109, 859, 1317, 1166, 1174, 
1190, 1891, 1069, 863, 1322, 826, 1172, 1091, 1285, 1063, 895, 
1259, 651, 949, 1143, 1656, 800, 1237, 1134, 282, 1264, 1241, 
905, 1059, 951, 1228, 1178, 998, 1574, 1467, 1076, 791, 933, 
1643, 1786, 2408, 2631, 2098, 851, 1473, 954, 1217, 2151, 1901, 
1253, 1359, 1513, 1093, 1639, 1152, 1585, 1021, 1109, 1034, 1333, 
1397, 1244, 1404, 1201, 1222, 1128, 1186, 1286, 1243, 1134), 
    y = c(1L, 12L, 23L, 34L, 45L, 56L, 67L, 78L, 89L, 100L, 111L, 
    122L, 133L, 144L, 155L, 176L, 177L, 188L, 199L, 210L, 221L, 
    232L, 242L, 253L, 264L, 275L, 286L, 297L, 319L, 329L, 331L, 
    332L, 363L, 367L, 375L, 395L, 406L, 417L, 428L, 439L, 450L, 
    451L, 462L, 473L, 484L, 495L, 505L, 516L, 527L, 537L, 563L, 
    575L, 585L, 607L, 633L, 660L, 674L, 703L, 708L, 719L, 729L, 
    740L, 751L, 762L, 773L, 784L, 795L, 806L, 817L, 828L, 839L, 
    850L, 861L, 872L, 883L, 894L, 905L, 916L, 927L, 938L, 956L, 
    967L, 971L, 982L, 1004L, 1016L, 1022L, 1039L, 1049L, 1051L, 
    1061L, 1073L, 1082L, 1090L, 1116L, 1126L, 1142L, 1144L, 1158L, 
    1169L, 1177L, 1188L, 1213L, 1220L, 1262L)), .Names = c("x", 
"y"), row.names = c("1", "12", "23", "34", "45", "56", "67", 
"78", "89", "100", "111", "122", "133", "144", "155", "176", 
"177", "188", "199", "210", "221", "232", "242", "253", "264", 
"275", "286", "297", "319", "329", "331", "332", "363", "367", 
"375", "395", "406", "417", "428", "439", "450", "451", "462", 
"473", "484", "495", "505", "516", "527", "537", "563", "575", 
"585", "607", "633", "660", "674", "703", "708", "719", "729", 
"740", "751", "762", "773", "784", "795", "806", "817", "828", 
"839", "850", "861", "872", "883", "894", "905", "916", "927", 
"938", "956", "967", "971", "982", "1004", "1016", "1022", "1039", 
"1049", "1051", "1061", "1073", "1082", "1090", "1116", "1126", 
"1142", "1144", "1158", "1169", "1177", "1188", "1213", "1220", 
"1262"), class = "data.frame")

spans<-tune.loess(data=tmp.data[,"x",drop=F],y=tmp.data[,"y"],folds=length(y))
x<-loess.normalization(x=tmp.data[,"x",drop=F],y=tmp.data[,"y"],span=spans)

plot(tmp.data[,"y"],tmp.data[,"x"],"n")
points(tmp.data[,"y"],tmp.data[,"x"],col="red")
points(tmp.data[,"y"],x,col="blue")
lines(tmp.data[,"y"],loess(tmp.data[,"y"]~x)$fitted,col="blue")

}