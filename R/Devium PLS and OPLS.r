
#function to carry out PLS or orthogonal signal correction PLS (O-PLS) adapted from OSC.PLS adding predictions
make.OSC.PLS.model<-function(pls.y,pls.data,comp=2,OSC.comp=1,validation = "LOO",progress=TRUE,cv.scale=FALSE,return.obj="stats",train.test.index=NULL,OPLSDA=FALSE,...){ 
	
	check.get.packages("pls")
	
	#initialize
	OSC.results<-list()
	OSC.results$data[[1]]<-pls.data # may need to 
	OSC.results$y[[1]]<-pls.y<-as.matrix(pls.y)
	if(!is.null(train.test.index)){ # objects fo predictions
			OSC.results$test.data[[1]]<-OSC.results$data[[1]][train.test.index=="test",]
			# if(cv.scale==TRUE){ # the same?
				# OSC.results$test.y<-test.y<-as.matrix(OSC.results$y[[1]][train.test.index=="test",])
			# } else {
			OSC.results$test.y<-test.y<-as.matrix(OSC.results$y[[1]][train.test.index=="test",])
			# }		
			OSC.results$data[[1]]<-OSC.results$data[[1]][train.test.index=="train",] # data may need to be a data.frame
			OSC.results$y[[1]]<-as.matrix(OSC.results$y[[1]][train.test.index=="train",])
	} 
	
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = (OSC.comp+1), style = 3)}
	
	#need to iteratively fit models for each OSC
	for(i in 1:(OSC.comp+1)){ 	
		data<-OSC.results$data[[i]]
		tmp.model<-plsr(OSC.results$y[[1]]~., data = data, ncomp = comp, validation = validation ,scale=cv.scale,...)
		ww<-tmp.model$loading.weights[,1] # 
		pp<-tmp.model$loadings[,1]
		w.ortho<- pp - crossprod(ww,pp)/crossprod(ww)*ww
		t.ortho<- as.matrix(data) %*% w.ortho
		p.ortho<- crossprod(as.matrix(data),t.ortho)/ c(crossprod(t.ortho))
		Xcorr<- data - tcrossprod(t.ortho,p.ortho)
		
		#prediction objects
		if(!is.null(train.test.index)){
			test.data<-OSC.results$test.data[[i]]
			predicted.mod<-	predict(tmp.model,newdata=test.data, ncomp=1:comp, comps=1:comp, type="response")
			OSC.results$predicted.Y[[i]]<-predicted.mod
			# predicted.RMSEP
			OSC.results$predicted.RMSEP[[i]]<-sapply(1:ncol(test.y), function(i){
				(sum((predicted.mod[,i]-test.y[,i])^2)/nrow(predicted.mod))^.5
			})
			#stats for classifiers, currently for two group comparisons only
			if(OPLSDA==TRUE){
				OSC.results$OPLSDA.stats[[i]]<-O.PLS.DA.stats(pred=predicted.mod,truth=test.y)
			}
			
			t.tst<-as.matrix(test.data)%*%w.ortho
			p.tst <- crossprod(as.matrix(test.data), t.tst) / c(crossprod(t.tst))
			OSC.test.data <- as.matrix(test.data) - tcrossprod(t.tst, p.tst)
			OSC.results$test.data[[i+1]]<-OSC.test.data # for next round
		}
		
		#store results
		OSC.results$RMSEP[[i]]<-matrix(t(RMSEP(tmp.model)$val[dim(RMSEP(tmp.model)$val)[1],,]),,ncol=ncol(pls.y)) #
		OSC.results$rmsep[[i]]<- RMSEP(tmp.model)$val[dim(RMSEP(tmp.model)$val)[1],,comp+1]# CV adjusted rmsep for each y by column 
		OSC.results$Q2[[i]]<-matrix(pls::R2(tmp.model)$val,ncol=ncol(pls.y),byrow=TRUE)
		OSC.results$Xvar[[i]]<-drop(tmp.model$Xvar/tmp.model$Xtotvar)#matrix(drop(tmp.model$Xvar/tmp.model$Xtotvar),ncol=1)
		OSC.results$fitted.values[[i]]<-tmp.model$fitted.values
		OSC.results$scores[[i]]<-tmp.model$scores
		OSC.results$loadings[[i]]<-tmp.model$loadings
		OSC.results$loading.weights[[i]]<-tmp.model$loading.weights
		OSC.results$total.LVs[[i]]<-comp
		OSC.results$OSC.LVs[[i]]<-i-1 # account for first model not having any OSC LVs
		#coefficients
		OSC.results$coefficients[[i]]<-matrix(coefficients(tmp.model),ncol=1)
		#initialize data for next round
		OSC.results$data[[i+1]]<-as.data.frame(Xcorr)
		OSC.results$model.description<-as.list( sys.call() )#as.list(environment())
		#update timer
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		
		#exit loop if no more orthogonal dimensions can be calculated
		if(any(is.na(Xcorr))){break}
		}
	
	if (progress == TRUE){close(pb)}
	#calculate VIP if single Y and oscorespls
	#based on http://mevik.net/work/software/VIP.R 
	if(ncol(pls.y)==1){
		object<-tmp.model
		VIP<-function(object){
			SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
			Wnorm2 <- colSums(object$loading.weights^2)
			SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
			t(sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS)))[,comp,drop=FALSE]
		}
		OSC.results$VIP<-tryCatch(VIP(object),error=function(e){data.frame(VIP=matrix(1,nrow(tmp.model$loadings[,]),1))})	
	} else { OSC.results$VIP<-data.frame(VIP=matrix(1,nrow(tmp.model$loadings[,]),ncol(pls.y)))}
	

	if (return.obj=="model"){return(tmp.model)} else {	return(OSC.results)	}
}

#fit many OPLS models to overview optimal LV and OLV
optimize.OPLS<-function(max.LV=4,tolerance =0.01,pls.y,pls.data,validation = "LOO",method="oscorespls",cv.scale=T,...){

	#iterate and fit OSC models for each possible LV > 1
	out<-lapply(1:max.LV, function(i){
		mod<-OSC.correction(pls.y=pls.y,pls.data=pls.data,comp=i,OSC.comp=i,validation = validation,cv.scale=cv.scale,...)
		tmp<-data.frame(RMSEP=do.call("rbind",mod$RMSEP))
		tmp$LV<-i
		tmp$OLV<-rep(mod$OSC.LVs,each=(ncol(pls.y)*(i+1)))
		tmp$pls.y<-rep(1:ncol(pls.y), each=(i+1))
		#do not report partials
		get<-matrix(rep(0:i),nrow=nrow(tmp))
		tmp[get==max(get),]
	})
	obj<-do.call("rbind",out)
	
	#choose optimal combination of LV/OLV for all Ys
	choose.opt.OPLS.comp(obj=obj,pls.y=pls.y,tolerance=0.01)
}

#choose optimal model LV and OLV component number
choose.opt.OPLS.comp<-function(obj,pls.y,tolerance=0.01){
	

	tmp.list<-split(obj,obj$pls.y)
	
	results<-lapply(1:length(tmp.list), function(i){
		x<-tmp.list[[i]]
		RMSEP<-x[,1:2]
		comp<-x$LV
		ocomp<-x$OLV
		
		even<-1:ncol(RMSEP)%%2==0 # CV RMSEP currently assuming this was used in modeling
		tmp<-RMSEP[,even]# CV RMSEP
		is.min<-which.min(tmp)
		min.RMSEP<-tmp[is.min]
		#look for smaller model with in tolerance
		# not worse than this, accept smaller				
		delta<-tmp-min.RMSEP
		tmp.min<-which(delta<=tolerance)
		data.frame(x[c(tmp.min),], delta[tmp.min])

	})
	
	#choose smallest model within tolerance for both
	tmp<-do.call("rbind",results)
	x<-split(tmp$LV, tmp$pls.y )
	LV<-unlist(Reduce(intersect, x))
	x<-split(tmp$OLV, tmp$pls.y )
	OLV<-unlist(Reduce(intersect, x))
	
	#if there is an intersection
	if(length(LV)>0&length(OLV)>0){
		list(best=tmp[tmp$LV==min(LV)&tmp$OLV==min(OLV), ], LV=min(LV), OLV=min(OLV))
	} else {
		list(best=tmp, LV=tmp$LV[which.min(tmp$delta.tmp.min.)], OLV = tmp$OLV[which.min(tmp$delta.tmp.min.)])
	}		
}

#plot OSC results
plot.OSC.results<-function(obj,plot="RMSEP",groups=NULL){
	check.get.packages("ggplot2")
	# obj is from make.OSC.PLS.model
	#plot = one of: c("RMSEP","scores","loadings","delta.weights")
	#groups is a factor to show group visualization in scores plot
	switch(plot,
		RMSEP 			=  .local<-function(obj){
								#bind info and RMSEP
								comps<-obj$total.LVs
								ocomps<-obj$OSC.LVs
								plot.obj<-obj$RMSEP
								bound<-do.call("rbind",lapply(1:length(comps),function(i)
									{
										out<-as.data.frame(cbind(plot.obj[[i]][,1],c(0:comps[i]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
										colnames(out)<-c("RMSEP","component","model")
										out
									}))
								bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
								
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(),  
													plot.background = element_blank()
												 )
								#plot				 
								p<-ggplot(data=bound, aes(x=component, y=RMSEP,color=model)) + geom_line(size=1,alpha=.5) + geom_point(size=2)+.theme
								print(p)
							},
		scores 			=	.local<-function(obj){
								comps<-obj$total.LVs
								ocomps<-obj$OSC.LVs
								plot.obj<-obj$scores
								if(is.null(groups)){groups<-rep("gray",nrow(plot.obj[[1]][,]))}
								bound<-do.call("rbind",lapply(1:length(comps),function(i)
									{
										out<-as.data.frame(cbind(plot.obj[[i]][,1:2],unlist(groups),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
										colnames(out)<-c("Comp1","Comp2","groups","model")
										out
									}))
								bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
								
								#calculate convex hull for polygons for each group
								data.obj <- split(bound, bound$model)
								tmp.obj <- lapply(1:length(data.obj), function(i){
									obj<-data.obj[[i]]
									s2<-split(obj,obj[,3])
									do.call(rbind,lapply(1:length(s2),function(j){
										tmp<-s2[[j]]
										tmp[chull(tmp[,1:2]),] 
										}))
								})
								chull.boundaries <- do.call("rbind", tmp.obj)
							
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(), 
													panel.border = element_rect(colour="gray",fill=NA),
													plot.background = element_blank()
												 )
												 
								#make plot
								p<-ggplot(data=bound, aes(x=Comp1, y=Comp2, group=groups,color=groups)) + #geom_density2d(aes(group=groups))+
								geom_hline(aes(yintercept=0),color="gray60",linetype="dashed")+geom_vline(aes(xintercept=0),color=I("gray60"),linetype=2)+facet_grid(. ~ model)
								p<-p+geom_polygon(data=chull.boundaries,aes(x=Comp1,y=Comp2,fill=groups),alpha=.5) +geom_point(size=2)+.theme
								print(p)
							},
		loadings 		= 	.local<-function(obj){ # will only plot first component for each model
							comps<-obj$total.LVs
							ocomps<-obj$OSC.LVs
							plot.obj<-obj$loadings
							bound<-do.call("rbind",lapply(1:length(comps),function(i)
								{
									out<-as.data.frame(cbind(plot.obj[[i]][,1:2],rownames(plot.obj[[i]]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
									colnames(out)<-c("Comp1","Comp2","variable","model")
									out
								}))
							bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
							
							#custom theme
							.theme<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												legend.position = "none",
												plot.background = element_blank()
											 )
							
							#make plot
							p<-ggplot(data=bound, aes(x=variable,y=Comp1, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
							facet_grid(. ~ model) +.theme
							print(p)
						},
		delta.weights 	= 	.local<-function(obj){ # will only plot first component for each model
							comps<-obj$total.LVs
							ocomps<-obj$OSC.LVs
							plot.obj<-obj$loading.weights
							bound<-do.call("rbind",lapply(2:(length(ocomps)),function(i)
								{
									out<-as.data.frame(cbind(plot.obj[[1]][,1]-plot.obj[[i]][,1],names(plot.obj[[i]][,1]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
									colnames(out)<-c("delta_weight","variable","model")
									out
								}))
							bound[,1]<-signif(as.numeric(as.matrix(bound[,1])),3)	
							
							#theme
							.theme<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												legend.position = "none",
												plot.background = element_blank()
											 )
							#make plot
							p<-ggplot(data=bound, aes(x=variable,y=delta_weight, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
							facet_grid(. ~ model) +.theme
							print(p)
						}
						)				
	.local(obj)
	}

	
#recreating plots based on plot.PCA options with slight modifications (good example of a place to use oob, need to have helper function to switch top level inputs based on class and use generic plotter)
plot.PLS<-function(obj, results = c("screeplot","scores","loadings","biplot"),xaxis=1,yaxis=2,size=3,color=NULL, shape=NULL, label=TRUE, legend.name =  NULL, font.size=5,group.bounds="ellipse",alpha=.5,g.alpha=.2,print.plot=TRUE,extra=NULL,...){
	require(ggplot2)
	require(grid)
	#obj is the results of type get.OSC.model
	#plot = one of: c("screeplot","scores","loadings","biplot","multi")
	#color is a factor to show group visualization of scores based on color
	
	
	local<-switch(results, # only tested for single Y models!
		RMSEP 			=  function(obj,...){
									
									.theme<- theme(
										axis.line = element_line(colour = 'gray', size = .75), 
										panel.background = element_blank(),  
										plot.background = element_blank()
									) 
									# RMSEP<-obj$RMSEP[length(obj$RMSEP)]][,ncol(obj$RMSEP[[1]])] # get for all LVs and optionally CV version
									# Q2<-obj$Q2[[length(obj$Q2)]][,ncol(obj$Q2[[1]])]
									# Xvar<-c(0,obj$Xvar[[length(obj$Xvar)]]) # 0 is for intercept only model
									
									RMSEP<-obj$RMSEP[,ncol(obj$RMSEP)] # get for all LVs and optionally CV version
									Q2<-obj$Q2[,ncol(obj$Q2)]
									Xvar<-cumsum(c(0,obj$Xvar)) # 
									
									
									LV<-paste0("",0:(length(RMSEP)-1))
									tmp<-melt(data.frame(LV,RMSEP,Q2,Xvar))
									
									
									# RMSEP<-obj$RMSEP[,ncol(obj$RMSEP)] # get for all LVs and optionally CV version
									# Q2<-obj$Q2[,ncol(obj$Q2)]
									# Xvar<-c(0,obj$Xvar) # 
									
									# LV<-as.character(c(0:(length(RMSEP)-1))) # account for intercept only model
									# tmp<-melt(data.frame(LV,RMSEP,Q2,Xvar),id=LV)
									
									p<-ggplot(data=tmp ,aes(y=value,x=LV,fill=variable))+
									geom_bar(stat="identity",position=position_dodge())+.theme +ylab("value")+xlab("LV") +extra
									if(print.plot){
										print(p)
									}	 else {
										return(p)
									}
								
							},
		scores 			=	function(obj,color,size,alpha,shape,...){
								comps<-obj$total.LVs[1]
								tmp.obj<-tryCatch(obj$scores[[comps]][,c(xaxis,yaxis)],error=function(e){obj$scores[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce to data.frame
					
								tmp<-data.frame(tmp.obj,id = rownames(tmp.obj))
								#plot 
								.theme2<- theme(
											axis.line = element_line(colour = 'gray', size = .75), 
											panel.background = element_blank(), 
											plot.background = element_blank(),
											legend.background=element_rect(fill='white'),
											legend.key = element_blank()
										 )
										 
								if(is.null(color)){
										tmp$color<-"gray"
									}else{
										tmp$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}
								
								if(is.null(shape)){
										tmp$shape<-21
									}else{
										tmp$shape<-as.factor(shape[,])
								}
								
								points<-if(all(tmp$color=="gray")) { 
									geom_point(color="gray",size=size,alpha=alpha,show_guide = FALSE) 
								} else { 
									geom_point(aes(color=color,),size=size,alpha=alpha)  # shape disabled
								}
								#labels
								tmp$lab.offset<-tmp[,2]-abs(range(tmp.obj[,2])[1]-range(tmp.obj[,2])[2])/50						
								labels<-if(label==TRUE){geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)} else { NULL }
								
								#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj),tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
								#making the actual plot 
								p<-ggplot(data=tmp,aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2])) + 
								geom_vline(xintercept = 0,linetype=2, size=.5, alpha=.5) + 
								geom_hline(yintercept = 0,linetype=2, size=.5, alpha=.5) +
								points +
								.theme2 + 
								labels +
								polygons +
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) 
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								p<-p+extra
								if(print.plot){
									print(p)
								}	 else {
									return(p)
								}
							},
		"loadings"		= function(obj,color,size,alpha,...){
							comps<-obj$total.LVs[1]
							tmp.obj<-tryCatch(obj$loadings[[comps]][,c(xaxis,yaxis)],error=function(e){obj$loadings[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce to data.frame
							tmp<-data.frame(tmp.obj,id = rownames(tmp.obj))
							#plot 
							.theme2<- theme(
										axis.line = element_line(colour = 'gray', size = .75), 
										panel.background = element_blank(), 
										plot.background = element_blank(),
										legend.background=element_rect(fill='white'),
										legend.key = element_blank()
									 )
							#check to make sure color length matches dim[1]
							if(is.null(color)){
									tmp$color<-"gray"
								}else{
									if(!length(color[,])==nrow(tmp)){tmp$color<-"gray"# reset if doesn't match
									} else { 
											tmp$color<-as.factor(color[,])
											if(is.null(legend.name)){legend.name<-colnames(color)}
									}
							}
							
							points<-if(all(tmp$color=="gray")) { 
								geom_point(color="gray",size=size,alpha=alpha,show_guide = FALSE) 
							} else { 
								geom_point(aes(color=color),size=size,alpha=alpha)  
							}
							#labels
							tmp$lab.offset<-tmp[,2]-abs(range(tmp.obj[,2])[1]-range(tmp.obj[,2])[2])/50						
							labels<-if(label==TRUE){geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)} else { NULL }
							
							#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(all(tmp$color=="gray")){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj),tmp$color)# group visualization via 
									polygons<-if(all(tmp$color=="gray")){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
							
							#making the actual plot 
							p<-ggplot(data=tmp,aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2])) + 
							geom_vline(xintercept = 0,linetype=2, size=.5, alpha=.5) + 
							geom_hline(yintercept = 0,linetype=2, size=.5, alpha=.5) +
							points +
							.theme2 + 
							labels +
							polygons +
							scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
							scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) 
							if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
							p<-p+extra
							if(print.plot){
									print(p)
								}	 else {
									return(p)
								}
						},				
		"biplot"		= function(obj,color,size,alpha,...){
								comps<-obj$total.LVs[1]
								loadings<-tmp.loadings<-tryCatch(obj$loadings[[comps]][,c(xaxis,yaxis)],error=function(e){obj$loadings[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce
								scores<-tmp.obj<-data.frame(tryCatch(obj$scores[[comps]][,c(xaxis,yaxis)],error=function(e){obj$scores[,c(xaxis,yaxis)]})) # not sure how to simply unclass and coerce to data.frame
								.theme2<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												plot.background = element_blank()
											 )
								#based on https://groups.google.com/forum/#!topic/ggplot2/X-o2VXjDkQ8
								tmp.loadings[,1]<-rescale(loadings[,1], range(scores[,1]))
								tmp.loadings[,2]<-rescale(loadings[,2], range(scores[,2]))
								tmp.loadings<-data.frame(tmp.loadings,label=rownames(loadings))
								
								#using tmp.obj because no need for labels, and started badly need to rewrite
								#Adding Hoettellings T2 ellipse
								if(is.null(color)){
										tmp.obj$color<-"gray"
									}else{
										tmp.obj$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}	
								
							#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp.obj$color)# group visualization via 
									polygons<-if(all(tmp.obj$color=="gray")){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj[,1:2]),tmp.obj$color)# group visualization via 
									polygons<-if(all(tmp.obj$color=="gray")){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
										}
								}
									
								points<-if(all(tmp.obj$color=="gray")) { 
									geom_point(data=data.frame(tmp.obj),aes_string(x=colnames(tmp.obj)[1], y=colnames(tmp.obj)[2]),color="gray",size=size,alpha=alpha,show_guide = FALSE) 
								} else { 
									geom_point(data=data.frame(tmp.obj), aes_string(x=colnames(tmp.obj)[1], y=colnames(tmp.obj)[2],color="color"),size=size,alpha=alpha)  
								}
								#plot
								p<-ggplot()+
								points +
								polygons+
								geom_segment(data=tmp.loadings, aes_string(x=0, y=0, xend=colnames(tmp.loadings)[1], yend=colnames(tmp.loadings)[2]), arrow=NULL, alpha=0.25)+
								geom_text(data=tmp.loadings, aes_string(x=colnames(tmp.loadings)[1], y=colnames(tmp.loadings)[2], label="label"), alpha=0.5, size=font.size)+
								scale_colour_discrete("Variety")+
								scale_x_continuous(paste(colnames(tmp.obj)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp.obj)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) +
								.theme2
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								p<-p+extra
								if(print.plot){
										print(p)
									}	 else {
										return(p)
									}
							}
		)
							
		local(obj,color=color,size=size,alpha=alpha,shape,...)
	}		
	
#create PLS model
make.PLS.model<-function(y,data,pls.method="simpls",
		ncomp=2, CV="LOO",CV.segments=NULL,segment.type=NULL, CV.scale=FALSE, opt.comp=FALSE){
	#use opt.comp=TRUE to dynamically optimize the # of latent variables
	#minimum of 2 components 
	#need to switch based on CV specifications
	
	#make sure the number of supplied components won't error plsr
	if(ncomp>nrow(data)-1)
		{
			ncomp<-nrow(data)-1
			#cat("The number of components was changed to", ncomp, "to accommodate sample number","\n")
		}
	if(CV=="LOO")

	{	if(opt.comp==TRUE)
		{	
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
			new.comp<-c(2:ncomp)[which.max(R2(mod1)$val[-c(1:2)])]
			#cat("PCs were changed from",PCs,"to", new.comp)
			if(dim(as.data.frame(new.comp))[1]==0){new.comp<-2}
			mod1 <- plsr(y~ as.matrix(data), ncomp=new.comp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
		}else{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
		}
	}else{
		if(opt.comp==TRUE)
		{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
			new.comp<-c(2:ncomp)[which.max(R2(mod1)$val[-c(1:2)])]
			if(dim(as.data.frame(new.comp))[1]==0){new.comp<-2}
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
		}else{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
		}
	}

	mod1
	}
	
#extract OSC submodel from OSC results object
get.OSC.model<-function(obj,OSC.comp){
	#obj = results from OSC.correction()
	#OSC.comp = number of orthogonally corrected components
	
	index<-c(1:length(obj$OSC.LVs))
	id<-index[obj$OSC.LVs==OSC.comp]
	
	#extract and return
	out<-list()
	out$data<-obj$data[[id]]
	out$y<-obj$y
	out$fitted.values<-data.frame(obj$fitted.values[[id]][,,dim(obj$fitted.values[[id]])[3],drop=FALSE])
	colnames(out$fitted.values)<-paste0("Y_",c(1:ncol(out$y[[1]])),"_fitted.values")
	out$residuals<-data.frame(out$fitted.values-out$y[[1]])
	colnames(out$residuals)<-paste0("Y_",c(1:ncol(out$y[[1]])),"_residuals")
	out$RMSEP<-obj$RMSEP[[id]]
	out$predicted.RMSEP<-obj$predicted.RMSEP[[id]]
	out$predicted.Y<-obj$predicted.Y[[id]]
	out$Q2<-obj$Q2[[id]]
	out$Xvar<-obj$Xvar[[id]]
	out$scores<-obj$scores[[id]]
	out$loadings<-obj$loadings[[id]]
	out$loading.weights<-obj$loading.weights[[id]]
	out$total.LVs<-obj$total.LVs[[id]]
	out$OSC.LVs<-obj$OSC.LVs[[id]]
	out$VIP<-obj$VIP
	out$OPLSDA.stats<-obj$OPLSDA.stats[[id]]
	out$coefficients<-data.frame(obj$coefficients[[id]])
	colnames(out$coefficients)<-"coefficients"
	return(out)
}
	
#calculate root mean squared error	
RMSE<-function(values,predictions){sqrt(sum((values-predictions)^2)/length(values))}

#model function
model.fxn<-function(data,inds,algorithm="pcr",y,ncomp=2,return="pred.error",...){
			mod<-do.call(algorithm,list(formula=y~.,data=data,ncomp=ncomp,subset=inds,...=...))
			switch(return,
			"pred.error" = .local<-function(){
								y-predict(mod, newdata=data,ncomp=ncomp,...)
							},
			"coef"		 = 	.local<-function(){
								c(coef(mod))
							}
					)
			.local()
		}

#bootstrap function
boot.fxn<-function(algorithm="pcr",data=tmp.data,y,ncomp=2,return="pred.error", R=499,...){
	library(boot);library(pls)
	boot(data,statistic=model.fxn,R=R,algorithm=algorithm,ncomp=ncomp,y=y,return=return,...=...)	
}

#generate boostrapped parameters for model (only works for single Y models)
boot.model<-function(algorithm="pcr",data=tmp.data,y,ncomp=2,return="pred.error",R=499,...){
	# function currently tailored to pls and tested with pcr and plsr
	# return can be one of c("pred.error","coef")
	# pred.error = out of bag (sample) error of prediction (RMSEP)
	# coef = bootstrapped coefficent weights 
	
	x<-boot.fxn(algorithm,data,y,ncomp,return,R,...)
	in.bag<-boot.array(x)
	out.bag<-in.bag==0
	switch(return,
	"pred.error" 	= .local<-function(){
							in.bag<-boot.array(x)
							oob.error<-mean((x$t^2)[in.bag==0])
							oob.error.sd<-sd((x$t^2)[in.bag==0])
							app.error<-MSEP(do.call(algorithm,list(formula=y~.,data=data,ncomp=ncomp,...=...)),ncomp=ncomp,intercept=FALSE)
							est.error<-sqrt(0.368*c(app.error$val) + 0.632 * oob.error)
							sd.error<-sqrt(0.368*c(app.error$val) + 0.632 * oob.error.sd)
							return(list(RMSEP=data.frame(bootstrapped.0.632_RMSEP = est.error,mean.error = mean(abs(x$t0)),sd.error=sd(abs(x$t0))),boot.obj = x))
						},
	"coef" 			= .local<-function(){ 
							
							return(list(coef=data.frame(bootstrapped.coef=x$t0,CI.95.percent=t(apply(x$t,2,quantile, c(0.025,.975)))),boot.obj = x))
						}
			)	
	.local()	
}

#function to select upper boundaries of a vector based on quantile or number
#returns row id of positions (or logical)
feature.cut2<-function(obj,type="quantile",thresh=.9,separate=FALSE){
	# type can be one of c("number", "quantile")
	# thresh = select values above quantile or number of largest values
	# separate = value tested separately based on sign

	obj<-fixln(obj) # make a vector of type numeric
	#get position of selections
	tmp<-data.frame(id=1:length(obj),obj=obj)
	if(separate==TRUE){
			tmp.l<-split(abs(tmp),sign(tmp$obj))
		} else {
			tmp.l<-list(abs(tmp))
	}
	
	if(type=="quantile"){
		filter<-lapply(1:length(tmp.l),function(i){
			cut.point<-quantile(tmp.l[[i]][,2],prob=thresh)
			tmp.l[[i]][tmp.l[[i]][,2]>=cut.point,1]
		})
	}
	
	if(type=="number"){
		filter<-lapply(1:length(tmp.l),function(i){
			cut.point<-thresh
			x<-tmp.l[[i]][order(tmp.l[[i]][,2],decreasing=TRUE),]
			x[1:nrow(x)<=cut.point,1]
		})
	}
	
	#a logical vector
	# return(c(1:length(obj))%in%unlist(filter))
	
	#return row
	return(unlist(filter))
}
#function to bootstrap many models
multi.boot.model<-function(algorithm="pcr",data=data,y,
							feature.subset=NULL,ncomp=4,return="pred.error",R=10,
							parallel=FALSE,plot=TRUE,...){
		
		.local<-function(var.id,...){
			boot.model(algorithm=algorithm,data=data[,var.id],y=y,ncomp=ncomp,return=return,R=R,...)[[1]] # no boot obj returned
		}
		
		if (parallel == TRUE){
				check.get.packages(c("snow","doSNOW","foreach"))
				#start cluster
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				
				#
				out<-list()		
				out<-foreach(i=c(1:length(feature.subset)),.combine="rbind") %dopar% .local(var.id=feature.subset[[i]])
				stopCluster(cl.tmp)	
			} else {
				pb <- txtProgressBar(min = 0, max = length(feature.subset), style = 3)
				out<-do.call("rbind",lapply(1:length(feature.subset),
								function(i){
								setTxtProgressBar(pb, i)
								.local(var.id=feature.subset[[i]])})) #,...
				dimnames(out)<-list(c(1:length(feature.subset)),c("bootstrapped.0.632_RMSEP", "mean.error", "sd.error"))
				close(pb)
			}
			
			#output
			val<-data.frame(feature.set=c(1:nrow(out)),number.of.features=sapply(feature.subset,length),RMSEP_0.632=as.numeric(out[,1]), 
							mean.error = out[,2] )
							
			#calculate loess mins
			get.lo<-function(){
					lo <- loess(RMSEP_0.632 ~ number.of.features, data=val)
					which.min(predict(lo,new.data=val))}
			lo.min<-tryCatch(get.lo(),error=function(e){NULL})		
			
			out<-list()
			out$results<-val
			out$loess.min<-lo.min
			out$RMSEP_0.632.min<-val$number.of.features[which.min(val$RMSEP_0.632)]
			
			if(plot == TRUE){
				check.get.packages(c("ggplot2","reshape"))
				
				plot.obj<-data.frame(melt(val[,-c(1:2)]),features=rep(val$number.of.features,(ncol(val)-2)))
				print(plot.obj)
				#make plot
				p<-ggplot(data=plot.obj, aes(y=value, x=features,group=variable,color=variable, fill=variable)) + 
				xlab("number of features")  
				
				if(length(lo.min)>0){
					p<-p+stat_smooth(level = 0.95,size=.75,alpha=.15,legend=FALSE) + 
					geom_vline(xintercept = out$RMSEP_0.632.min,lty=2,col="red") +
					ggtitle(paste("minimum at ",out$RMSEP_0.632.min," features"))+ 
					geom_point(size=3,alpha=.75)  
				} else {
					p<-p+geom_point(size=3,alpha=.75)
				}
				print(p)
			}
			return(out)
}

#make bar plot for weights or loadings
feature.bar.plot<-function(feature.set,weights.set,extra.plot=NULL){
	#feature.set = index for selected features
	#all weights to plot as a bar graph
	#show.all determines if only selected or all features are ploted
		
		#Bargraphs
		#custom theme
		.theme<- theme(
							axis.line = element_line(colour = 'gray', size = .75), 
							panel.background = element_blank(), 
							#legend.position = "none",
							plot.background = element_blank()
						 )
		#plotting data
		bound<-data.frame(weights=weights.set,
						variable=c(1:length(weights.set)),
						show = c(1:length(weights.set))%in%feature.set)
		
		#cut offs
		cuts<-range(bound$weights[!bound$show])
		plot.title<- paste ("upper/lower bounds = ", signif(cuts[2],4), " / " ,signif(cuts[1],4))
		#plot.colors<-scale_fill_brewer(palette="Blues")
		
		#make plot of variable and weight
		p<-ggplot(data=bound, aes(x=variable,y=weights, fill=show)) +
		geom_bar(stat = "identity") + #geom_density2d(aes(group=groups))+
		.theme +geom_hline(yintercept = cuts,lty=2,col="red") +
		 labs(title = plot.title, fill= "Selected") #+
		#plot.colors
	
		
		# sorted weight
		sorted.bound<-bound[order(bound$weights),]
		sorted.bound$variable<-c(1:length(bound$weights))
		
		#theme
		.theme2<- theme(
			axis.line = element_line(colour = 'gray', size = .75), 
			panel.background = element_blank(), 
			legend.position = "none",
			plot.background = element_blank()
		 )		
						 
		p2<-ggplot(data=sorted.bound, aes(x=variable,y=weights, fill=show)) +
		geom_bar(stat = "identity") + xlab(" ") + #geom_density2d(aes(group=groups))+
		.theme2 + geom_hline(yintercept = cuts,lty=2,col="red")# +
		#plot.colors
		#print(p2)
		
		#print plots
		if(is.null(extra.plot)){
				multiplot (p,p2, plotlist=NULL, cols=1)		
			} else {
				multiplot (extra.plot,p,p2, plotlist=NULL, cols=1)	
			}
	}

#create S-plot for variable loadings and conduct significance testing woth FDR to determine optimal feature selection cut off
#use plot.S.plot to plot the results of PLS.feature.select
make.S.plot<-function(pls.data,pls.scores,pls.loadings, cut.off=0.05, FDR=TRUE,plot=TRUE,...){
	
	check.get.packages("ggplot2")
	
	#pls.data 	= data used for model
	#scores 	= scores for selected (1st) component
	#loadings 	= loadings for selected (1st) component
	#plot	 	= make S-Plot
	#cut.off 	= select optimal features based on a test of the significance of the correlation (pearsons) between variable and scores
	#FDR 		= use q-value as the cut off 
	#... 		= can specify correlation type = c("pearson","spearman","biweight")
	
	# calculate p(corr) or correlation between scores and the original variable
	cor.mat<-devium.calculate.correlations(cbind(pls.scores,pls.data),...) #
	corrs<-cor.mat$cor[-1,1]
	p.vals<-cor.mat$p.value[-1,1]
	
	#false discovery rate correction
	if(FDR==TRUE){
			#p.vals<-FDR.adjust(p.vals,type="pvalue",return.all=TRUE)$qval # 
			p.vals<-p.adjust(p.vals, method="BH")
		}
		
	#index to draw visualization	
	show<-p.vals
	show[]<-1
	if(is.numeric(cut.off)){
			show[p.vals>cut.off]<-0
		} 
	
	#make plot	
	plot.obj<-data.frame(pcorr=corrs,loadings=pls.loadings,value=p.vals, significant=as.logical(show))
	
	if(plot==TRUE){
		#theme
		.theme<- theme(
							axis.line = element_line(colour = 'gray', size = .75), 
							panel.background = element_blank(), 
							#legend.position = "none",
							plot.background = element_blank()
						 )
		
		#cut offs
		selected<-plot.obj$significant==1
		plot.title<- paste (sum(selected)," selected features or ",round(sum(selected)/length(pls.loadings)*100,0),"%",sep="")
		
		
		#make plot of variable and weight
		p<-ggplot(data=plot.obj, aes(x=loadings,y=pcorr, color=significant)) +
		geom_point(stat = "identity",alpha=.75) + #geom_density2d(aes(group=groups))+
		.theme + labs(title = plot.title, fill= "Selected") 
		print(p)
	} else { p<-"NULL"}
	
	return(list(plot=p,selected.data=pls.data[,plot.obj$significant==1],feature.info=plot.obj))
}

#create S-plot based on PLS.feature select results
plot.S.plot<-function(obj,names=NULL,return=c("all","splot","barplot","top"),extra=NULL, plot=TRUE){
	
	check.get.packages(c("ggplot2","gridExtra"))
	
	#model.weight 			= x cut
	#pcorr 					= correlation between scores and variables (y cut)
	#combined.selection 	= selected features
	

	#make plot	
	plot.obj<-data.frame(names=rownames(obj),pcorr=obj$pcorr,loadings=obj$model.weight, significant=obj$combined.selection)
	
	.theme<- theme(
						axis.line = element_line(colour = 'gray', size = .75), 
						panel.background = element_blank(), 
						#legend.position = "none",
						plot.background = element_blank()
					 )
		
	#cut offs
	selected<-plot.obj$significant==1
	plot.title<- paste (sum(selected)," selected features or ",round(sum(selected)/length(plot.obj$loadings)*100,0),"%",sep="")
		
		
	#make S plot of variable and weights
	p1<-ggplot(data=plot.obj, aes(x=loadings,y=pcorr, color=significant)) +
	geom_point(stat = "identity",alpha=.75,show_guide=FALSE,size=5) + #geom_density2d(aes(group=groups))+
	.theme + labs(title = plot.title, fill= "Selected") +extra
	
	#
	#feature.set = index for selected features
	#all weights to plot as a bar graph
	#show.all determines if only selected or all features are ploted
	
	#plotting data (redundant object!, need to stay consistent with names to avoid this idiocy)
	bound<-data.frame(name=plot.obj$name,weights=plot.obj$loadings,
					variable=c(1:length(plot.obj$loadings)),
					show = plot.obj$significant)
		
	#cut offs
	tmp<-bound$weights[bound$show]
	tmp<-split(tmp,sign(tmp))
	
	cuts<-c(max(tmp[['1']]),min(tmp[['-1']]))
	plot.title<- paste ("upper/lower bounds = ", signif(cuts[2],4), " / " ,signif(cuts[1],4))
	#plot.colors<-scale_fill_brewer(palette="Blues")
	
	#make plot of variable and weight
	p2<-ggplot(data=bound, aes(x=variable,y=weights, fill=show)) +
	geom_bar(stat = "identity") + #geom_density2d(aes(group=groups))+
	.theme +geom_hline(yintercept = cuts,lty=2,col="red") +
	 labs(title = plot.title, fill= "Selected") +extra #+
		#plot.colors
	
		
	# sorted weight and show names in vertical bar plot (selected only)
	sorted.bound<-bound[order(bound$weights,decreasing=FALSE),]
	sorted.bound$index<-1:nrow(sorted.bound)
	#remove unused factor levels
	sorted.bound<-sorted.bound[sorted.bound$show==TRUE,,drop=FALSE]
	if(nrow(sorted.bound)>0){
		sorted.bound$name<-fixlc(sorted.bound$name)
		sorted.bound$index<-1:nrow(sorted.bound)
		p3<-ggplot(sorted.bound, aes(x = index, y = weights, fill = show))+
		geom_bar(stat = "identity",show_guide=FALSE,fill="gray") + xlab(" ") + #geom_density2d(aes(group=groups))+
		.theme + geom_hline(yintercept = cuts,lty=2,col="red") + coord_flip() +
		scale_x_continuous(breaks=c(1:length(sorted.bound$name)),labels=fixlc(sorted.bound$name)) + extra	
	} else {p3<-ggplot(sorted.bound, aes(x = index, y = weights, fill = show))+geom_bar(stat = "identity",show_guide=FALSE,fill="gray")}	
	

	#plot
	if(plot){
		switch(return,
		"all" = print(grid.arrange(p1,p2,p3, ncol = 1)),
		"splot" = print(p1),
		"barplot" = print(p2) ,
		"top" = print(p3)
		)
	} else {
		switch(return,
		"all" = list(tmp.list),
		"splot" = p1,
		"barplot" = p2 ,
		"top" = p3
		)
	}	
	
}

#feature select using a combination of analyte correlation to scores (S-plot) and feature weights
PLS.feature.select<-function(pls.data,pls.scores,pls.loadings,pls.weight,plot=TRUE,p.value=0.05, FDR=TRUE,
		cut.type="quantile",top=0.95,separate=TRUE,...){
		#combined args from
		#feature.cut() & make.S.plot()
		#cuts is a single value which is a propability for type = quantile or integer for number
		
		#first selection criteria based on magnitude of model weight
		# weight.cut<-feature.cut(obj=pls.weight,type=cut.type,cuts=top,separate=separate,plot=FALSE)
		weight.cut<-feature.cut2(obj=pls.weight,type=cut.type,thresh=top,separate=separate)
		weight.cut.selected<-data.frame(selected.weights=rep(0,length(pls.weight)))
		weight.cut.selected[unlist(weight.cut),]<-1
		
		#second selection criteria based on variable correlation with scores
		cor.cut<-make.S.plot(pls.data=pls.data,pls.scores=pls.scores,pls.loadings=unname(pls.loadings),cut.off=p.value, FDR=FDR,plot=FALSE,...)
		
		#combine and plot
		combo.cut<-data.frame(model.weight=unname(pls.weight),weight.cut.selected, cor.cut$feature.info)
		combo.cut$combined.selection<-combo.cut$significant&combo.cut$selected.weights==1
		
		#return results!!! check
		# invisible(as.data.frame(combo.cut))
		
		if(plot){
			#create updated S-plot
			plot.obj<-combo.cut
			selected<-plot.obj$combined.selection==1
			plot.title<- paste (sum(selected)," selected features or ",round(sum(selected)/length(pls.loadings)*100,0),"%",sep="")
			
			#theme
			.theme<- theme(
							axis.line = element_line(colour = 'gray', size = .75), 
							panel.background = element_blank(), 
							legend.position = "none",
							plot.background = element_blank()
						 )
		
			
			#make plot of variable and weight
			p<-ggplot(data=plot.obj, aes(x=loadings,y=pcorr, color=combined.selection)) +
			geom_point(stat = "identity",alpha=.75) + #geom_density2d(aes(group=groups))+
			.theme + labs(title = plot.title, fill= "Selected")
			
			
			#plot results
			feature.bar.plot(feature.set=c(1:length(combo.cut$model.weight))[combo.cut$combined.selection],weights.set=combo.cut$model.weight, extra.plot=p)
		}
		
		#return results
		# return(as.data.frame(combo.cut))
		return(invisible(as.data.frame(combo.cut)))
	}	

#permute PLS model
permute.PLS<-function(data,y,n=10,ncomp,...){# could be made parallel
	#permuted Y
	perm.y<-lapply(1:n,function(i)
			{
				apply(y,2,gtools::permute)
			})

	#generate permuted models		
	model<-lapply(1:n,function(i)
			{
				# cat("permuting model",i,"\n")
				model<-make.PLS.model(y=perm.y[[i]],data,ncomp=ncomp,...)
				#get stats
				q2<-R2(model)$val[,,ncomp+1]
				rx2<-drop(model$Xvar/model$Xtotvar)[ncomp]
				pred.val<-model$fitted.values[,,ncomp]
				rmsep<-pls::RMSEP(model)$val[2,,ncomp] # take CV adjuste RMSEP
				list(Q2=q2,RX2=rx2,RMSEP=rmsep)#,predicted=pred.val,actual=perm.y[[i]])
			})
	
	tmp<-matrix(unlist(do.call("rbind",model)),ncol=3) 
	colnames(tmp)<-c("Q2","Xvar","RMSEP")
	means<-apply(tmp,2,mean)
	sds<-apply(tmp,2,sd)
	summary<-paste(signif(means,4)," ± ", signif(sds,3))
	return(list(permuted.values=tmp, mean = means, standard.deviations = sds, summary = summary))
}	

#permute OSC-PLS model
permute.OSC.PLS<-function(data,y,n=10,ncomp,OSC.comp=1,train.test.index=NULL,...){ # should be made parallel
	
	#only using first Y
	# message("Only first Y permuted")
	#permuted Y
	perm.y<-lapply(1:n,function(i)
			{
				apply(y[,1,drop=FALSE],2,gtools::permute)
			})
			
	#collect correlation between y and permuted y
	# disabled for multi y 
	if(ncol(y)==1){
		cor.with.y<-data.frame(correlation=abs(cor(cbind(y,do.call("cbind",perm.y))))[-1,1])
	} else {
		cor.with.y<-NULL
	}	
	
	#generate permuted models		
	model<-sapply(1:n,function(i)
			{
				# cat("permuting model",i,"\n")
				if(!is.null(train.test.index)) {tmp.train.test.index<-train.test.index[,i,drop=FALSE]} else {tmp.train.test.index<-train.test.index}
				#if train data contains all the same value then this will cause an error for OPLS
				model<-make.OSC.PLS.model(pls.y=as.matrix(perm.y[[i]]),pls.data=data,comp=ncomp,OSC.comp=OSC.comp,train.test.index=tmp.train.test.index,...) #,...
				tmp.OSC.comp<-max(model$OSC.LVs)
				#get stats
				q2<-model$Q2[[tmp.OSC.comp+1]][ncomp+1,,drop=FALSE]# cv adjusted 
				rx2<-round(sum(model$Xvar[[tmp.OSC.comp+1]])*100,1)
				pred.val<-as.matrix(model$fitted.values[[tmp.OSC.comp+1]][,,ncomp])
				rmsep<-model$rmsep[[tmp.OSC.comp+1]]# take CV adjusted internal RMSEP (see true RMSEP below)
				if(!is.null(train.test.index)) {
					rmsep<-model$predicted.RMSEP[[tmp.OSC.comp+1]] 
				}
				if(!is.null(model$OPLSDA.stats)){oplsda.stats<-data.frame(model$OPLSDA.stats[[tmp.OSC.comp+1]])} else {oplsda.stats<-data.frame(empty=NA)}
				
				data.frame(RX2=rep(rx2,length(q2)),Q2=q2,RMSEP=rmsep,oplsda.stats)#,predicted=pred.val,actual=perm.y[[i]])
			})
	#return results 
	tmp<-as.matrix(t(model[!rownames(model)=="empty",]))
	names<-colnames(tmp)
	tmp<-matrix(unlist(tmp),ncol=ncol(tmp))#needs to be atomic has to be a better way
	colnames(tmp)<-names
	means<-apply(tmp,2,mean, na.rm=TRUE)
	sds<-apply(tmp,2,sd,na.rm=TRUE)
	summary<-matrix(paste(signif(means,4),"+/-", signif(sds,3)),ncol=length(sds))
	colnames(summary)<-colnames(tmp)
	return(list(permuted.values=cbind(tmp,cor.with.y), mean = means, standard.deviations = sds, summary = summary))
}	

#permute OSC-PLS model (in progress version for multiple Ys)
permute.OSC.PLS2<-function(data,y,n=10,ncomp,OSC.comp=1,train.test.index=NULL,...){ # should be made parallel
	
	
	#permuted Y
	perm.y<-lapply(1:n,function(i)
			{
				apply(y,2,gtools::permute)
			})
			
	#collect correlation between y and permuted y\
	# disa bled for multi y until main fxn can handle these correctly
	if(ncol(y)==1){
		cor.with.y<-data.frame(correlation=abs(cor(cbind(y,do.call("cbind",perm.y))))[-1,1])
	} else {
		cor.with.y<-NULL
	}	
	
	#generate permuted models		
	model<-lapply(1:n,function(i)
			{
				# cat("permuting model",i,"\n")
				if(!is.null(train.test.index)) {tmp.train.test.index<-train.test.index[,i,drop=FALSE]} else {tmp.train.test.index<-train.test.index}
				model<-make.OSC.PLS.model(pls.y=as.matrix(perm.y[[i]]),pls.data=data,comp=ncomp,OSC.comp=OSC.comp,train.test.index=tmp.train.test.index,...) #
				#get stats
				q2<-model$Q2[[OSC.comp+1]][ncomp+1,,drop=FALSE]# cv adjusted 
				rx2<-round(sum(model$Xvar[[OSC.comp+1]])*100,1)
				pred.val<-as.matrix(model$fitted.values[[OSC.comp+1]][,,ncomp])
				rmsep<-model$rmsep[[OSC.comp+1]]# take CV adjusted internal RMSEP (see true RMSEP below)
				if(!is.null(train.test.index)) {
					rmsep<-model$predicted.RMSEP[[OSC.comp+1]] 
				}
				list(RX2=rep(rx2,length(q2)),Q2=q2,RMSEP=rmsep)#,predicted=pred.val,actual=perm.y[[i]])
			})
	
	tmp<-matrix(unlist(do.call("rbind",model)),ncol=ncol(y)*3 )
	colnames(tmp)<-paste0("Y.",1:ncol(y),"_",rep(c("Xvar","Q2","RMSEP"),each=ncol(y)))
	means<-apply(tmp,2,mean, na.rm=TRUE)
	sds<-apply(tmp,2,sd,na.rm=TRUE)
	summary<-paste(signif(means,4),"±", signif(sds,3))
	return(list(permuted.values=cbind(tmp,cor.with.y), mean = means, standard.deviations = sds, summary = summary))
}	

#statistical test to compare permuted distribution to model performance
OSC.validate.model<-function(model, perm, train= NULL, test="t.test",...) {
#model is an object generated with get.OSC.model
#perm must be object generated with permute.OSC.PLS
# if train = NULL  and test = "t.test" perform a one-sample t-test to test if model stat comes from permuted distribution
# else perform a two sample t-test to compare train/test to permuted stats
# if test= "perm.test" calculate p-value based on http://www.ncbi.nlm.nih.gov/pubmed/21044043
# number tests different than the permuted +1/ number of permutations + 1
        
        #match model and perm objects
        perm.vals<-perm$permuted.values
        comp<-length(model$Q2)
        if(is.null(train)){
                if(any(colnames(perm.vals)=="AUC")){
                        mod.vals<-data.frame(RX2 = cumsum(model$Xvar*100)[comp-1],
                                                                Q2 = model$Q2[comp],
                                                                RMSEP = model$RMSEP[comp],model$OPLSDA.stats)[1,] 
                } else {
                        mod.vals<-data.frame(RX2 = cumsum(model$Xvar*100)[comp-1],
                                                                Q2 = model$Q2[comp],
                                                                RMSEP = model$RMSEP[comp])[1,] 
                }
        } else {
                mod.vals<-data.frame(train$performance) # from train object
        } 
        
        #test single model value (should test log distributions to avoid the effect of outliers?)
        single.test<-function(mod,perm){
                data.frame(matrix(tryCatch(t.test(perm,mu=unlist(mod))$p.value, error=function(e) {1}),ncol=1))
        }
        
        #two group test
        group.test<-function(mod,perm){
                data.frame(matrix(tryCatch(t.test(mod,perm)$p.value, error=function(e) {1}),ncol=1))
        }
        
        if(is.null(train)){
                p.vals<-lapply(1:ncol(mod.vals),function(i){
                        if(names(mod.vals[i])=="RMSEP") {dir<-">"} else {dir<-"<"} # used for permutation tests
                        switch(test,
                                "t.test"        = single.test(mod=mod.vals[i],perm=perm.vals[,i]),
                                "perm.test" = perm.test(mod=mod.vals[i],perm=perm.vals[,i],dir,type=1),
								"perm.test2" = perm.test(mod=mod.vals[i],perm=perm.vals[,i],dir,type=2))        
                })
                
                if(is.null(perm$summary)){perm$summary<-"not permuted"}
                #make output in table form
                res<-data.frame(rbind(signif(mod.vals,4),perm$summary,unname(signif(unlist(p.vals),4))))
                rownames(res)<-c("model","permuted model","p-value")
        } else {
                p.vals<-lapply(1:ncol(mod.vals),function(i){
                        if(names(mod.vals[i])=="RMSEP") {dir<-">"} else {dir<-"<"} # used for permutation tests
                        switch(test,
                                "t.test"        = group.test(mod=mod.vals[,i],perm=perm.vals[,i]),
                                "perm.test" = perm.test(mod=mod.vals[i],perm=perm.vals[,i],dir),
								"perm.test2" = perm.test(mod=mod.vals[i],perm=perm.vals[,i],dir,type=2))        
                        
                })
                
                if(is.null(perm$summary)){perm$summary<-"not permuted"}
                #make output in table form
                res<-data.frame(rbind(train$summary,perm$summary,unlist(signif(unlist(p.vals),4))))
                rownames(res)<-c("model","permuted model","p-value")
        }
        return(res)
}

#conservative p-value based on permutation tests  http://www.ncbi.nlm.nih.gov/pubmed/21044043 (could go elsewhere)
perm.test<-function(mod,perm,compare="<",type=1){
		f<-function(mod,perm,compare){(sum(do.call(compare,list(na.omit(mod),na.omit(perm))))+1)/(mean(length(na.omit(perm)),length(na.omit(mod)))+1)}
		if(type==1){f(mod,perm,compare)} else {f(mod,perm,compare=compare)-1/(mean(length(na.omit(perm)),length(na.omit(mod)))+1)}
	}

#compare train stats between two models
OSC.PLS.model.compare<-function(model1, model2,test="t.test",...){
		#models must be object generated with OSC.PLS.train.test
		
		p.vals<-do.call("cbind",lapply(1:ncol(model1$performance), function(i) {	
			if(colnames(model1$performance)[i]=="RMSEP") {dir<-">"} else {dir<-"<"}
			switch(test,
			
				"t.test" 	= tryCatch(t.test(model1$performance[,i],model2$performance[,i])$p.value, error=function(e) {1}), #force error = insiginificant 
				"perm.test" = perm.test(model1$performance[,i],model2$performance[,i],compare=dir),
				"perm.test2" = perm.test(model1$performance[,i],model2$performance[,i],compare=dir,type=2)
				)
			})	
		)
		
		res<-data.frame(rbind(model1$summary,model2$summary,signif(p.vals,4))) # don't include Xvar
		dimnames(res)<-list(c("model1","model2","p-value"),colnames(model1$summary))
		return(res)
}

#conduct train/test validations on PLS model
PLS.train.test<-function(pls.data,pls.y,pls.train.index,comp,...) 
	{
		#only test first Y
		
		pls.y<-as.matrix(pls.y)
		#order for merging test with train stats in one object
		new.order<-c(c(1:nrow(pls.data))[pls.train.index=="train"],c(1:nrow(pls.data))[pls.train.index=="test"])
		back.sort<-order(new.order)

		train.y<-train.real<-pls.y[pls.train.index=="train",]
		train.data<-pls.data[pls.train.index=="train",]
		#all arguments gave been preset elsewhere
		test.pls.results<-make.PLS.model(train.y,train.data,ncomp=comp,...)
		Q2<-unlist(R2(test.pls.results)$val[,,max(comp)+1])

		train.pred<-test.pls.results$fitted.values[,,comp]
		
		#use model to predict test values
		test.data<-as.matrix(pls.data[pls.train.index=="test",])
		test.pred<- as.data.frame(predict(object=test.pls.results, newdata=test.data, ncomp = c(1:comp), comps=c(1:comp),type ="response"))

		test.real<-pls.y[pls.train.index=="test",]
		RMSEP<-sapply(1:ncol(pls.y), function(i){
			(sum((test.pred[,i]-test.real[,i])^2)/nrow(test.pred))^.5
		})

		#results
		predicted.y<-rbind(train.pred,test.pred)
		actual.y<-rbind(train.real,test.real)
		test.index<-pls.train.index
		res<-list(predicted.y[back.sort,], actual.y[back.sort,], test.index,RMSEP,Q2,LVs=PCs)
		names(res)<-c("predicted.y","actual.y=","pls.train.index","RMSEP","Q2","LVs")
		return(res)
	}

#conduct train/test validations on O-PLS model	
OSC.PLS.train.test<-function(pls.data,pls.y,train.test.index,comp,OSC.comp,...) 
	{
		pls.y<-as.matrix(pls.y)
		results<-lapply(1:ncol(train.test.index), function(i){
			pls.train.index<-as.matrix(train.test.index[,i])
			#order for merging test with train stats in one object
			new.order<-c(c(1:nrow(pls.data))[pls.train.index=="train"],c(1:nrow(pls.data))[pls.train.index=="test"])
			back.sort<-order(new.order)

			train.y<-train.real<-pls.y[pls.train.index=="train",]
			train.data<-pls.data[pls.train.index=="train",]
			test.real<-pls.y[pls.train.index=="test",]
			#all arguments gave been preset elsewhere
			test.pls.results<-make.OSC.PLS.model(pls.y=pls.y,pls.data=pls.data,comp=comp,OSC.comp=OSC.comp, train.test.index=pls.train.index,...)
			tmp.OSC.comp<-max(test.pls.results$OSC.LVs) # control when limited with Orthogonal dimensions
			Q2<-data.frame(Q2=test.pls.results$Q2[[tmp.OSC.comp+1]][comp,])
			
			#fitted values
			train.pred<-test.pls.results$fitted.values[[tmp.OSC.comp+1]][,,comp]
			test.pred<-test.pls.results$predicted.Y[[tmp.OSC.comp+1]]
			
			RMSEP<-data.frame(RMSEP=test.pls.results$predicted.RMSEP[[tmp.OSC.comp+1]])
			Xvar<-data.frame(Xvar=round(sum(test.pls.results$Xvar[[tmp.OSC.comp+1]])*100,1))
			if(!is.null(test.pls.results$OPLSDA.stats)){oplsda.stats<-data.frame(test.pls.results$OPLSDA.stats[[tmp.OSC.comp+1]])} else {oplsda.stats<-NULL} 
		
			#results
			predicted.y<-rbind(as.matrix(train.pred),as.matrix(test.pred))
			actual.y<-rbind(as.matrix(train.real),as.matrix(test.real))
			test.index<-pls.train.index
			if(is.null(oplsda.stats)){
					res<-list(data.frame(predicted = predicted.y[back.sort,], actual = actual.y[back.sort,],train.test.id=test.index), data.frame(Xvar,Q2,RMSEP))
				} else {
					res<-list(data.frame(predicted = predicted.y[back.sort,], actual = actual.y[back.sort,],train.test.id=test.index), data.frame(Xvar,Q2,RMSEP,oplsda.stats))
				}
			return(res)
		})
		
		#need to summarize results
		id<-c(1:length(results))[c(1:length(results))%%2==0]
		aggregated<-do.call("rbind",lapply(1:length(results),function(i){data.frame(results[[i]][2])}))
		
		aggregated.summary<-matrix(paste(signif(apply(aggregated,2,mean,na.rm=TRUE),4),"+/-",signif(apply(aggregated,2,sd,na.rm=TRUE),3)),nrow=1)
		colnames(aggregated.summary)<-colnames(aggregated)
		list(full.results=results, performance=aggregated, summary=aggregated.summary)
	}	

# function for splitting dataset in to test and trainning sets
test.train.split<-function(nsamples, n=1, strata=NULL, prop.train=2/3, split.type="random",data=NULL){
	#nsamples the number of samples in the data set to split among test and trainnig data sets
	#n the number ot test/training splits to return
	#strata factor within whose levels the test/trainning sets will be derived
	#prop.train the proportion of samples in the trainning set
	#split.type how sample assignment to trainning/test splits is determined,  the options are "random", "duplex"
	#data needed for duplex method
	res<-lapply(1:n,function(i){
		if(is.null(strata)){
			if(split.type=="random"){
				t.num<-ceiling(nsamples*prop.train)
				test.train.id<-rep("test",nsamples)
				test.train.id[sample(c(1:length(test.train.id)),t.num)]<-"train"
			}
			
			if(split.type=="duplex"){ # takes the floor, if the sample number is not even the proportion of trainning samples may be one less than specified
				object<-ken.sto2(data, per = "TRUE",per.n= (1-prop.train),va = "TRUE",num=1)
				object<-duplex.select(data,object,percent.in.test=(1-prop.train))	
				test.train.id<-rep("train",nrow(data))
				test.train.id[object$`Chosen validation row number`]<-"test"	
			}	
		} else {
			if(split.type=="random"){
				tmp<-split(1:nsamples,strata)
				train.id<-unlist(sapply(1:length(tmp),function(i){
						t.num<-ceiling(length(tmp[[i]])*prop.train)
						tmp[[i]][sample(c(1:length(tmp[[i]])),t.num)]
					}))
				test.train.id<-rep("test",nsamples)
				test.train.id[train.id]<-"train"
			}
			
			if(split.type=="duplex"){ # trainning/test assignments are underestimated for odd number of samples (due rounding down)
				tmp<-split(data,strata)
				test.id<-fixlc(sapply(1:length(tmp),function(i){
					object<-ken.sto2(tmp[[i]], per = "TRUE",per.n= (1-prop.train),va = "TRUE",num=1)
					object<-duplex.select(tmp[[i]],object,percent.in.test=(1-prop.train))	
					object$`Chosen validation sample names`
					}))
				test.train.id<-rep("train",nrow(data))
				test.train.id[rownames(data)%in%test.id]<-"test"	
			}	
				
		}
		test.train.id
	})
	as.data.frame(do.call("cbind",res))
}	

#function for carrying out test/trainning split based on duplex or kennard-stone method 
ken.sto2<-function(inp, per = "TRUE", per.n = 0.3, num = 7, va = "TRUE")
 {
	#based on  ken.sto in package "soil.spec"
	#changes: altered number of PCs selection
	#took out saving, plotting
	#opened slot for custom PCA analysis options
	#fixed some bugs 

    if (class(inp) != "data.frame" & class(inp) != "matrix") 
	{
        stop("Invalid argument: 'inp' has to be of class 'data.frame' or 'matrix'.")
    	}
    if (per != "TRUE" & per != "FALSE") 
	{
        stop("Invalid argument: 'per' has to be either 'TRUE' or 'FALSE'.")
    }

    if (per == "TRUE")
	 {
        if (class(per.n) != "numeric") 
		{
            stop("Invalid argument: 'per' has to be of class 'numeric'.")
        	}

        if (per.n < 0 | per.n > 1) 
		{
            stop("Invalid argument: 'per' has to be between 0 and 1.")
        	}
        n <- round(per.n * nrow(inp), 0)
    }

    if (per == "FALSE") 
	{
        if (class(as.integer(num)) != "integer") 
		{
            	stop("Invalid argument: 'num' has to be of class 'integer'.")
       	 }

        if (num <= 0) 
	 {
            stop("Invalid argument: 'num' has to be between 1 and the number of samples minus one.")
        }

        if (num >= nrow(inp)) 
		{
           	 stop("Invalid argument: 'num' has to be between 1 and the number of samples minus one.")
        	}

        n <- num
   	 }
    if (va != "TRUE" & va != "FALSE") 
	{
        stop("Invalid argument: 'va' has to be either 'TRUE' or 'FALSE'.")
   	 }
   
    #allow to use specified PCA analysis results
    pca <- prcomp(inp, scale = T)
    prco <- as.data.frame(pca$x)
    cpv <- summary(pca)[[6]][3,]
    zzz <- matrix(nrow = 1, ncol = (length(cpv)-2))
    for (i in 1:(ncol(zzz)-2)) 
	{
        e <- (cpv[i] + 0.04) < cpv[i + 3]
        zzz[i] <- e
    	}
    pc <- (which(zzz == FALSE) - 1)[1]
    if (pc == 1|is.na(pc)) 
	{
        pc <- 2
    	}
    prco <- prco[, 1:pc]
    min <- c(rep(1, ncol(prco)))
    max <- c(rep(1, ncol(prco)))
    for (i in 1:ncol(prco)) 
	{
        blub <- which(prco[, i] == min(prco[, i]))
        min[i] <- blub[1]
        bla <- which(prco[, i] == max(prco[, i]))
        max[i] <- bla[1]
   	 }
    min <- rownames(prco)[min]
    max <- rownames(prco)[max]
    start <- unique(c(min, max))
    start.n <- match(start, rownames(inp))

    if (va == "FALSE") 
    {
        euc <- as.data.frame(as.matrix(dist(prco)))
        inp.start <- rownames(prco)[-start.n]
        inp.start.b <- inp.start
        cal <- start
	  stop<-min(c(n,length(start)))
        for (k in 1:(stop)) 
		{
            test <- apply(euc[inp.start.b, cal], 1, min)
            bla <- names(which(test == max(test)))
            cal <- c(cal, bla)
            inp.start.b <- inp.start.b[-(which(match(inp.start.b, 
                bla) != "NA"))]
        	}
       cal.n <- match(cal, rownames(inp))
	
       output <- list(`Calibration and validation set` = va, 
            `Number important PC` = pc, `PC space important PC` = prco, 
            `Chosen sample names` = unique(cal), `Chosen row number` = unique(cal.n), 
            `Chosen calibration sample names` = "NULL", `Chosen calibration row number` = "NULL", 
            `Chosen validation sample names` = "NULL", `Chosen validation row number` = "NULL")
    }

    if (va == "TRUE") 
    {
	  n<-ceiling(per.n*nrow(inp))
        cal.start <- start
        cal.start.n <- start.n
        val.min <- c(rep(1, ncol(prco)))
        val.max <- c(rep(1, ncol(prco)))
        for (i in 1:ncol(prco)) 
		{
            blub <- which(prco[-cal.start.n, i] == min(prco[-cal.start.n, 
                i]))
            val.min[i] <- blub[sample(length(blub), 1)]
            bla <- which(prco[-cal.start.n, i] == max(prco[-cal.start.n, 
                i]))
            val.max[i] <- bla[sample(length(bla), 1)]
       	 }
        val.min <- rownames(prco[-cal.start.n, ])[val.min]
        val.max <- rownames(prco[-cal.start.n, ])[val.max]
        val.start <- unique(c(val.min, val.max))
        val.start.n <- match(val.start, rownames(inp))
        cal.val <- c(cal.start, val.start)
        cal.val.start <- match(c(cal.start, val.start), rownames(inp))
        euc <- as.data.frame(as.matrix(dist(prco)))
        inp.start <- rownames(prco)[-cal.val.start]
        inp.start.b <- inp.start
        val <- val.start
	  stop<-n#min(c(n,length(val.start)))
	  k<-1
        for (k in 1:(stop)) 
		{
            test <- apply(euc[inp.start.b, val], 1, min)
            bla <- names(which(test == max(test)))
            val <- c(val, bla)
            inp.start.b <- inp.start.b[-(which(match(inp.start.b, 
                bla) != "NA"))]
       	 }
        val.n <- match(val, rownames(inp))
        cal.n <- c(1:nrow(inp))[-val.n]
        cal <- rownames(inp)[cal.n]
        
		#tmp fix for problem in function
		n<-ceiling(per.n*nrow(inp))
		if(n<1){n<-1}
		tst.id<-unique(val.n)
		if(n>length(tst.id)){n=length(tst.id)}
		val.n<-sample(tst.id,n)
		cal.n<-c(cal.n,tst.id[!tst.id%in%val.n])

		cal <- rownames(inp)[cal.n]
		val<-rownames(inp)[val.n]



        output <- list(`Calibration and validation set` = va, 
            `Number important PC` = pc, `PC space important PC` = prco, 
            `Chosen sample names` = "NULL", `Chosen row number` = "NULL", 
            `Chosen calibration sample names` = unique(cal), `Chosen calibration row number` = unique(cal.n), 
            `Chosen validation sample names` = unique(val), `Chosen validation row number` = unique(val.n))
	}
        class(output) <- "ken.sto"
        return(output)
    }

#wrapper to iterate ken.sto2
duplex.select<-function(data,ken.sto2.obj,percent.in.test)
	{
	#determine how many more are needed
	start.have<-ken.sto2.obj$`Chosen validation sample names`
	need<-percent.in.test*nrow(data)-length(start.have)
	
	#don't do anything if there are enough
	if(need>0)
	{
	#extract from remainning data
	have<-start.have
	while(need>0)
		{
			tmp.data<-data[!rownames(data)%in%have,]
			more<-ken.sto2(tmp.data, per = "TRUE", per.n = percent.in.test, num = 7, va = "TRUE")
			now.have<-more$`Chosen validation sample names`
			need<-percent.in.test*nrow(data)-(length(now.have)+length(have))
			have<-c(have,now.have)
		}

	#adjust for too many selected 
	drop<-NA
	if(need<0)
		{
			drop<-now.have[sample(length(now.have),abs(need))]
		}

	new.obj<-have[!have%in%drop]
	
	#objects to return
	`Chosen validation sample names`=c(new.obj)
	`Chosen validation row number`= c(1:nrow(data))[rownames(data)%in%new.obj]
	`Chosen calibration sample names`= rownames(data)[!rownames(data)%in%`Chosen validation sample names`]
	`Chosen calibration row number` =c(1:nrow(data))[rownames(data)%in%`Chosen calibration sample names`]
	}else{
	`Chosen validation sample names`=ken.sto2.obj$`Chosen validation sample names`
	`Chosen validation row number`= ken.sto2.obj$`Chosen validation row number`
	`Chosen calibration sample names`= ken.sto2.obj$`Chosen calibration sample names`
	`Chosen calibration row number` =ken.sto2.obj$`Chosen calibration row number`
	}
	output<-list(`Chosen validation row number`= `Chosen validation row number`,
			 `Chosen validation sample names`=`Chosen validation sample names`,
			 `Chosen calibration sample names` = `Chosen calibration sample names`, 
			 `Chosen calibration row number` = `Chosen calibration row number`)
}

#function to calculate included/excluded feature model stats
optimize.OPLS.feature.select<-function(model,feature.subset,permute=TRUE,train.test.index,progress=TRUE,...){
	#need to know OPLS model args (*later store in model and use this a reference)

	#selected model stats
	data<-model$data[[1]][,feature.subset,drop=F]
	model1<-make.OSC.PLS.model(pls.y=model$y[[1]],pls.data=data,comp=model$total.LVs[1],OSC.comp=max(model$OSC.LVs), validation = model$model.description$validation,method=model$model.description$method, cv.scale=model$model.description$cv.scale,return.obj="stats")
	if(permute==TRUE){
		#permutation
			sel.permuted.stats <- permute.OSC.PLS(data = data, y = model$y[[1]], n = ntests, ncomp = model$total.LVs[1], osc.comp=max(model$OSC.LVs), progress = progress, train.test.index = train.test.index) #...
		} else {
			sel.permuted.stats<-NULL
		}

	#training/testing to get robust model stats
	sel.OPLS.train.stats <- OSC.PLS.train.test(pls.data = data, pls.y = model$y[[1]], train.test.index, comp = model$total.LVs[1], OSC.comp = max(model$OSC.LVs), cv.scale = model$model.description$cv.scale, progress = progress) # ...
	sel.OPLS.model<-OSC.validate.model(model = model1, perm = sel.permuted.stats, train = sel.OPLS.train.stats)

	#excluded model stats
	data<-model$data[[1]][,!feature.subset]
	model2<-make.OSC.PLS.model(pls.y=model$y[[1]],pls.data=data,comp=model$total.LVs[1],OSC.comp=max(model$OSC.LVs), validation = model$model.description$validation,method=model$model.description$method, cv.scale=model$model.description$cv.scale,return.obj="stats")
	if(permute==TRUE){
		#permutation
			ex.permuted.stats <- permute.OSC.PLS(data = data, y = model$y[[1]], n = ntests, ncomp = model$total.LVs[1], osc.comp=max(model$OSC.LVs), progress = progress, train.test.index = train.test.index) #...
		} else {
			ex.permuted.stats<-NULL
		}

	#training/testing to get robust model stats
	ex.OPLS.train.stats <- OSC.PLS.train.test(pls.data = data, pls.y = model$y[[1]], train.test.index, comp = model$total.LVs[1], OSC.comp = max(model$OSC.LVs), cv.scale = model$model.description$cv.scale, progress = progress) # ...
	ex.OPLS.model<-OSC.validate.model(model = model2, perm = ex.permuted.stats, train = ex.OPLS.train.stats)

	full.sel.model.comparison<-OSC.PLS.model.compare(model1=sel.OPLS.train.stats, model2=ex.OPLS.train.stats)
	#create final table
	out<-data.frame(cbind(model=c(rep("selected",3),rep("excluded",3),"comparison"),rbind(as.matrix(sel.OPLS.model),as.matrix(ex.OPLS.model),as.matrix(full.sel.model.comparison)[3,,drop=F])))

	list(selected.train=sel.OPLS.train.stats,selected.permuted=sel.permuted.stats,excluded.train=ex.OPLS.train.stats,excluded.permuted=ex.permuted.stats,summary=out)

}

#get classification performance statistics
O.PLS.DA.stats<-function(truth,pred){
	#
	library(ROCR)
	# library(caret) #need e1071
	library(hmeasure)
	
	
	y.range<-range(as.numeric(truth))
	mid<-mean(y.range)
	binned.pred<-pred
	binned.pred[binned.pred<mid]<-y.range[1]
	binned.pred[binned.pred>=mid]<-y.range[2]
	# scaled.pred<-rescale(as.numeric(pred),y.range)
	# scaled.pred[scaled.pred<mid]<-y.range[1]
	# scaled.pred[scaled.pred>=mid]<-y.range[2] # not sure what to do with a prediction == the mid point
	#get AUC
	mod.AUC<-function(pred,truth){
		# pred1 <- prediction(pred, truth)
		#perf <- performance(pred1, measure="tpr", x.measure="fpr")
		# plot(perf,lty=1,lwd=4,col="#9400D350") # plot not interesting with so  few measurements
		# add precision recall http://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r
		unlist(performance(prediction(pred, truth),measure= "auc")@y.values)
	}
	
	
	#misclassCounts(binned.pred,truth)  # library(hmeasure)
	
	AUC<-tryCatch(mod.AUC(binned.pred,truth),error=function(e){NA}) # protect errors due to !=2 groups
	# get other metrics
	# library(hmeasure) # using modified fxn which accepts inputs other than 1 and 0
	results<-tryCatch(misclassCounts2(binned.pred,truth)$metrics ,error=function(e){NULL}) # protect errors due to >2 groups or use caret::confusionMatrix
	 #happens when there are perfect predictions
	# library(caret)
	# results<-tryCatch(confusionMatrix(binned.pred,as.numeric(truth)),error=function(e){"error"}) # protect errors due to >2 groups
	if(is.null(results)){results<-list();results$byClass[1:2]<-NA}
	# res<-data.frame(AUC=AUC,sensitivity=results$byClass[1], specificity=results$byClass[2])
	res<-data.frame(AUC=AUC,results)
	
	rownames(res)<-"model"
	return(res)	
}

#modified hmeasure::misclassCounts to accept class values besides 0 and 1
# limited to only 2 classes
misclassCounts2<-function (pred,truth){
   
	vals<-sort(unique(truth),decreasing=TRUE) # smaller value is not the class
    TP <- sum(pred == vals[1] & truth == vals[1])
    FP <- sum(pred == vals[1] & truth == vals[2])
    TN <- sum(pred == vals[2] & truth == vals[2])
    FN <- sum(pred == vals[2] & truth == vals[1])
	
    conf.matrix <- data.frame(pred.1 = c(TP, FP), pred.0 = c(FN, 
        TN))
    row.names(conf.matrix) <- c("actual.1", "actual.0")
    ER <- (FP + FN)/(TP + FP + TN + FN)
    Sens <- TP/(TP + FN)
    Spec <- TN/(TN + FP)
    Precision <- TP/(TP + FP)
    Recall <- Sens
    TPR <- Recall
    FPR <- 1 - Spec
    F <- 2/(1/Precision + 1/Sens)
    Youden <- Sens + Spec - 1
    metrics <- data.frame(ER = ER, Sens = Sens, Spec = Spec, 
        Precision = Precision, Recall = Recall, TPR = TPR, FPR = FPR, 
        F = F, Youden = Youden)
    return(list(conf.matrix = conf.matrix, metrics = metrics))
}

# generic mean squared error of prediction
.MSEP<-function(actual,pred){
	if(is.null(dim(actual))){
		mean((actual-pred)^2)	
	} else {
		sapply(1:ncol(actual),function(i){mean((actual[,i]-pred[,i])^2)})
	}
}

#wrapper to carry out multiple feature selection and model validation runs
multi.OPLS.feature.select<-function(model,filter,plot=TRUE,ocomp=max(model$OSC.LVs),extra=NULL,train.test.index=NULL,progress=TRUE,...){
	
	library(plyr)
	library(ggplot2)
	library(gridExtra)
	.model<-get.OSC.model(obj=model,OSC.comp=ocomp)	

	optimal.feature.selection<-lapply(1:length(filter),function(i){
		top<-filter[i]
		#this step can be done only once
		opts<-PLS.feature.select(model$data[[1]],pls.scores=.model$scores[,][,1,drop=F],pls.loadings=.model$loadings[,][,1,drop=F],pls.weight=.model$loadings[,][,1,drop=F],plot=FALSE,p.value=1,FDR=FALSE,cut.type="number",top=top,separate=FALSE)
		optim<-optimize.OPLS.feature.select(model=model,feature.subset=opts$combined.selection,permute=TRUE,train.test.index=train.test.index,progress=progress,...) # check variance explained in X
		if(progress==TRUE){print(paste0(round(i/length(filter)*100,0)," % complete"))}
		rbind(data.frame(vars=top,model="included",optim$selected.train$performance),data.frame(vars=top,model="excluded",optim$excluded.train$performance))
	})

	#summary
	obj<-do.call("rbind",optimal.feature.selection)
	#get median and MAD for all levels
	library(plyr)
	medians<-ddply(obj,.(vars,model),colwise(median,na.rm=TRUE))
	mads<-ddply(obj,.(vars,model),colwise(mad,na.rm=TRUE))
	p.vals<-ddply(obj,.(vars),function(d.sub){multi.mann.whitney(d.sub[,-c(1:2)],factor=data.frame(d.sub$model),progress=progress)$mann.whitney.U.test_p.value})
	colnames(p.vals)[2:ncol(p.vals)]<-colnames(medians)[3:ncol(medians)]
	
	res<-list(all.results=do.call("rbind",optimal.feature.selection),summary=list(median=medians,mad=mads,p.values=p.vals))
	
	if(plot){
		#create a plot
		plot.list<-list()
		k<-1
		for(i in 3:ncol(medians)){
		value<-colnames(medians)[i]
		tmp<-data.frame(value=medians[,value],error=mads[,value],model=medians$model,vars=medians$vars)

		.theme<- theme(
						axis.line = element_line(colour = 'gray', size = .75), 
						panel.background = element_blank(),  
						plot.background = element_blank()
					 )
					 
		vlines<-p.vals$var[p.vals[,value]<=0.05]
		if(length(vlines)>0){show.sig<-geom_vline(xintercept=vlines,linetype="dotted")}	else {show.sig<-NULL}										 
		plot.list[[k]]<-ggplot(data = tmp, aes(x = vars, y = value,group=model) ) + 
			 geom_errorbar(aes(ymin = value - error, ymax = value + error, fill=model,color=model), size=1, width=0.1) + # add error bars (do so before geom_point so the points are on top of the error bars)
			 geom_line(aes(color=model),size=1) + # join points with lines (specify this before geom_point, or the lines will be drawn over the shapes)
			 geom_point(aes(shape=model, fill=model,color=model), size=5) +ylab(value) +
			 xlab("cutoff") + .theme + scale_x_continuous("cutoff", breaks=unique(tmp$vars)) +show.sig +extra
			 k<-k+1
		}
		do.call("grid.arrange",plot.list)
	}	
	return(res)
}	 

plot.multi.OPLS.feature.select<-function(object,extra=NULL,objects=c("RMSEP","Q2","AUC","F","Sens","Spec","Precision","Recall")){
	
		medians<-object$summary$median[,-c(1:3)]
		mads<-object$summary$mad[,-c(1:3)]
		p.vals<-object$summary$p.values[,-c(1:2)]
		model<-object$summary$median$model
		vars<-object$summary$median$vars
		#filter error columns
		f<-function(x){sum(is.na(x))}
		id<-!apply(medians,2,f)==nrow(medians)&colnames(medians)%in%objects #exclude all NA columns
		medians<-medians[,id]
		mads<-mads[,id]
		p.vals<-p.vals[,id]
		
		.theme<- theme(
						axis.line = element_line(colour = 'gray', size = .75), 
						panel.background = element_blank(),  
						plot.background = element_blank()
					 )
		
		plot.list<-list()
		k<-1
		for(i in 1:ncol(medians)){
		value<-colnames(medians)[i]
		tmp<-data.frame(value=medians[,value],error=mads[,value],model=model,vars=vars)
		vlines<-p.vals$var[p.vals[,value]<=0.05]
		if(length(vlines)>0){show.sig<-geom_vline(xintercept=vlines,linetype="dotted")}	else {show.sig<-NULL}										 
		plot.list[[k]]<-ggplot(data = tmp, aes(x = vars, y = value,group=model) ) + 
			 geom_errorbar(aes(ymin = value - error, ymax = value + error, fill=model,color=model), size=1, width=.1) + # add error bars (do so before geom_point so the points are on top of the error bars)
			 geom_line(aes(color=model),size=1) + # join points with lines (specify this before geom_point, or the lines will be drawn over the shapes)
			 geom_point(aes(shape=model, fill=model,color=model), size=5) +ylab(value) +
			 xlab("cutoff") + .theme + scale_x_continuous("cutoff", breaks=unique(tmp$vars)) +show.sig +extra
			 k<-k+1
		}
		do.call("grid.arrange",plot.list)

}	 

#extract best model
best.OPLS.features<-function(obj=res,decreasing = TRUE,measures=c("AUC","Sens","Spec","Precision","Recall","TPR","F","Youden")){
	#rank
	tmp<-obj$summary$median[obj$summary$median$model=="included",]
	.tmp<-tmp[,colnames(tmp)%in%measures]
	.rank<-apply(.tmp,2,rank)
	rmat<-matrix(.rank,,length(measures))
	rowid<-matrix(1:nrow(rmat),nrow(rmat),ncol(rmat))[which.max(rmat)]
	tmp[rowid,,drop=FALSE]
}

#iteratively split a vector into fractional units taking the floor
split.vector<-function(n,step=.5){
	
		#adapted from randomForest::rfcv
		
        k <- floor(log(n, base = 1/step))
        n.var <- round(n * step^(0:(k - 1)))
        same <- diff(n.var) == 0
        if (any(same)) 
            n.var <- n.var[-which(same)]
        if (!1 %in% n.var) 
            n.var <- c(n.var, 1)
		return(n.var)	
}


#various tests
test<-function(){
data(mtcars)

#O-PLS Regression
{
	pls.data<-mtcars[,-1]
	pls.y<-mtcars[,1,drop=F]

	opls.results<-make.OSC.PLS.model(pls.y,pls.data,
							comp=2,
							OSC.comp=1, 
							validation = "LOO", 
							cv.scale = TRUE,
							train.test.index=NULL,
							progress=TRUE)				
							
	final.opls.results<-get.OSC.model(obj=opls.results,OSC.comp=1)		
	(opls.model.text<-data.frame("Xvar"=c(0,round(cumsum(final.opls.results$Xvar)*100,2)),"Q2"=final.opls.results$Q2,"RMSEP"= final.opls.results$RMSEP)	)

	#predict values using training test split
	#test
	ntests<-1
	strata<-NULL # use to control equivalent sampling from groups
	#train/test index
	train.test.index <- test.train.split(nrow(pls.data), n = ntests, strata = strata, split.type = "random", data = pls.data) 
	mods<-make.OSC.PLS.model(pls.y,pls.data,
							comp=2,
							OSC.comp=1, 
							validation = "LOO", 
							cv.scale = TRUE,
							train.test.index=train.test.index,
							progress=FALSE)				
	#view predictions for test data
	final.opls.results2<-get.OSC.model(obj=mods,OSC.comp=1)	
	fitted<-final.opls.results2$predicted.Y
	(RMSEP<-(.MSEP(actual=pls.y[train.test.index=="test",],pred=fitted))^.5)

	# carry out multiple runs of trainning/testing
	ntests<-10
	#train/test index
	train.test.index <- test.train.split(nrow(pls.data), n = ntests, strata = strata, split.type = "random", data = pls.data) 
	multi.train.test<-OSC.PLS.train.test(pls.data = pls.data, pls.y = pls.y, train.test.index, comp = mods$total.LVs[1], OSC.comp = max(mods$OSC.LVs), cv.scale = mods$model.description$cv.scale, progress = TRUE) # ...
	# carry out model permutation testing
	multi.permute<-permute.OSC.PLS(data = pls.data, y = pls.y, n = ntests, ncomp = mods$total.LVs[1], osc.comp=max(mods$OSC.LVs), progress = TRUE, train.test.index = train.test.index) #...
	#compare actual to permuted model performance
	(model.validation<-OSC.validate.model(model = mods, perm = multi.permute, train = multi.train.test,test="perm.test2"))

	#feature selection
	opts<-PLS.feature.select(pls.data,pls.scores=final.opls.results$scores[,][,1,drop=F],pls.loadings=final.opls.results$loadings[,][,1,drop=F],pls.weight=final.opls.results$loadings[,][,1,drop=F],plot=FALSE,p.value=0.1,FDR=TRUE,cut.type="number",top=3,separate=FALSE)
	# make s-plot plus
	plot.S.plot(obj=opts,return="all")

	#calculate included and excluded feature statistics
	(optim<-optimize.OPLS.feature.select(model=opls.results,feature.subset=opts$combined.selection,permute=TRUE,train.test.index,progress=TRUE,test="perm.test2") )# check variance explained in X

	#optimize model feature selection
	res<-multi.OPLS.feature.select(model=opls.results,filter=filter,plot=FALSE,OPLSDA=TRUE,train.test.index=train.test.index) # use full model without training split as input
	plot.multi.OPLS.feature.select(res) # viwe results
	best.OPLS.features(res) # extract best model
}

#O-PLS-DA
#--------------------------
{
	pls.data<-mtcars[,!colnames(mtcars)%in%"am"]
	pls.y<-mtcars$am

	opls.results<-make.OSC.PLS.model(pls.y,pls.data,
							comp=2,
							OSC.comp=1, 
							validation = "none", 
							cv.scale = TRUE,
							train.test.index=NULL,
							progress=FALSE,
							OPLSDA=TRUE)
				
	final.opls.results<-get.OSC.model(obj=opls.results,OSC.comp=1)		
	(opls.model.text<-data.frame("Xvar"=c(0,round(cumsum(final.opls.results$Xvar)*100,2)),"Q2"=final.opls.results$Q2,"RMSEP"= final.opls.results$RMSEP)	)

	# predict class labels
	ntests<-1
	strata<-pls.y # use to control equivalent sampling from groups

	#train/test index
	train.test.index <- test.train.split(nrow(pls.data), n = ntests, strata = strata, split.type = "random", data = pls.data) 
	opls.results2<-make.OSC.PLS.model(pls.y,pls.data,
							comp=3,
							OSC.comp=1, 
							validation = "LOO", 
							cv.scale = TRUE,
							train.test.index=train.test.index,
							progress=FALSE,
							OPLSDA=TRUE)	
	#get classification stats
	final.opls.results<-get.OSC.model(obj=opls.results2,OSC.comp=1)
	final.opls.results$OPLSDA.stats # perfect model

	#carry out model feature selection and validation 
	ntests<-5
	strata<-pls.y # use to control equivalent sampling from groups

	#train/test index
	train.test.index <- test.train.split(nrow(pls.data), n = ntests, strata = strata, split.type = "random", data = pls.data)

	filter<-seq(3,ncol(pls.data)-3)
	#try different thresholds for feature selection 
	#wrapper to fit multiple models and validate
	res<-multi.OPLS.feature.select(model=opls.results,filter=filter,plot=FALSE,OPLSDA=TRUE,train.test.index=train.test.index) # use full model without training split as input
	plot.multi.OPLS.feature.select(res) # view results
	best.OPLS.features(res) # extract best model
	#extract best model
}


}
