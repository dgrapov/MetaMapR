# TODO add control for polygon plot order (https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
# open addition of pallets, themes,add group labels to plot, ... 

#trying to debug issue in shiny, pca.data is now an object instead of its name as a character
devium.pca.calculate<-function(pca.inputs,args.list=TRUE,return="list", plot=TRUE)
	{
		#port of imDEV source code optimized for GUI use
		#accepts list with the following arguments
		#pca.data<- data object (samples as rows)
		#pca.components numeric number of number of principal components
		#pca.algorithm see pcaMethods::pca for options
		#pca.center logical, mean center the data
		#pca.scaling character describing scaling method, see pcaMethods::prep for options
		
		#check for text or factor and remove (add to subset) 
		tmp<-pca.inputs
		# data.obj<-as.data.frame(get(tmp$pca.data))
		# data.obj<-data.obj[sapply(1:ncol(data.obj), function(i) {class(data.obj[,i])=="numeric"|class(data.obj[,i])=="integer"})] # has to be better way to avoid factors
		 data.obj<-afixln(tmp$pca.data) # converts factors or characters to numeric
		
		if(is.null(tmp$pca.cv)){pca.cv<-"none"} else {pca.cv<-tmp$pca.cv} #avoid issues elsewhere
		
		
		#adjust PCS if > than data
		PCs<-tmp$pca.components
		if(PCs> min(dim(data.obj))){PCs<-min(dim(data.obj))} # this should be done internally in the PCa fxn
		pca.results<-pcaMethods::pca(as.matrix(data.obj), method=tmp$pca.algorithm, 
			nPcs=PCs, center=tmp$pca.center,scale=tmp$pca.scaling, cv = pca.cv, seed=123)
		
		#results
		scores<-as.data.frame(pca.results@scores)
		loadings<-as.data.frame(pca.results@loadings)
		eigenvalues<-data.frame(eigenvalues=pca.results@R2)
		
		
		if(pca.cv=="q2"){
				# account for unequal r2 and q2 lengths 
				q2<-tryCatch( pcaMethods:::Q2(pca.results), error=function(e) {0} )#some versions of pcaMEthods don't have this?
				q2<-c(q2,rep(q2[length(q2)],nrow(eigenvalues)-length(q2)))
				eigenvalues<-data.frame(eigenvalues,q2=q2)
			}

		#add leverage and dmodX
		#bind between scores and loadings
		lev<-tryCatch(as.matrix( pcaMethods:::leverage(pca.results)),error=function(e){"can not calculate"})
		dmodx<-tryCatch(as.matrix( pcaMethods:::DModX(pca.results)),error=function(e){"can not calculate"})
		diagnostics<-tryCatch(data.frame(leverage=lev,DmodX=dmodx),error=function(e){data.frame(Error="not applicable")})

		#scree plot
		if(plot==TRUE){ make.scree.plot.bar(eigenvalues) }
	
		
		#get the name of the data
		if(return=="list"){
				return(list(pca.scores = scores, pca.loadings =  loadings,pca.eigenvalues = eigenvalues, pca.diagnostics = diagnostics))} 
		
		if(return=="model"){return(pca.results)}
	}
	
# generate a scree plot base
make.scree.plot<-function(eigenvalues)
	{
		pcaeigen<-eigenvalues
		# x11()
		par(mar=c(4,4,4,4.25))
		total<-sum(matrix(unlist(pcaeigen))*100)
		plot(c(1:nrow(pcaeigen)),matrix(unlist(pcaeigen))*100,type="l",main=paste("PCA Screeplot showing",round(total,0), "% explained variance"),
		lwd=2,xaxt="n",frame.plot=TRUE,xlab=paste("Principal components (n =",nrow(pcaeigen) ,")"),ylab="")#cbind(matrix(1:nrow(as.matrix(pcaeigen)))
		abline(v=seq(1,nrow(pcaeigen),by=1),lty=2,col="gray40")
		points(as.matrix(pcaeigen)*100,col="black",pch=21,bg="red",cex=2)
		mtext("% Explained Variance",side=2,line=3,col="red")
		abline(h=1,lty=2)
		total.var<-round(cumsum(matrix(unlist(pcaeigen)))*100,0)
		par(new=TRUE)
		plot(c(1:nrow(pcaeigen)),total.var,type="l",
		lwd=2,xaxt="n",yaxt="n",lty=1,ylab="",xlab="")
		abline(v=c(1:nrow(pcaeigen))[total.var>79][1],lty=2)
		points(total.var,col="black",pch=21,bg="blue",cex=2)
		axis(4)
		mtext("Total % explained variance",side=4,line=3,col="blue")
		axis(1,seq(1,nrow(pcaeigen),by=1))
		#---------------------------------------------------------------
	}

make.scree.plot.bar<-function(eigenvalues){
	
	library(ggplot2)
	library(reshape2)
	
	check.get.packages("gridExtra")
	.theme<- theme(
					axis.line = element_line(colour = 'gray', size = .75), 
					panel.background = element_blank(),  
					plot.background = element_blank()
				 )	
	
	tmp<-data.frame(melt(eigenvalues$eigenvalue),PCs=rep(1:nrow(eigenvalues)))
	tmp$value<-tmp$value*100
	p1<-ggplot(tmp, aes(y=value, x = as.factor(PCs)))+geom_bar( fill="gray",stat="identity",position=position_dodge())+
	 .theme + geom_hline(yintercept=1,linetype=2) + ylab("% variance explained") + xlab("Principal Component")
	
	#cumulative	
	eigenvalues$eigenvalues<-cumsum(eigenvalues$eigenvalues)
	tmp<-data.frame(melt(eigenvalues),PCs=rep(1:nrow(eigenvalues)))
	p2<-ggplot(tmp, aes(y=value, x = as.factor(PCs), fill=variable))+geom_bar( stat="identity",position=position_dodge())+
	.theme + geom_hline(yintercept=.8,linetype=2) +xlab("Principal Component")
	
	#multiple plot out put
	grid.arrange(p1, p2, ncol=1) 
	
}


plot.PCA<-function(pca,xaxis=1,yaxis=2, results = c("screeplot","scores","loadings","biplot"),group.bounds="ellipse",size=3,color=NULL, label=TRUE, legend.name =  NULL, font.size=5,alpha=.75,g.alpha=0.2,theme=NULL, point.labels=NULL,extra=NULL,...){
	
	library(ggplot2)
	library(reshape2)
	library(grid) # arrows
	#results<-match.args(results) #
	local<-switch(results[1],

		"screeplot" 	= function(pca,...){make.scree.plot.bar(pca$pca.eigenvalues)},
		"scores"		= function(pca,color,size,extra,...){
								obj<-pca$pca.scores[,c(xaxis,yaxis)]	
								tmp<-data.frame(obj,id = rownames(obj))
								#plot 
								if(is.null(theme)){
								.theme2<- theme(
											axis.line = element_line(colour = 'gray', size = .75), 
											panel.background = element_blank(), 
											plot.background = element_blank(),
											legend.background=element_rect(fill='white'),
											legend.key = element_blank()
										 )
								} else {.theme2<-theme}	
								
								if(is.null(color)){
										tmp$color<-"gray"
									}else{
										
										tmp$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}
								
								points<-if(all(tmp$color=="gray")) { 
									geom_point(color="gray",size=size,alpha=alpha,show_guide = FALSE) 
								} else { 
									if(!is.data.frame(size)){
										geom_point(aes(color=color),size=size,alpha=alpha)  
									} else {
										tmp$size<-size[,1]
										geom_point(aes(color=color,size=size),alpha=alpha) 
									}	
								}
								#labels
								tmp$lab.offset<-tmp[,2]-abs(range(obj[,2])[1]-range(obj[,2])[2])/50						
								labels<-if(label==TRUE){
									if(is.null(point.labels)){
										geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)
									} else {
										tmp$custom.label<-point.labels
										geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="custom.label"),color="black",show_guide = FALSE)
									}	
								} else { NULL }
								
								#grouping visualizations
								polygons<-NULL
								#Hoettellings T2 ellipse
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(obj[,1],obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
									} else {
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
									}
								}
								#convex hull
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(cbind(obj[,1],obj[,2])),tmp$color)# group visualization via 
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
								polygons +
								points +
								labels +
								.theme2 + 
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(pca$pca.eigenvalues[xaxis,1],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(pca$pca.eigenvalues[yaxis,1],digits=2)*100),sep=" ")) 
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								p<-p+extra
								print(p)
							},
							
		"loadings"		= function(pca,color,size,...){
								obj<-pca$pca.loadings[,c(xaxis,yaxis)]	
								tmp<-data.frame(obj,id = rownames(obj))
								#plot 
								if(is.null(theme)){
								.theme2<- theme(
											axis.line = element_line(colour = 'gray', size = .75), 
											panel.background = element_blank(), 
											plot.background = element_blank(),
											legend.background=element_rect(fill='white'),
											legend.key = element_blank()
										 )
								} else {.theme2<-theme}	
										 
								if(is.null(color)){
										tmp$color<-"gray"
									}else{
										tmp$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}
								
								points<-if(all(tmp$color=="gray")) { 
									geom_point(color="gray",size=size,alpha=alpha,show_guide = FALSE) 
								} else { 
									geom_point(aes(color=color),size=size,alpha=alpha)  
								}
								#labels
								tmp$lab.offset<-tmp[,2]-abs(range(obj[,2])[1]-range(obj[,2])[2])/50						
								labels<-if(label==TRUE){geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)} else { NULL }
								
								#grouping visualizations
								polygons<-NULL
								#Hoettellings T2 ellipse
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(obj[,1],obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
									} else {
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
									}
								}
								#convex hull
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(cbind(obj[,1],obj[,2])),tmp$color)# group visualization via 
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
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(pca$pca.eigenvalues[xaxis,1],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(pca$pca.eigenvalues[yaxis,1],digits=2)*100),sep=" ")) 
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								print(p)
							},
							
		"biplot"		= function(pca,...){
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
								if(is.null(theme)){
								.theme2<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												plot.background = element_blank()
											 )
								} else {.theme2<-theme}	
								
								#based on https://groups.google.com/forum/#!topic/ggplot2/X-o2VXjDkQ8
								scores<-pca$pca.scores[,c(xaxis,yaxis)]
								loadings<-tmp.loadings<-pca$pca.loadings[,c(xaxis,yaxis)]
								tmp.loadings[,1]<-rescale(loadings[,1], range(scores[,1]))
								tmp.loadings[,2]<-rescale(loadings[,2], range(scores[,2]))
								tmp.loadings<-data.frame(tmp.loadings,label=rownames(loadings))
								
								#Adding grouping visualizations
								tmp<-scores[,1:2]
								if(is.null(color)){
										tmp$color<-"gray"
									}else{
										tmp$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}	
								#grouping visualizations
								polygons<-NULL
								#Hoettellings T2 ellipse
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp[,1],tmp[,2]), group=tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
									} else {
										geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
									}
								}
								#convex hull
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(cbind(tmp[,1],tmp[,2]),tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
										geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=g.alpha, show_guide = FALSE) 
									} else {
										geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=g.alpha, show_guide = FALSE) 
									}
								}
								# ell<-get.ellipse.coords(cbind(tmp[,1],tmp[,2]), group=tmp$color)# group visualization via 
								# polygons<-if(is.null(color)){
										# geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.1, show_guide = FALSE) 
									# } else {
										# geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=.1, show_guide = FALSE) 
									# }
									
								points<-if(all(tmp$color=="gray")) { 
									geom_point(data=tmp,aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2]),color="gray",size=size,alpha=alpha,show_guide = FALSE) 
								} else { 
									if(!is.data.frame(size)){
										geom_point(data=tmp, aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2],color="color"),size=size,alpha=alpha)   
									} else {
										tmp$size<-size[,1]
										geom_point(data=tmp, aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2],color="color",size="size"),alpha=alpha)
									}	  
								}
								
								p<-ggplot()+
								points +
								polygons+
								geom_segment(data=tmp.loadings, aes_string(x=0, y=0, xend=colnames(tmp.loadings)[1], yend=colnames(tmp.loadings)[2]), arrow=arrow(length=unit(0.05,"cm")), alpha=0.25)+
								geom_text(data=tmp.loadings, aes_string(x=colnames(tmp.loadings)[1], y=colnames(tmp.loadings)[2], label="label"), alpha=0.5, size=font.size)+
								scale_colour_discrete("Variety")+
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(pca$pca.eigenvalues[xaxis,1],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(pca$pca.eigenvalues[yaxis,1],digits=2)*100),sep=" ")) +
								.theme2
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								p<-p+extra
								print(p)
							}
	)
	
	local(pca=pca,color=color,size=size,alpha=alpha,group.bounds=group.bounds,extra,...)

}

	
test<-function(){
data(mtcars)
data<-mtcars

tmp<-list()
tmp$pca.algorithm<-"svd"
tmp$pca.components<-2
tmp$pca.center<-TRUE
tmp$pca.scaling<-"uv"
tmp$pca.data<-data
pca.inputs<-tmp

res<-devium.pca.calculate(pca.inputs,return="list",plot=FALSE)

results<-"biplot"#"biplot"#"scores","loadings","biplot")"screeplot"
color<-data.frame(am=mtcars$am,vs=mtcars$vs)#NULL#data.frame(am=mtcars$am)
color<-data.frame(color=join.columns(color))
color=NULL
xaxis<-1
yaxis=2
group.bounds="polygon"

plot.PCA(pca=res,results=results,yaxis=yaxis,xaxis=xaxis,size=1,color=color, label=TRUE, legend.name =  NULL,font.size=.1,group.bounds,alpha=.75)

}
