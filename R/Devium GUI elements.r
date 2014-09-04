#message box
done.info.GUI<-function(message)
{
			done<-gwindow("Information",width = 200, height= 100)
			g<-glayout(cont = done)
			g[2,1]<-glabel(message,container = g)
			g[3,3]<-gbutton("ok",container = done, handler = function(h,...){dispose(done)})
		}	

#ask question do OK fxn		
ask.gui<-function(message, OK=function(h,...){print("OK!")})
{
		done<-gwindow("Question",width = 200, height= 100)
		g<-glayout(cont = done)
		g[2,1]<-glabel(message,container = g)
		g[3,3]<-gbutton("cancel",container = done, handler = function(h,...){dispose(done)})
		g[3,4]<-gbutton("ok",container = done, 
			handler = function(h,...)
				{
					tryCatch(OK(h,...),
					error=function(e){print("couldn't proceed")});dispose(done)
				})
	}

#GUI for pcaMethods
devium.PCA.gui<-function(container=NULL)
{
	check.get.packages(c("aroma.light","pcaMethods"))
	options(device="gWidgetsRGtk2")
	
	#for debugging
	#container= gwindow("test")

	#use buttons to assign variables to a main object: get("devium.pca.object",envir=devium)
	#upon execute 
	# 1) gather gui inputs
	# 2) translate
	# 3) send translated inputs to main fxn
	
	#gui objects
	gui.objects<-c("pca.data","pca.scaling","pca.center","pca.algorithm","pca.components")
	
	 #fxn to create the environment "devium" if it does not exist
	 create.devium.env()	
	 
	 if(!exists("devium.pca.object",envir=devium))
		{
			tmp<-list()
			tmp$"max.pca.components"<-3 # some defaults
			assign("devium.pca.object",tmp,envir=devium)
			
		}
	 
	mainWin = ggroup(horizontal = FALSE, container = container)
	
	#make tool bar on top
	make.tool.bar<-function(container=NULL)
	{
		
			# for debugging container=gwindow()
			mainWin = ggroup(horizontal = FALSE, container = container)
	
			#make tool bar on top
			buttonBar = ggroup(spacing = 0,container=mainWin)
			add(mainWin, buttonBar)
			
			#setting options second tool bar
			toolbar = list()

			toolbar$save$icon = "save"
			#toolbar$tmp1$separator = TRUE
			toolbar$save$handler = function(h, ...){done.info.GUI("add save options: to excel, etc")}
				
			#toolbar$tmp2$separator = TRUE
			toolbar$execute$icon = "execute"
			toolbar$execute$handler = function(h, ...) {
			d.get(gui.objects) # get all objects in form
			devium.pca.calculate(pca.inputs=get("devium.pca.object",envir=devium)) # calculate PCA
			}

			toolbar$help$icon = "help"
			toolbar$help$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			
			toolbar$plot$icon = "plot"
			toolbar$plot$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			tmp = gtoolbar(toolbar)
			add(buttonBar, tmp, expand = TRUE)
		}
		
	#make top tool bar
	make.tool.bar(container=mainWin)
	
	#notebook to hold options
	.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1)
	
	#variables for plots
	#plot type and subset
	scaling<-tmp<-glayout(container=.notebook,label="Data")
	tmp[1,1:2]<-assign("pca.data",gedit("",container=tmp),envir=devium)
	adddroptarget(get("pca.data",envir=devium),handler = function(h,...) 
	{
			svalue(h$obj)<-(h$dropdata)
			d.assign("pca.data",main.object="devium.pca.object",get((h$dropdata)))
			#set upper limit for components
			obj<-tryCatch(get("devium.pca.object",envir=devium),error=function(e){NULL})
			upper.limit<-min(dim(tryCatch(obj$pca.data,error=function(e){data.frame(NULL)})))
			if(class(upper.limit)=="NULL")
				{
					return()
				} else {
					d.assign("max.pca.components",main.object="devium.pca.object",upper.limit)#set upper limit for components
				}
	})
	
	
	tmp[2,1]<-glabel("scaling")
	tmp[2,2]<-assign("pca.scaling",gcombobox(c("none","pareto", "vector", "uv"),selected = 4),envir=devium)#,envir=devium
	tmp[3,1]<-glabel("center",container=tmp)
	tmp[3,2]<-assign("pca.center",gcheckbox("",checked = TRUE,container=tmp),envir=devium)#, envir=devium
	
	#methods
	pca.methods<-tmp<-glayout(container=.notebook,label="Methods")
	tmp[1,1]<-glabel("algorithm")
	tmp[1,2]<-assign("pca.algorithm",gcombobox(c("svd","rnipals", "bpca", "ppca", "svdImpute","nlpca"),container=tmp),envir=devium)#,envir=devium # avoid nipals,robustPCA due to errors
	tmp[2,1]<-glabel("components",container=tmp)
	#pca components
	get.max.pc<-function(){tmp<-get("devium.pca.object",envir=devium);tmp$max.pca.components}
	tmp[2,2]<-assign("pca.components",gspinbutton(from = 1, to =get.max.pc(),value= get.max.pc(),container=tmp, handler=function(h,...)
		{
			#set upper limit for components
			tmp[2,2]<-assign("pca.components",gspinbutton(from = 2, to = get.max.pc(),container=pca.methods), envir=devium)
		}),envir= devium)
		
	#not sure need to assign this
	svalue(.notebook)<-1 # show first tab on start up
	assign("devium.pca.notebook", mainWin,envir = devium)
	return(mainWin)
	}
	
#2D scatter plot for 2 vectors or basic pairs plot for a data.frame 
devium.scatter.plot<- function(container=NULL)
{
	#access plot settings using: get("devium.scatter.plot.pars",envir=devium)	
	#container= gwindow("test")
	
	#making the GUI------------------------
	#create notebook container for GUI
	mainWin = ggroup(horizontal = FALSE, container = container)
	
	#gvarbrowser(container=container)
	
	#make tool bar on top
	make.tool.bar<-function(container=NULL)
	{
		
			mainWin = ggroup(horizontal = FALSE, container = container)
	
			#make tool bar on top
			buttonBar = ggroup(spacing = 0,container=mainWin)
			add(mainWin, buttonBar)
			
			#setting options second tool bar
			toolbar = list()

			toolbar$save$icon = "save"
			#toolbar$tmp1$separator = TRUE
			toolbar$save$handler = function(h, ...){
				tmp<-tryCatch(get("devium.plotnotebook.window",envir=devium),error= function(e){NULL})
				if (is.null(tmp) || !is.gWindow(tmp) ||  is.invalid(tmp)) {
					tmp<-gwindow("D E V I U M plot notebook", visible = TRUE)  
					add(tmp, ggraphicsnotebook())
					 assign("devium.plotnotebook.window", tmp, envir = devium)
				}
				else {
					tmp<-get("devium.plotnotebook.window",envir=devium)
					focus(tmp) 
				}
			}

			#toolbar$tmp2$separator = TRUE
			toolbar$plotnotebook$icon = "plot"
			toolbar$plotnotebook$handler = function(h, ...) {
			refresh.plot()
			}

			toolbar$help$icon = "help"
			toolbar$help$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			tmp = gtoolbar(toolbar)
			add(buttonBar, tmp, expand = TRUE)
		}

	make.tool.bar(container=mainWin)
	
	#notebook to hold options
	.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1)

	
	#options for points
	#-------------------------------------------
	plot.opts<-c("Xaxis","Yaxis","color","size","shape","border","width") # form refrence
	plot.opts.labs<-c("  Xaxis","  Yaxis","  color","  size","  shape","  border","  width") # labels
	plot.defaults<-c(x=NA,y=NA,bg=alpha.col("gray",if.or("color.alpha",default=.75)),cex=1,pch=21,col=alpha.col("black",if.or("color.alpha",default=.75)),lwd=1)	#fxn defaults
	plot.names<-c("x","y","bg","cex","pch","col","lwd") # fxn input possibilities
	
	
	# point properties
	point.var<-glayout(container=.notebook,label="Points")
	
	i<-1
	for(i in 1:length(plot.opts))
		{	
			point.var[(i+1),2]<-assign(plot.opts[i],gedit("", container=point.var, width=15)) #,envir=devium
			point.var[(i+1),1]<-glabel(plot.opts.labs[i],width=5)
		}
	
	#add color alpha
	point.var[9,2]<-assign("color.alpha",gslider(from = 0.1, to = 1,by=0.01,value=.75,
		handler=function(h,...)
			{
				 tmp<-get("devium.scatter.plot.pars",envir=devium)
				 tmp$bg<-alpha.col(tmp$bg,if.or("color.alpha",default=.75))
				 tmp$col<-alpha.col(tmp$col,if.or("color.alpha",default=.75)) 
				 assign("devium.scatter.plot.pars",tmp,envir=devium)
				refresh.plot()
			}),envir=devium)
	point.var[9,1]<-glabel("  transparency")
	
	#for mapping point plotting properties (names up top)
	point.var[1,3]<- glabel("  n  |  levels    options")


	
	# make discreet or use n levels for mapping
	#bg color
	point.var[4,3]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.bg.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.bg",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp,handler=function(h,...){refresh.plot()}),envir=devium)
	#pallet
	point.var[4,4]<- assign("color.pallet.bg",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE,container=tmp,handler=function(h,...){refresh.plot()}),envir=devium)
	
	#bordercolor
	point.var[7,3]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.col.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.col",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp),envir=devium)
	point.var[7,4]<- assign("color.pallet.col",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE,container=tmp,handler=function(h,...){refresh.plot()}),envir=devium)
	
	i<-5
	for(i in 5:(length(plot.opts)+1))
		{
			point.var[i,3]<-tmp<-ggroup(horizontal=TRUE)
			assign(paste(plot.opts[(i-1)],"levels.n",sep="."),gcheckbox("n",container=tmp))
			assign(paste(plot.opts[(i-1)],"levels",sep="."),gspinbutton(from=0, to = 20, selected = 1, editable = FALSE,container=tmp))
			if(!any(i%in%c(6,7)))
				{
					glabel("min",container=tmp)
					assign(paste(plot.opts[(i-1)],"min",sep="."),gspinbutton(from=.1, to=10, by = .2, selected = .1, editable = FALSE,value = 1, container=tmp,
						handler=function(h,...){refresh.plot()}),envir=devium)
					glabel("max",container=tmp)
					assign(paste(plot.opts[(i-1)],"max",sep="."),gspinbutton(from=.1, to=10,by = .2, selected = .1, editable = FALSE,value=5,container=tmp,
						handler=function(h,...)
							{refresh.plot()}),envir=devium)
				}
		}
		
	#offset	
	
	#options for groups
	#-------------------------------------------
	group<-glayout(container = .notebook,label="Groups")
	group[1,1]<-glabel("  variable")
	group[1,2]<-assign("group.type.var",gedit("",container=group),envir=devium)# choosing type of visualization
		#name = form obj name
	adddroptarget(get("group.type.var",envir=devium),handler = function(h,...) 
	{
			svalue(h$obj)<-(h$dropdata)
			refresh.plot()
	})
		
	group[1,3]<-glabel("visualization")
	group[2,3]<-assign("group.type",gcombobox(c("none","ellipse","polygon","cluster","ellipse.cluster"), selected = 1, editable = FALSE,container=group, handler=function(h,...){refresh.plot()} ),envir=devium)
	
	#setting color and grouping options
	group[2,2]<-tmp<-ggroup(horizontal=TRUE)
	group[2,1]<-glabel("  color pallet")
	group[2,2]<-assign("color.pallet.group",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE,handler=function(h,...){refresh.plot()}),envir=devium)
	group[3,1]<-glabel("  levels")
	group[3,2]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.group.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.group",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp),envir=devium)
	
	# transparency
	group[4,1]<-glabel("  transparency")
	group[4,2]<-assign("group.transparency",gslider(from = 0, to = 1, by = .05,value=0.5,handler=function(h,...){refresh.plot()}),envir=devium)
	
	#line width and type
	group[4,3]<-tmp<-ggroup(horizontal=FALSE)
	glabel("line width",container=tmp)
	assign("group.width",gspinbutton(from = 0, to = 10, by = 1,value=1,container=tmp,handler=function(h,...){refresh.plot()}),envir=devium)
	
	# C.I. for ellipse
    group[5,1]<-glabel("  ellipse level")
	group[5,2]<-assign("ellipse.level",gslider(from = 0, to = .99, by = .01,value=0.95,
		handler=function(h,...){refresh.plot(plot=FALSE)}),envir=devium)#,handler=function(h,...){refresh.plot(plot=FALSE)}),envir=devium) # disable for manual refresh only
	#line type
	group[5,3]<-tmp<-ggroup(horizontal=FALSE)
	glabel("line type",container=tmp)
	assign("group.line.type",gspinbutton(from = 0, to = 6, by = 1,value=1,
		handler=function(h,...){refresh.plot()},container=tmp),envir=devium)#,handler=function(h,...){refresh.plot()}),envir=devium)
	
	
	#global plotting options
	global<-ggroup(container = .notebook,label="Global")
	
	#legend plotting options
	legend<-ggroup(container = .notebook,label="Legend")
	svalue(.notebook)<-1
	#GUI Done next need functions for handlers
	#-------------------------------------------
	
	
	#add handlers for drag and drop objects
	#should minimaly change name dislayed and call a fxn
	#which gathers inputs from all fomr fields
	#based on 
	# plot.opts 	= names of drag and drop objects
	# plot.defaults = generics for NA or NULL
	# plot.names 	= names of objects in another fxn
	
	#function to update form with droped objects name
	drop.names<-function(name)
		{
			#name = form obj name
			adddroptarget(name,handler = function(h,...) 
				{
					svalue(name)<-h$dropdata
				})

		}
	
	#function to gather all form inputs
	#based on plot opts 	
	gather.names<-function(names)
		{
			tmp<-lapply(1:length(names),function(i)
				{
					svalue(get(names[i]))
				})
			names(tmp)<-names	
			return(tmp)			
		}
	
	# match names between objects (form and fxn)
	# and get object values
	get.named<-function(obj)
		{
			
			id<-names(obj)
			tmp<-lapply(1:length(id),function(i)
				{
					tryCatch(gget(svalue(get(id[i]))),error=function(e){NULL})
				})
			#match names based on loolup table
			names(tmp)<-id	
			return(tmp)			
		}
	
	#translate between form and fxn based on look up table
	translate.names <-function(names,lookup=cbind(plot.opts,plot.names))
		{
		#lookup 	= 	is a column matrix with the first column the index
		#				for the object
		# 				and subsequent columns the translations
				id<-lookup[,1]
				lookup[id%in%names,-1,drop=FALSE]				
		}

	
	#convert options to fxn inputs and/or set defaults if missing/NULL
	convert.or.set<-function(named.obj,default=plot.defaults)
		{
			id<-names(named.obj)
			obj<-lapply(1:length(id),function(i)
				{	
					
					out<-switch(id[i],
								x 		= named.obj[i],
								y 		= named.obj[i],
								bg 		= convert.to.color(named.obj[i],legend="bg", pallet=if.or("color.pallet.bg",default="rainbow"),alpha=if.or("color.alpha",default=.75)),
								cex 	= convert.to.size(named.obj[i],legend="cex"),
								pch 	= convert.to.shape(named.obj[i],legend="pch"),
								col 	= convert.to.color(named.obj[i],legend = "col",pallet=if.or("color.pallet.col",default="rainbow"),alpha=if.or("color.alpha",default=.75)),
								lwd		= convert.to.size(named.obj[i],legend="lwd"),
								xlab 	= named.obj[i],
								ylab 	= named.obj[i])
								
					#can't use NULL above due to exclusion in string		
					# need to set the type of the variable character or numeric
					if(class(out)=="NULL"|length(out)==0) assign("out",default[i])
					if(any(is.na(out))) out<-default[i]
					unlist(out)
				})	
				names(obj)<-id
				return(obj)
		}
	
	#generate fxn input from form output
	get.inputs<-function(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults)
		{
			tmp<-gather.names(form.names)
			assign("scatter.plot.legend.ids",tmp,env=devium)
			tmp<-get.named(tmp)
			tmp.names<-translate.names(names(tmp),lookup=lookup)
			names(tmp)<-tmp.names
			convert.or.set(tmp,default)
		}
		
	#fxn to make plot 
	new.plot<-function(fxn,tmp,layer=if.or("group.type",default="none"))
		{
		
		#if(names(dev.cur())=="null device")
		if(names(dev.cur())[1]=="null device"){x11()} 
			#{
				opar<-par()
				layout(matrix(c(1,1,1,1,1,1,2,2),nrow=2,ncol=4))
				par(mar=c(4, 4, 1,.5))
				#plot.new()
				#x11()
			#}
		
		
		switch(fxn,
			plot = 	.plot<-function(fxn) 
				{
					#initialize plot
					do.call(fxn,
							list(
									x=tmp$x,
									y=tmp$y,
									type="n",
									frame.plot=FALSE,
									xlab=as.character(tmp$xlab), 
									ylab=as.character(tmp$ylab)
								))
					
					#add layers
					#collect layer options
					layer.par<-list()
					layer.par$lwd<-if.or("group.width",default=1)
					layer.par$lty<-if.or("group.line.type",default=1)
					layer.par$color<-if.or("group.color",default=tmp$bg)
					layer.par$color.pallet<-if.or("color.pallet.group",default="rainbow")
					layer.par$ellipse.level<-if.or("ellipse.level",default=.95)
					layer.par$transparency<-if.or("group.transparency",default=.5)
					layer.par$object<-if.or("group.type.var",default=1)
					
					#map layer object to color
					if(layer.par$object==1|layer.par$object=="")
						{
							grp.color<-rep(alpha.col("gray",layer.par$transparency),length(tmp$x))
						} else {
							grp.color<-convert.to.color(object=gget(layer.par$object),
								pallet=layer.par$color.pallet,alpha=layer.par$transparency,legend="group")	
						}
					
					switch(layer,
						none 			= .layer<-function(){return()},
						cluster 		= .layer<-function(){edge.group(obj=cbind(tmp$x,tmp$y),color=grp.color,lwd=layer.par$lwd,lty=layer.par$lty)},
						ellipse 		= .layer<-function(){ellipse.group(obj=cbind(tmp$x,tmp$y),color=grp.color,lwd=layer.par$lwd,lty=layer.par$lty,
											border="black",ellipse.level=layer.par$ellipse.level,show.polygon=TRUE,alpha=layer.par$transparency)},
						polygon 		= .layer<-function(){polygon.group(obj=cbind(tmp$x,tmp$y),color=grp.color,lwd=layer.par$lwd,lty=layer.par$lty,
											border="black",show.polygon=TRUE,alpha=layer.par$transparency)},
						ellipse.cluster = .layer<-function()
												{
													ellipse.group(obj=cbind(tmp$x,tmp$y),color=grp.color,lwd=layer.par$lwd,lty=layer.par$lty,
														border="black",ellipse.level=layer.par$ellipse.level,show.polygon=TRUE,alpha=layer.par$transparency)
														edge.group(obj=cbind(tmp$x,tmp$y),color=grp.color,lwd=layer.par$lwd,lty=layer.par$lty)
												} 
							)
				
					#avoid for one demensional objects					
					if(!is.null(tmp$x)&!is.null(tmp$y)){.layer()}					
					
					#add points
					do.call("points",
							list(
									x=tmp$x,
									y=tmp$y,
									col=tmp$col,
									bg=tmp$bg,
									cex=as.numeric(tmp$cex),
									pch=as.numeric(tmp$pch),
									lwd=as.numeric(tmp$lwd)
								))
								
					#make legend
					legend<-get("scatter.plot.legend",env=devium)
					if(length(legend)==0) 
						{
							return() 
						} else {
								par(mar=c(.5, 2, 4,.5))
								plot.new()
								obj<-do.call("rbind",format.for.legend(legend.list=legend,limit=5))
								make.plot.legend(obj)
						}				
				},
							
			pairs = .plot<-function(fxn) 
				{
				do.call(fxn,
							list(
									x=tmp$x,
									col=tmp$col,
									bg=tmp$bg,
									frame.plot=FALSE,
									cex=as.numeric(tmp$cex),
									pch=as.numeric(tmp$pch),
									lwd=as.numeric(tmp$lwd)
								)
							)
				})
				
			.plot(fxn)	
			tryCatch(bringToTop(which=dev.cur()),error=function(e){print("Check for error hidden window (Mac users)")})
		}
	
	#make plot legend
	format.for.legend<-function(legend.list=get("scatter.plot.legend",env=devium),limit=4)
			{
				check.get.packages("gtools")
				#take a named list and place items in columns for merged legend or separate for separate and
				#format
				#obj structure by colummns 
				#	[1] name 
				#	[2] color 
				#	[3] shape
				#	[4] outline color 
				#	[5] outline width 
				#	[6] size
				
				#use to set defualt if error 
				def.val<-function()
					{
						tmp<-list()
						tmp$col = "white"
						tmp$bg = "gray"
						tmp$cex = 3
						tmp$pch = 21
						tmp$lwd = 2
						return(tmp)
					}
					
				default<-function(name="",nrow=1)
					{
						tmp<-as.data.frame(matrix(
						c(name=name,bg=rep("#FFFFFF00",length(name)),pch=rep(21,length(name)),col=rep("#FFFFFF00",length(name)),lwd=rep(2,length(name)),cex=rep(3,length(name)))
						,nrow=nrow, ncol=6,byrow=FALSE))
						colnames(tmp)<-c("name","bg","pch","col","lwd","cex")
						return(tmp)
					}
				
				#initialize				
				#obj1<-as.matrix(legend.list[[1]])[as.matrix(order(legend.list[[1]][,"name"])),]	
				#attempt to merge legend on common mapping "name"
				#common<-sapply(2:(length(legend.list)),function(i)
				#	{
				#		obj2<-as.matrix(legend.list[[i]])[order(as.matrix(legend.list[[i]][,"name"])),]
				#		match<-as.matrix(obj1[,"name"])==intersect(as.matrix(obj1[,"name"]),as.matrix(obj2[,"name"]))
				#		tmp<-cbind(obj1[match,1],obj1[match,-1],obj2[match,-1])
				#	})
					
				#convert to default format
				list.names<-names(legend.list)
				
				out<-lapply(1:length(legend.list),function(i)
					{
						
						fill<-default(name=list.names[i] )
						obj<-legend.list[[i]][as.matrix(order(legend.list[[i]][,"name"])),]
						out<-merge(fill,obj,all.x=TRUE,all.y=TRUE)
						#out<-out[,order(colnames(out))] # common order
						
						#fill NA with default values
						def<-def.val()
						tmp<-sapply(1:ncol(out),function(j)
							{
								tmp<-as.matrix(unlist(out[,j]) )
								tmp[is.na(tmp)]<-as.matrix(def[colnames(out)[j]])
								out[,j]<-tmp
							})
						colnames(tmp)<-colnames(out)
						
						#limit output 
						obj<-tmp[-1,]
						#test the number of unique levels
						test<-as.factor(unlist(obj[,1]))
						if(limit<nlevels(test))
							{
								limit.id<-quantile(1:nrow(obj),seq(0,1,length.out=limit))+1
							} else {
								limit.id<-unique.id(test) + 1
							}
							
						tmp[c(1,limit.id),c("name","bg","pch","col","lwd","cex")]				
					})
					
					#ids for mapped objects
					id<-get("scatter.plot.legend.ids", env= devium)
					n<-names(out)<-names(legend.list)
					
					mapped<-function(name)
								{
								switch(name,
									bg = paste("color",id$color),
									pch = paste("shape",id$shape), 
									col = paste("border",id$border),
									cex = paste("size",id$size),
									lwd = paste("width",id$border))
								}
					#only return non-generic legend items
					# identify return index
					to.send<-function(name)
									{
									switch(name,
										bg = if(!id$color=="") name,
										pch = if(!id$shape=="")name, 
										col = if(!id$border=="")name,
										cex = if(!id$size=="") name,
										lwd = if(!id$width=="") name)
									}
					
					lapply(1:length(n),function(i)
						{
							if(is.null(to.send(n[i])))
								{
									return()
								}else{
									tmp<-out[[n[i]]]
									tmp[1,1]<-mapped(n[i])
									tmp
								}
						})	
								
					}
					
	make.plot.legend<-function(obj,legend.placement="topleft",new=FALSE,legend.ncol=1,legend.cex=1.5)
		{
			#obj structure by colummns 
			#	[1] class [2] color [3] point character (required)
			#	[4] outline color [5] outline width (optional)
			#	[6] line or point logic [7] line type for lines (not used)
			
			if(is.null(obj))
				{
					return()
				}else{
				
				plot.legend<-function(place)
					{
						
						if(place=="custom"){ place<-locator(1)}
							legend(place,ncol=legend.ncol, legend=c(as.character(obj[,1])),
							col=as.character(obj[,4]),pt.lwd=as.numeric(obj[,5]),pt.bg=as.character(obj[,2]),pch=as.numeric(obj[,3]),cex=legend.cex,pt.cex=as.numeric(obj[,6]),bty="n")
					}

			if(new==TRUE)
			{
				x11()
				par(mar=c(.1,.1,.4,.1))
				plot(1,1,type="n",xaxt="n",yaxt="n",frame.plot=FALSE,xlab="",ylab="")
			}
			
			plot.legend(legend.placement)
			}	
		}

	#generic call to plot
	refresh.plot<-function(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults, plot=TRUE)
		{
			if(names(dev.cur())[1]=="null device"){x11()} 			
			#gather inputs
			tmp<-get.inputs(form.name=form.names,lookup,default)
		
			#set x and y axis labels 
			tmp$xlab=svalue(Xaxis)#svalue(Xaxis)
			tmp$ylab=svalue(Yaxis)#svalue(Yaxis)
			
			#check to see if x/y is a data.frame (will be NULL in tmp)
			cx<-tryCatch(get(svalue(Xaxis)),error=function(e){NULL})
			cy<-tryCatch(get(svalue(Yaxis)),error=function(e){NULL})
			
			if(class(cx)=="data.frame")
				{
					tmp$x<-cx
				}
				
			if(class(cy)=="data.frame")
				{
					tmp$x<-cy
				}
				
			if(plot==TRUE)
			{
				#select pairs for data.frames
				if(is.data.frame(tmp$x))
					{
							new.plot("pairs",tmp)	
						} else {
							new.plot("plot",tmp)	
					}
			}
		}
	
	#fxn to set set form handlers
	make.plot.handler<-function(form.names,lookup,default)
		{
			handler.name<-paste("handler",form.names,sep=".")
			i<-1
			for(i in 1:length(form.names))
				{
					#form obj
					obj<-get(form.names[i]) #envir=devium
					#gather inputs and plot
					assign(handler.name[i],function(h,...) 
					{
							#upadate form to show drop source
							
							svalue(h$obj)<-h$dropdata
												
							#gather inputs
							tmp<-get.inputs(form.name=form.names,lookup,default)
							assign("devium.scatter.plot.pars",tmp,envir=devium)	# for debugging
							
							#set x and y axis labels 
							tmp$xlab=svalue(Xaxis)#svalue(Xaxis)
							tmp$ylab=svalue(Yaxis)#svalue(Yaxis)
							
							#check to see if x/y is a data.frame (will be NULL in tmp)
							cx<-tryCatch(get(svalue(Xaxis)),error=function(e){NULL})
							cy<-tryCatch(get(svalue(Yaxis)),error=function(e){NULL})
							
							if(class(cx)=="data.frame")
								{
									tmp$x<-cx
								}
								
							if(class(cy)=="data.frame")
								{
									tmp$x<-cy
								}
										
							#select pairs for data.frames
							if(is.data.frame(tmp$x))
								{
										new.plot("pairs",tmp)	
									} else {
										new.plot("plot",tmp)	
								}
					})
			}
				
				#assign handler to form 
				i<-1
				handler.list<-data.frame(form.name=form.names,handler=handler.name)
				for(i in 1:nrow(handler.list))
				{
					adddroptarget(get(as.character(handler.list[i,1])),handler=get(as.character(handler.list[i,2])))
				}
		}
		
	make.plot.handler(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults)
	assign("devium.scatter.plot.notebook", mainWin,envir = devium)
	return(mainWin)
	}

#2D scatter plot (using ggplot2) for 2 vectors or basic pairs plot for a data.frame 
devium.qplot<- function(container=NULL) 
{
	check.get.packages(c("ggplot2","gWidgets","gWidgetsRGtk2"))
	options(device="gWidgetsRGtk2") # may not be nessesary
	
	#for debugging
	container= gwindow("test")
	gvarbrowser(container=container)
	
	mainWin = ggroup(horizontal = FALSE, container = container)
	
	#make tool bar on top
	make.tool.bar<-function(container=NULL)
	{
		
			mainWin = ggroup(horizontal = FALSE, container = container)
	
			#make tool bar on top
			buttonBar = ggroup(spacing = 0,container=mainWin)
			add(mainWin, buttonBar)
			
			#setting options second tool bar
			toolbar = list()

			toolbar$save$icon = "save"
			#toolbar$tmp1$separator = TRUE
			toolbar$save$handler = function(h, ...){
				tmp<-tryCatch(get("devium.plotnotebook.window",envir=devium),error= function(e){NULL})
				if (is.null(tmp) || !is.gWindow(tmp) ||  is.invalid(tmp)) {
					tmp<-gwindow("D E V I U M plot notebook", visible = TRUE)  
					add(tmp, ggraphicsnotebook())
					 assign("devium.plotnotebook.window", tmp, envir = devium)
				}
				else {
					tmp<-get("devium.plotnotebook.window",envir=devium)
					focus(tmp) 
				}
			}

			#toolbar$tmp2$separator = TRUE
			toolbar$plotnotebook$icon = "plot"
			toolbar$plotnotebook$handler = function(h, ...) {
				update.qplot(form.names=c(plot.opts,group.opts))
			}

			toolbar$help$icon = "help"
			toolbar$help$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			tmp = gtoolbar(toolbar)
			add(buttonBar, tmp, expand = TRUE)
		}
		
	#make top tool bar
	make.tool.bar(container=mainWin)
	
	#notebook to hold options
	.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1)
	
	#variables for plots
	#plot type and subset
	plot.type<-tmp<-glayout(container=.notebook,label="Type")
	tmp[1,2]<-assign("qplot.plot.type",gcombobox(c("point","line"),value="point"))
	tmp[2,1]<-glabel("subset",container=tmp)
	tmp[2,2]<-assign("qplot.subset.var",gedit("",container=tmp,width=20))
	
	asthetic<-glayout(container=.notebook,label="Asthetics")
	
	#options for plot asthetics
	plot.opts<-c("x","y","fill","color","size","shape","alpha") # form refrence
	plot.opts.labels<-c("  X-axis","  Y-Axis","  color","  border","  size","  shape/style","  transparency")
	plot.names<-c("x","y","fill","color","size","shape","alpha") # fxn input possibilities
	
	# to left align (has to be an easier way!)
	#check length and add white space to make all the same length
	tmp<-asthetic	
	for(i in 1:length(plot.opts))
		{	
			
			tmp[i,1]<-glabel(plot.opts.labels[i],container=tmp)
			tmp[i,2]<-assign(plot.opts[i],gedit("", container=tmp,width=20))
		}
	
	#options for group visualizations	
	groups<-glayout(container=.notebook,label="Groups")
	group.opts<-c("x.group","y.group") # form reference
	group.opts.labels<-c("x-axis group","y-axis group")
	group.names<-c("x.group","y.group") # fxn input possibilities
	# to left align (has to be an easier way!)
	#check length and add white space to make all the same length
	tmp<-groups
	for(i in 1:length(group.opts))
		{	
			tmp[i,1]<-glabel(group.opts.labels[i],container=tmp)
			tmp[i,2]<-assign(group.opts[i],gedit("", container=tmp,width=20))
		}
		
	#add options for groups visualizations
	tmp<-groups
	start<-length(group.opts)+1
	tmp[start,1]<-glabel("group visualization",container=tmp)
	tmp[start,2]<-assign("group.vis",gcombobox(c("point","line","bin2d","dotplot","freqpoly","hex","rect","bar","contour","crossbar","density","histogram","density2d","path","polygon","ellipse", "boxplot","'boxplot','jitter'"),value="point"))
	
	#bind with group.opts
	group.opts<-c(group.opts,"group.vis")
	
	#add handlers for drag and drop objects
	#should minimaly change name dislayed and call a fxn
	#which gathers inputs from all fomr fields
	#based on 
	# plot.opts 	= names of drag and drop objects
	# plot.defaults = generics for NA or NULL
	# plot.names 	= names of objects in another fxn
	
	#function to update form with droped objects name
	drop.names<-function(name)
		{
			#name = form obj name
			adddroptarget(name,handler = function(h,...) 
				{
					svalue(name)<-h$dropdata
				})

		}
	
	#function to gather all form inputs
	#based on plot opts 	
	gather.names<-function(names)
		{
			tmp<-lapply(1:length(names),function(i)
				{
					svalue(get(names[i]))
				})
			names(tmp)<-names	
			return(tmp)			
		}

	#function to convert output to character for call
	make.text<-function(arg.list)
		{
			#format facet object
			if(!arg.list$x.group==""){
				x.group<-strsplit(fixlc(arg.list$x.group),"\\$")
				data<-fixlc(x.group[[1]][1])
				x.group<-fixlc(x.group[[1]][2])
			} else{ 
				x.group<-"."
			}
			
			if(!arg.list$y.group=="")
				{
					y.group<-strsplit(fixlc(arg.list$y.group),"\\$")
					data<-fixlc(y.group[[1]][1])
					y.group<-fixlc(y.group[[1]][2])
				} else{ 
					y.group<-"."
				}
				
			if(y.group=="."&x.group=="."){
					facet.args<-""
				} else {
					facet.args<-paste("data=",data,",","facets=",y.group,"~", x.group)
				}
			#format geom object
			if(!arg.list$group.vis=="")
				{
					group.vis<-arg.list$group.vis
					group.vis.args<-paste("geom = ","'",group.vis,"'",sep="")
				} else{ 
					group.vis.args<-""
				}
				
				
			ignore<-c("group.vis","x.group","y.group")
			out<-as.character(sapply(1:length(arg.list),function(i)
				{
					if(!arg.list[i]==""){
					#use exact value id not being mapped from a data frame
					 if(length(unlist(strsplit(fixlc(arg.list[[i]]),"\\$")))==1){arg.list[i]<-paste("I(",arg.list[i],")", sep="")}
						#make shape a factor
						if(names(arg.list)[i]=="shape") #
							{
								paste(names(arg.list)[i],"=","as.factor(",arg.list[i],")")
							}
							
						if(names(arg.list)[i]=="alpha") #
							{
								paste(names(arg.list)[i],"=","as.factor(",arg.list[i],")")
							}
							
							if(names(arg.list)[i]=="group.vis"){ # for geome
								paste("geom =(",arg.list[i],")")
							}
							
							if(!names(arg.list)[i]%in%ignore){
								paste(names(arg.list)[i],"=",arg.list[i])
							}
						}
							
				}))
				
			return(c(out[!out=="NULL"],facet.args,group.vis.args))
			print(c(out[!out=="NULL"],facet.args,group.vis.args)) # debugging
		}
	#generate fxn input from form output
	get.inputs<-function(form.names=plot.opts)
		{
			#shape needs to be converted to a factor
			tmp<-gather.names(form.names)
			make.text(tmp)
		}
		
	#fxn to make plot 
	new.qplot<-function(tmp)
		{
			#add some arguments
			plot.arg<-"ggplot2::qplot("
			end.arg<-")"
			x<-paste(plot.arg,paste(tmp,collapse=", "),end.arg)
			if(names(dev.cur())=="null device"){x11() } #dev.new("X11")
			p<-eval(parse(text = as.expression(x)), envir = .GlobalEnv)
			print(p)
			tryCatch(bringToTop(which=dev.cur()),error=function(e){print("Check for error hidden window (Mac users)")})
		}
	
	#fxn to set set form handlers
	make.plot.handler<-function(form.names)
		{
			handler.name<-paste("handler",form.names,sep=".")
			i<-1
			for(i in 1:length(form.names))
				{
					#form obj
					obj<-get(form.names[i])
					#gather inputs and plot
					assign(handler.name[i],function(h,...) 
					{
							#upadate form to show drop source
							svalue(h$obj)<-h$dropdata
												
							#gather inputs
							tmp<-get.inputs(form.name=form.names)
							new.qplot(tmp)
					})
			}
				
				#assign handler to form 
				i<-1
				handler.list<-data.frame(form.name=form.names,handler=handler.name)
				for(i in 1:nrow(handler.list))
				{
					adddroptarget(get(as.character(handler.list[i,1])),handler=get(as.character(handler.list[i,2])))
				}
		}
	
		#update plot
		update.qplot<-function(form.names){
			tmp<-get.inputs(form.name=form.names)
			new.qplot(tmp)			
		}
	
		
	make.plot.handler(form.names=c(plot.opts,group.opts))
	#assign("devium.scatter.plot.notebook", mainWin,envir = devium)
	return(mainWin)
	}
	
#function to make gui to load data from csv or google docs
devium.data.import<-function (container = NULL) 
{
		#container=gwindow()
		#for import of: CSVs (read.csv, assume header)
		#for clipboard assume row and column names
		#google spreadsheets (RGoogleDocs)
		#Excel Worksheet (XLConnect)
		
		main = ggroup(horizontal = FALSE, container = container)
		
		#load from clipboard
        csv = gexpandgroup(text="From: Clipboard",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = csv)
        tbl = glayout(cont = g)
		tbl[2, 1:2]<-data.opts<-gradio(c("no name","with name","with row and column names", "with meta data"), container = tbl)
        tbl[3, 1] <- "New Name          " # has to be a better way to align this below
        tbl[3, 2] <- clip.name<-gedit("clipboard")
		
		#load from clipboard
		tbl[3, 3] <- (gapply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
					{
						name<-svalue(clip.name)
						#read from excel and format based on type
						type<-switch(svalue(data.opts),"with row and column names"="with.dimnames",
										"with meta data"="with.meta.data","with name"="with.name","no name"="no.name")
						tmp<-read.excel(type)#assign(name,tmp,envir=.GlobalEnv)
						#assign objects to global environment
						all<-c()
						if(class(tmp)=="list"){
							for(i in 1:length(tmp)){
									obj.name<-paste(name,".",names(tmp)[i],sep="")
									assign(obj.name,data.frame(tmp[[i]]),envir=.GlobalEnv)
									all<-c(all,obj.name)
								}
						}else{
							all<-obj.name<-name
							assign(obj.name,data.frame(tmp),envir=.GlobalEnv)
						}
						#check to make sure objects exists
						if(!tryCatch(all(sapply(all,exists)),error=function(e){TRUE}))
							{
								msg<-"Error occured. Make sure the object name doesn't start with a number, contain spaces or non-letter characters."
							} else {
								msg<-paste(all,"was successfully loaded")
							}
						done.info.GUI(msg)
					}
				))
		
		#load from CSV
        csv = gexpandgroup(text="From: CSV",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = csv)
        tbl = glayout(cont = g)
        tbl[1, 1] <- "Select Workbook" # has to be a better way to align this below
        tbl[1, 2] <- (filebrowse.excel.csv = gfilebrowse(text = "browse to select", action = invisible, container = tbl, filter = "*.csv", quote = FALSE))
		
		tbl[2, 1] <- "New Name          " # has to be a better way to align this below
        tbl[2, 2] <-the.name<- (gedit("data"))
										
		#hitting apply button loads the CSV and 
		tbl[2, 3] <- (gapply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
					{
					#selected file
					theFile = svalue(filebrowse.excel.csv )
					the.name<-svalue(the.name)
					if (!theFile == "browse to select")
						{
							#name for file
							tmp<-unlist(strsplit(basename(the.name), split = "\\."))[1]
							tmp<-chartr("-","_",tmp)
							file.name<-chartr(" ","_",tmp)
						
							#read as csv asume top row = header
							tryCatch(tmp.obj<-read.csv(theFile,header=TRUE),error=function(e){stop("error occured, check file")})
							assign(file.name,tmp.obj,envir=.GlobalEnv)#
							done.info.GUI(paste(file.name,"loaded"))
						}
				}))
				
		#import from Excel workbook
        excel = gexpandgroup(text="From: Excel",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = excel)
        tbl = glayout(cont = g)
        tbl[2, 1] <- "Select Workbook" # has to be a better way to align this below
        tbl[2, 2] <- (filebrowse.excel = gfilebrowse(text = "browse to select", action = invisible, container = tbl, filter = "*.csv", quote = FALSE))
		
		#dropdown box for worksheets and named ranges
		tbl[4, 1] <- "Spreadsheets"
		tbl[4, 2] <- (espreadsheets = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl))
		tbl[5, 1] <- "Named Ranges"
		tbl[5, 2] <- (enamed.ranges = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl))								
		
		#connect button loads worksheets and named ranges in workbook
		tbl[2, 3] <- (eapply = gbutton(text = "Connect",container = tbl, 
			handler = function(h,...)
					{
					#selected file
					theFile = svalue(filebrowse.excel)
					if (!theFile == "browse to select")
						{
							#get objects and place into dropdown boxes
							excel.objects<-viewExcelObject(theFile)
							
							#populate spreadsheets
							espreadsheets[] <- excel.objects[[1]]
							svalue(espreadsheets, index=TRUE) <- 1
						
							#populate named ranges
							enamed.ranges[] <- excel.objects[[2]]
							svalue(enamed.ranges, index=TRUE) <- 1
						}
				}))
				
		#load worksheet
		#apply button to load selectd google doc		
		tbl[4, 3] <- (eworksheet.apply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(espreadsheets)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from excel (may error of many objects on sheet due to best guess)
							tryCatch(
								{
									old.dir<-getwd()
									obj.path<-svalue(filebrowse.excel)
									wd<-dirname(obj.path)
									setwd(wd)
									workbook<-loadWorkbook(basename(obj.path))
									
									#get worksheet
									tmp.obj<-readWorksheet(workbook, sheet = theFile, header = TRUE,rownames=1)

									assign(file.name,tmp.obj,envir=.GlobalEnv)
									done.info.GUI(file.name)
									setwd(old.dir)
								},	error=function(e){stop("error occured, check excel object")})
						}
				}		
				))
				
		#load named range		
		tbl[5, 3] <- (eworksheet.apply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(enamed.ranges)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from excel (may error of many objects on sheet due to best guess)
							tryCatch(
								{
									old.dir<-getwd()
									obj.path<-svalue(filebrowse.excel)
									wd<-dirname(obj.path)
									setwd(wd)
									workbook<-loadWorkbook(basename(obj.path))
									
									#get named range
									tmp.obj<-readNamedRegion(wb, name = theFile, header = TRUE)

									assign(file.name,tmp.obj,envir=.GlobalEnv)
									done.info.GUI(file.name)
									setwd(old.dir)
								},	error=function(e){stop("error occured, check excel named range")})
						}
				}		
				))	

		#to load from google docs		
		gdocs = gexpandgroup(text="From: Google Spreadsheet",horizontal=FALSE, container=main, pos=2)
		gg = ggroup(horizontal = FALSE, cont = gdocs)
        tbl2 = glayout(cont = gg)
        tbl2[2, 1] <- "Google account"
        tbl2[2, 2] <- (gaccount = gedit("name@gmail.com", container= tbl2))
		tbl2[3, 1] <- "Password"
        tbl2[3, 2] <- tmp<-(gpassword = gedit("*********", container= tbl2))
		visible(tmp)<-FALSE # hide password
 
		# connect button --> connects to google account and gets 
		# list of available spreadsheets to the gdroplist above, asigned items in gdocs env
		# assigns connection to "gconnection" in gdocs env
		tbl2[3, 3] <- (gconnect = gbutton(text = "Connect",container = tbl2,
			handler = function(h,...)
				{
					tryCatch(
						{
							tmp<-GetGoogleDoc(account=svalue(gaccount),password=svalue(gpassword),connection="new")
							#update dropdownlist
							items<-tmp[[2]]
							assign("items", items, envir=gdocs)
							assign("gconnection",get(tmp[[1]],envir=googDocs),envir = gdocs)
							gspreadsheets[] <- items
							svalue(gspreadsheets, index=TRUE) <- 1
						}, error = function(e){"Check account name and password"})
				}		
				))
				
		#dropdown box for available spreadsheets
		gdocs<-new.env()
		assign("items", c("not connected"), envir=gdocs)
		tbl2[4, 1] <- "Spreadsheets"
		tbl2[4, 2] <- (gspreadsheets = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl2))
				
		#apply button to load selectd google doc		
		tbl2[4, 3] <- (gapply = gbutton(text = "Apply",container = tbl2, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(gspreadsheets)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from google as csv asume top row = header
							tryCatch(
								{
									tmp.obj<-getWorksheets(theFile,get("gconnection",envir=gdocs))
									#later add ability to load from diffrent sheets in the same workbook
									# for now get first sheet
									target<-names(tmp.obj)[1]
									sheet<-sheetAsMatrix(tmp.obj[[target]],header=TRUE, as.data.frame=TRUE, trim=TRUE)
									assign(file.name,sheet,envir=.GlobalEnv)
									done.info.GUI(file.name)
								},	error=function(e){stop("error occured, check file")})
						}
				}		
				))
		
		assign("devium.data.import.notebook",main,envir=devium) #optional
		return(main)
    }

#export data gui
#function to make gui to lad data from csv or google docs
devium.data.export<-function (container = NULL) 
{
		#container=gwindow()
		#for import of: CSVs (read.csv, assume header)
		#for clipboard assume row and column names
		#google spreadsheets (RGoogleDocs)
		#Excel Worksheet (XLConnect)
		
		main = ggroup(horizontal = FALSE, container = container)
		
		#load from clipboard
        clip = gexpandgroup(text="To: Clipboard",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = clip)
        tbl = glayout(cont = g)
		tbl[1, 1:2]<-data.opts<-gradio(c("no name","with name","with row and column names", "with meta data"), container = tbl)
        tbl[2,1]<-glabel("  object")
		tbl[2,2]<-tmp.obj<-gedit("drop name here")
		
		#load from clipboard
		tbl[3, 2] <- (gapply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
					{
						#read from excel and format based on type
						type<-switch(svalue(data.opts),"with row and column names"="with.dimnames",
										"with meta data"="with.meta.data","with name"="with.name","no name"="no.name")
						object<-tryCatch(get(svalue(tmp.obj)),error=function(e){NULL})		
						
						write.to.clipboard(obj=object,type=type)#assign(name,tmp,envir=.GlobalEnv)
		
					}
				))
		
		assign("devium.data.export.notebook",main,envir=devium) #optional
		return(main)
    }

# function to load R data sets
devium.load.Rdataset<-function (width = 550, height = 400) 
{
			win =gwindow("Load data set", v = T)
			size(win) <- c(width, height)
			group = ggroup(horizontal = FALSE, container = win, expand = TRUE)
			dataSetHandler = function(h, ...) {
				dataSets = svalue(dataSetList, drop = FALSE)
				for (i in 1:nrow(dataSets)) {
					dataset = dataSets[i, 1]
					package = dataSets[i, 2]
					command = Paste("data(", dataset, ",package=\"", 
						package, "\")")
					cat("> ", command, "\n")
					svalue(status) <- Paste("attach data set ", dataset)
					do.call("data", list(dataset, package = package))
					svalue(status)
				}
			}
			
			getDataSets<-function (...) 
				{
					dataSets = data()$results
					dataSets = dataSets[, c(3, 1, 4)]
					return(dataSets)
				}

			dataSetList = gtable(getDataSets(), multiple = TRUE, filter.column = 2, 
				handler = dataSetHandler)
			add(group, dataSetList, expand = TRUE)
			buttonGroup = ggroup(container = group)
			addSpring(buttonGroup)
			gbutton("cancel", container = buttonGroup, handler = function(h, 
				...) dispose(win))
			status = gstatusbar("Double click data set to load", container = group)
			invisible(win)
		}		

#based on pmg function (should import these!)		
devium.summary<-function(obj,...)
{
    objName = deparse(substitute(obj))
    if (is.character(obj) && length(obj) == 1) {
        objName = obj
        obj = svalue(obj)
    }
    group = ggroup(horizontal = FALSE, ...)
    icon = stockIconFromClass(class(obj))
    add(group, gimage(icon, dirname = "stock", size = "DIALOG"))
    table = glayout(adjust = "left")
    add(group, table)
    table[1, 1] = glabel("<b>Name:</b>", markup = TRUE)
    table[1, 2] = glabel(objName)
    table[2, 1] = glabel("<b>Kind:</b> ", markup = TRUE)
    table[2, 2] = glabel(paste(class(obj), sep = "", collapse = ", "))
    table[3, 1] = glabel("<b>Size:</b>", markup = TRUE)
	
	#sneaky str
	str1<-function (obj) 
		{
			md <- mode(obj)
			lg <- length(obj)
			objdim <- dim(obj)
			if (length(objdim) == 0) 
				dim.field <- paste("length:", lg)
			else {
				dim.field <- "dim:"
				for (i in 1:length(objdim)) dim.field <- paste(dim.field, 
					objdim[i])
				if (is.matrix(obj)) 
					md <- "matrix"
			}
			obj.class <- oldClass(obj)
			if (!is.null(obj.class)) {
				md <- obj.class[1]
				if (inherits(obj, "factor")) 
					dim.field <- paste("levels:", length(levels(obj)))
			}
			list(type = md, dim.field = dim.field)
		}

    if (!is.function(obj)) {
        theSize = str1(obj)$dim.field
        table[3, 2] = glabel(theSize)
    }
    else {
        table[3, 2] = glabel("NA")
    }
    stamp = NA #Timestamp(obj)
    if (!is.na(stamp)) {
        table[4, 1] = glabel("<b>Last modified:</b>", markup = TRUE)
        table[4, 2] = glabel(format(as.Date(stamp), "%B %d, %Y"))
    }
    table[5, 1] = glabel("<b>Preview:</b>", markup = TRUE)
    theValue = capture.output(eval(obj))
    if (length(theValue) > 10) 
        theValue = c(theValue[1:10], "... 8< snipped >8 ...")
    theHead = gtext(font.attr = c("monospace"))
    add(theHead, theValue)
    enabled(theHead) <- FALSE
    add(group, theHead, expand = TRUE)
    visible(table) <- TRUE
    return(group)
}
 
#data modification gui
devium.modify.data.gui<-function(container=NULL)
{
		#results stored in get("devium.modify.data.object",envir=devium)
		#for debugging
		#container= gwindow("test")
		
		#remove old object upon start up
		tryCatch(rm("devium.modify.data.object",envir=devium),error=function(e){})
		
		mainWin = ggroup(horizontal = FALSE, container = container)
		
		#make tool bar on top
		make.tool.bar<-function(container=NULL)
		{
			
				mainWin = ggroup(horizontal = FALSE, container = container)
		
				#make tool bar on top
				buttonBar = ggroup(spacing = 0,container=mainWin)
				add(mainWin, buttonBar)
				
				#setting options second tool bar
				toolbar = list()

				#toolbar$tmp2$separator = TRUE
				toolbar$execute$icon = "execute"
				toolbar$execute$handler = function(h, ...) {
				check.get.obj(gui.objects,main.object="devium.modify.data.object") #assign all objects in form to main.object
				tmp<-get("devium.modify.data.object",envir=devium)#get object
				if(is.null(tmp$devium.data)|tmp$devium.data=="")
					{ 
							return()
					} else {
						#carry out execute/operations save to main.object$new.data and then refresh data.view
						tmp.data<-get(tmp$devium.data)
						
						#switch basd on options
						switch(tmp$modify.options,
						remove 		= .local<-function(){tmp.data[!rownames(tmp.data)%in%tmp$devium.data.selected.rows,!colnames(tmp.data)%in%tmp$devium.data.selected.cols]},
						split  		= .local<-function(){},
						transpose 	= .local<-function(){data.frame(t(tmp.data))},
						merge 		= .local<-function(){},
						subset      = .local<-function(){})
						
						obj<-.local()
						d.assign("new.data",obj,main.object="devium.modify.data.object")
						#need to delete old and make a new gtabel
						to.delete<-get("data.view",envir=devium)
						delete(data.view.group,to.delete)
						data.view<-assign("data.view",gtable(obj),envir=devium)
						add(data.view.group,data.view)
						}
					}
				
				toolbar$save$icon = "save"
				toolbar$save$handler = function(h, ...) 
				{
					check.get.obj(gui.objects,main.object="devium.modify.data.object")
					ask.gui(paste("Save new object as:",get("devium.modify.data.object",envir=devium)$save.as.name,"?",sep=" "),
							OK=function(h,...){
								.<-get("devium.modify.data.object",envir=devium)
								assign(.$save.as.name,.$new.data,envir=.GlobalEnv)
								})
				}	
				
				toolbar$help$icon = "help"
				toolbar$help$handler = function(h, ...) 
				{
					done.info.GUI("to do--> write help file.")
				}	
				
				toolbar$cancel$icon = "cancel"
				toolbar$cancel$handler = function(h, ...) 
				{
					done.info.GUI("to do--> write help file.")
				}	
				
				tmp = gtoolbar(toolbar)
				add(buttonBar, tmp, expand = TRUE)
			}
			
		#make top tool bar
		make.tool.bar(container=mainWin)
		
		#gui objects
		gui.objects<-c("devium.data","devium.data.selected.cols","devium.data.selected.rows","modify.options","save.as.name")
		
		 #fxn to create the environment "devium" if it does not exist
		 create.devium.env()	
		 
		 
		 
		 if(!exists("devium.modify.data.object",envir=devium)) # should be object
			{
				tmp<-list()
				# some defaults
				assign("devium.modify.data.object",tmp,envir=devium)
			}
			
	 
		#target data and save as name
		tmp<-ggroup(horizontal=TRUE,container=mainWin)
		
		#gradio for what to method to execute
		assign("modify.options",gradio(c("transpose","split","remove","merge","subset"),container=tmp,horizontal=TRUE,
				handler=function(h,...)
					{
						d.assign("devium.data.selected.cols",svalue(get("devium.data.selected.cols",envir=devium)),main.object="devium.modify.data.object")
						d.assign("devium.data.selected.rows",svalue(get("devium.data.selected.rows",envir=devium)),main.object="devium.modify.data.object")
					
					}),envir=devium)
					
		midlevel<-ggroup(horizontal=TRUE,container=mainWin)
		data.tab<-tmp<-gframe("Data",container=midlevel)
		assign("devium.data",gedit("",container=tmp),envir=devium)
		adddroptarget(get("devium.data",envir=devium),handler = function(h,...) 
		{
				#svalue(h$obj)<-h$dropdata
				svalue(h$obj)<-h$dropdata
				d.assign("devium.data",svalue(h$obj),main.object="devium.modify.data.object")
				#set values of combo boxes
				main<-tryCatch(get(get("devium.modify.data.object",envir=devium)$devium.data),error=function(e){data.frame(NULL)})
				obj<-tryCatch(get("devium.data.selected.cols",envir=devium),error=function(e){})
				tryCatch(obj[]<-colnames(main),error=function(e){})
				obj<-tryCatch(get("devium.data.selected.rows",envir=devium),error=function(e){})
				tryCatch(obj[]<-rownames(main),error=function(e){})
		})
		
		save.as.tab<-tmp<-gframe("Save as",container=midlevel)
		assign("save.as.name",gedit("new.data",container=tmp),envir=devium)
		
		
		#ggroup to hold options
		midlevel<-gframe("Selections",horizontal=TRUE,container=mainWin)
		tmp<-ggroup(container=midlevel)
		
		select.rows<-gexpandgroup("  Select: Rows",container=tmp)
		tmp2<-ggroup(container=select.rows,horizontal=FALSE) # need to size correctly
		add(tmp2,glabel("_____________________________________"))
		assign("devium.data.selected.rows",gcheckboxgroup("         ",selected = 0,container=tmp2,use.table=TRUE),envir=devium)#,envir=devium
		#add(tmp2,glabel("_____________________________________"))
		
		tmp<-ggroup(container=midlevel)			
		select.cols<-gexpandgroup("  Select: Columns",container=tmp)
		tmp2<-ggroup(container=select.cols,horizontal=FALSE) # need to size correctly
		add(tmp2,glabel("_____________________________________"))
		assign("devium.data.selected.cols",gcheckboxgroup("         ",selected = 0,container=tmp2,use.table=TRUE),envir=devium)#,envir=devium
		#add(tmp2,glabel("_____________________________________"))
		
		#gtable to view results
		gseparator(horizontal = TRUE, container = mainWin)
		#check data first
		
		newdata<-tryCatch(get(get("devium.modify.data.object",envir=devium)$new.data),error=function(e){NULL})
		if(is.null(newdata)){newdata<-tryCatch(get(get("devium.modify.data.object",envir=devium)$devium.data),error=function(e){data.frame(variables=character(0), stringsAsFactors=FALSE)})}
		data.view.group<-ggroup(container=mainWin,horizontal=FALSE,expand=TRUE)
		#add(data.view.group,glabel("________________________________________________________"))
		data.view<-assign("data.view",gtable(newdata, container=data.view.group),envir=devium)
		return(mainWin)
}

#data transformation gui
devium.transform.data.gui<-function(container=NULL)
{
	#results stored in get("devium.data.object",envir=devium)
	#for debugging
	#container= gwindow("test")
	mainWin = ggroup(horizontal = FALSE, container = container,expand=TRUE)

	#make tool bar on top
	make.tool.bar<-function(container=NULL)
	{

			mainWin = ggroup(horizontal = FALSE, container = container)

			#make tool bar on top
			buttonBar = ggroup(spacing = 0,container=mainWin)
			add(mainWin, buttonBar)

			#setting options second tool bar
			toolbar = list()

			#toolbar$tmp2$separator = TRUE
			toolbar$execute$icon = "execute"
			toolbar$execute$handler = function(h, ...) {
			check.get.obj(gui.objects,main.object="devium.data.object") #assign all objects in form to main.object
			tmp<-get("devium.data.object",envir=devium)#get object
			if(is.null(tmp$devium.data)|tmp$devium.data=="")
				{ 
						return()
				} else {
					#transformed obj
						trans.obj<-transform.to.normal(data=get(tmp$devium.data),data.name=tmp$devium.data, test.method=tmp$devium.normalize.test, 
							alpha = 0.05 , force.positive=TRUE, transformation=tmp$devium.normalize.transformation)
					#scale and center
					tmp.data<-scale.data(trans.obj[[1]])
					trans.obj[[1]]<-tmp.data
					#save output to global	
					transform.to.normal.output(obj=trans.obj,name="transformed.data", envir=devium)
				}
			}

			toolbar$help$icon = "help"
			toolbar$help$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	

			toolbar$plot$icon = "plot"
			toolbar$plot$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			tmp = gtoolbar(toolbar)
			add(buttonBar, tmp, expand = TRUE)
		}

	#make top tool bar
	make.tool.bar(container=mainWin)

	#gui objects
	gui.objects<-c("devium.data","devium.data.scaling","devium.data.center","devium.normalize.test","devium.normalize.transformation")

	 #fxn to create the environment "devium" if it does not exist
	 create.devium.env()	

	 if(!exists("devium.data.obj",envir=devium)) # should be object not obj?
		{
			tmp<-list()
			# some defaults
			assign("devium.data.obj",tmp,envir=devium)

		}

	#target data
	data.tab<-tmp<-gframe("Data",container=mainWin)
	assign("devium.data",gedit("",container=tmp),envir=devium)
	adddroptarget(get("devium.data",envir=devium),handler = function(h,...) 
	{
			svalue(h$obj)<-h$dropdata
			d.assign("devium.data",get(id(h$dropdata),main.object="devium.data.object"))
	})

	#notebook to hold options
	.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1,expand=TRUE)

	#transform: normalize, scale, center
	tmp<-glayout(container=.notebook,label="Scale")
	tmp[1,1]<-glabel("  scale",container=tmp)
	tmp[1,2]<-assign("devium.data.scaling",gcombobox(c("none","pareto", "vector", "uv","range"),selected = 4,container=tmp),envir=devium)#,envir=devium
	tmp[2,1]<-glabel("  center",container=tmp)
	tmp[2,2]<-assign("devium.data.center",gcheckbox("",checked = TRUE,container=tmp),envir=devium)#, envir=devium
	tmp<-glayout(container=.notebook,label="Transform")
	tmp[1,1]<-glabel("  transformation",container=tmp)
	tmp[1,2]<-assign("devium.normalize.transformation",gcombobox(c("none","log","BOX-COX"),container=tmp),envir=devium)#,envir=devium
	tmp[2,1]<-glabel("  test normality",container=tmp)
	tmp[2,2]<-assign("devium.normalize.test",gcombobox(c("Anderson-Darling","Shapiro-Wilk","Cramer-von Mises","Kolmogorov-Smirno","Shapiro-Francia"),container=tmp),envir=devium)#,envir=devium
	return(mainWin)
}
	
#generate data networks
devium.network.gui<-function(container=NULL)
{
		#results stored in get("devium.network.object",envir=devium)
		#for debugging
		#container= gwindow("test")
		mainWin = ggroup(horizontal = FALSE, container = container,expand=TRUE)
		
		#make tool bar on top
		make.tool.bar<-function(container=NULL)
		{
			
				mainWin = ggroup(horizontal = FALSE, container = container)
		
				#make tool bar on top
				buttonBar = ggroup(spacing = 0,container=mainWin)
				add(mainWin, buttonBar)
				
				#setting options second tool bar
				toolbar = list()

				#toolbar$tmp2$separator = TRUE
				toolbar$execute$icon = "execute"
				toolbar$execute$handler = function(h, ...) {
				check.get.obj(object=gui.objects,main.object="devium.network.object") #assign all objects in form to main.object
				tmp<-get("devium.network.object",envir=devium)#get object
				if(is.null(tmp))
					{ 
							return()
					} else {
						#function to execute based on arguments set in tmp
						devium.network.execute(tmp,filter=as.numeric(tmp$devium.network.edge.list.weight.cutoff))
					}
				}

				toolbar$help$icon = "help"
				toolbar$help$handler = function(h, ...) 
				{
					done.info.GUI("to do--> write help file.")
				}	
				
				toolbar$plot$icon = "plot"
				toolbar$plot$handler = function(h, ...) 
				{
					
					#update options main objects
					check.get.obj(object=gui.objects,main.object="devium.network.object") #assign all objects in form to main.object
					.<-get("devium.network.object",envir=devium) # main object
					
					#check to see if edge list exists else create one from GUI set options
					object.name<-paste(.$devium.network.target.object,".network.edge.list",sep="")
					if(!exists(object.name)){devium.network.execute(.)}							

					#try to get labels
					labs<-tryCatch(as.character(unlist(get(.$"devium.network.labels"))),error=function(e){NULL})
					if(is.null(labs)){labs<-tryCatch(as.character(unlist(gget(.$"devium.network.labels"))),error=function(e){.$"devium.network.labels"})}
					if(length(labs)==0){labs<-""}
					
					#objects for visual properties
					visual.par<-list(
					layout = .$"devium.network.layout",  #have to get this later
					vertex.label = labs # 
					#vertex.color ="gray",
					#vertex.size = 6,
					#vertex.label.dist=-.3
					)
					
					#cut out NULL
					keep<-sapply(1:length(visual.par), function(i)
						{
							all(!visual.par[[i]]=="")
						})
						
					tmp<-visual.par[c(1:length(visual.par))[keep]]
					names(tmp)<-names(visual.par)[c(1:length(visual.par))[keep]]
					graph.par.obj<-tmp
					#update/recalculate edge list based on edge weight selection (maybe should be done upon edge change)
					tmp<-get("devium.network.object",envir=devium)#get object
					if(is.null(tmp))
					{ 
							return()
					} else {
						#function to execute based on arguments set in tmp
						#devium.network.execute(tmp,filter=as.numeric(tmp$devium.network.edge.list.weight.cutoff))
						#get full object and refilter
						tmp<-.$devium.network.edge.list.full
						keep<-as.numeric(as.character(unlist(tmp[,3])))>=as.numeric(.$devium.network.edge.list.weight.cutoff)
						#assign ass calculated object
						d.assign("devium.network.edge.list.calculated",tmp[keep,],main.object=devium.network.object)
					}
					
					#plot
					devium.igraph.plot(edge.list=get("devium.network.object",envir=devium)$"devium.network.edge.list.calculated", 
										plot.type=get("devium.network.object",envir=devium)$"devium.network.plot.type",
										graph.par.obj=graph.par.obj)
										#
							}	
				tmp = gtoolbar(toolbar)
				add(buttonBar, tmp, expand = TRUE)
			}
			
		#make top tool bar
		make.tool.bar(container=mainWin)
		
		#gui objects possible options
		gui.objects<-c("devium.network.target.object", # data.frame or character
						"devium.network.target.type", # data or cid
						"devium.network.edge.list.type", # calculation type
						"devium.network.plot.type",# plot type
						"devium.network.layout",#layout
						"devium.network.labels",#labels
						"devium.network.edge.list.weight.cutoff",
						"devium.network.edge.list.weight.cutoff.use.FDR"#cutt off to filter edge list
						) 
		
		 #fxn to create the environment "devium" if it does not exist
		 create.devium.env()	
		 
		 #check for the storage object for the gui 
		 if(!exists("devium.network.object",envir=devium))
			{
				tmp<-list()
				# some defaults
				assign("devium.network.object",tmp,envir=devium)
			}
	 
		#target data
		data.tab<-tmp<-gframe("Object",container=mainWin)
		assign("devium.network.target.object",gedit("",container=tmp),envir=devium)
		adddroptarget(get("devium.network.target.object",envir=devium),handler = function(h,...) 
		{
				svalue(h$obj)<-h$dropdata
				d.assign("devium.network.target.object",get(id(h$dropdata),main.object="devium.network.object"))
				#change options for edge type
				combo.box<-tryCatch(get("devium.network.edge.list.type",envir=devium), error=function(e){NULL})
				filter.box<-tryCatch(get("devium.network.edge.list.weight.cutoff",envir=devium), error=function(e){NULL})
				
				#test for a data.frame
				tmp<-tryCatch(get(svalue(get("devium.network.target.object",envir=devium))),error=function(e) {NULL})
				if(class(tmp)=="data.frame"|class(tmp)=="matrix")
					{
						options<-c("pearson correlations","spearman correlations","biweight mid-correlation")
						filter.value<-0.05
					} else{ # should be character or 1 column data frame
						options<-c("Tanimoto distances","KEGG reaction pairs")
						filter.value<-0.7
					}
				combo.box[]<-options
				svalue(combo.box, index=TRUE) <- 1
				tryCatch(filter.box[]<-filter.value, error=function(e){NULL})	#one day I will stop fixing leaks like this
		})
		
		tmp<-assign("devium.network.target.type",gcombobox(c("Data","CIDs"),selected = 1,container=tmp, 
			handler=function(h,...)
			{
			combo.box<-tryCatch(get("devium.network.edge.list.type",envir=devium), error=function(e){NULL})
					if(is.null(combo.box))
						{
							return()
						} else { 
							#get args to determine options
							tmp<-svalue(get("devium.network.target.type",envir=devium))
							if(tmp=="Data")
								{
									options<-c("pearson correlations","spearman correlations","biweight mid-correlation")
									filter.value<-0.05
								} else{
									options<-c("Tanimoto distances","KEGG reaction pairs")
									filter.value<-0.7
								}
							combo.box[]<-options
							svalue(combo.box, index=TRUE) <- 1
							filter.box<-tryCatch(get("devium.network.edge.list.weight.cutoff",envir=devium), error=function(e){NULL})
							filter.box[]<-filter.value
						}
			} ),envir=devium)#,envir=devium
			
		#notebook to hold options
		.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1,expand=TRUE)
		
		##EDGES
		tmp<-edges<-glayout(container=.notebook,label="Edges")
		tmp[1,1]<-glabel("  edge list",container=tmp)
		tmp[1,2]<-assign("devium.network.edge.list.type",gcombobox(c("Input object and edge list type"),selected = 1,container=tmp),envir=devium)# options should change based on "devium.network.target.type"
		tmp[1,3]<-glabel("  cutoff",container=tmp)
		tmp[1,4]<-assign("devium.network.edge.list.weight.cutoff",gedit(" ",width = 10,container=tmp, #gspinbutton(from=0.01, to = .99, by=0.01,selected = 0.05, editable = TRUE
				handler=function(h,...)
					{
					obj<-get("devium.network.edge.list.weight.cutoff",envir=devium)
					d.assign("devium.network.edge.list.weight.cutoff",svalue(obj),main.object="devium.network.object")
					}),envir=devium) #should change based on "devium.network.edge.list.type"
		tmp[2,4]<-assign("devium.network.edge.list.weight.cutoff.use.FDR",gcheckbox(text="use FDR", container=tmp,
				handler=function(h,...)
							{
							obj<-get("devium.network.edge.list.weight.cutoff.use.FDR",envir=devium)
							d.assign("devium.network.edge.list.weight.cutoff.use.FDR",svalue(obj),main.object="devium.network.object")
							}),envir=devium)
		
		#VERTICES
		tmp<-glayout(container=.notebook,label="Vertices")
		
		#plotting
		tmp<-glayout(container=.notebook,label="Global")
		tmp[1,1]<-glabel("  visualization",container=tmp)
		tmp[1,2]<-assign("devium.network.plot.type",gcombobox(c("static","interactive", "3D-plot", "Cytoscape"),selected = 1,container=tmp),envir=devium)#,envir=devium#,envir=devium
		tmp[2,1]<-glabel("  layout",container=tmp)
		tmp[2,2]<-assign("devium.network.layout",
						gcombobox(c("layout.random","layout.circle","layout.sphere","layout.fruchterman.reingold",
							"layout.kamada.kawai","layout.spring","layout.reingold.tilford","layout.fruchterman.reingold.grid",
							"layout.lgl","layout.graphopt","layout.mds","layout.svd","layout.drl"),
						selected = 4,container=tmp),envir=devium)#,envir=devium#,envir=devium
		tmp[3,1]<-glabel("  labels",container=tmp)
		tmp[3,2]<-assign("devium.network.labels",gedit("",container=tmp),envir=devium)
		adddroptarget(get("devium.network.labels",envir=devium),handler = function(h,...) 
		{
				svalue(h$obj)<-h$dropdata
				d.assign("devium.network.labels",get(id(h$dropdata),main.object="devium.network.object"))			
		})
		
		svalue(.notebook)<-1
		return(mainWin)
}

#completely taking out pmg functions 
devium.gui<-function (width = 850, height = .75 * width) 
{	
	#remove any old widgets 
	#objs<-objects(,envir=devium)
	#don't remove functions (later fix this)
	#is.fxn<-!sapply(1:length(objs),is.function)
	#rm(list = objs[is.fxn], pos = devium)
	
	#main window dimensions and options
	rightWidth = width * 0.6
	mainHeight = height * 0.8
	options(guiToolkit = "RGtk2")
	#cliType = "console" # use console instead of in gui cli

	#load accesory packages
	# fix this to download te package if not available
	 check.get.packages(c("RGtk2","gWidgets","gWidgetsRGtk2","proto","cairoDevice"))
	 
	 #accesory functions from R package "pmg" 
	 # avoid loading package due to non controllable GUI popup

	 #using an new environment instead of namespace for now (probably a very bad idea?)
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
	 create.devium.env()	
	 
	 # will need to change everything when namespace is implemented... 
	 devium.closeALL<-function () 
		{
			close.these<-with(devium,objects())[grep("devium",with(devium,objects()))]
			for (i in 1:length(close.these)) 
				{
				if(exists(close.these[i]))
					{
						window = get(i,envir = devium)
						try(dispose(window), silent = TRUE)
						assign(i, NULL, envir = devium)
					}	
				}
		}			

	#basic checks
	if (!interactive()) 
		{
			cat("Devium requires an interactive environment\n")
			return()
		}

	#check to see if the devium main window exists 
	#if not create one
	create.devium.main<-function()
		{
		tmp<-tryCatch(get("devium.main.window", envir=.GlobalEnv),error=function(e){NULL})
		if (is.null(tmp) || !is.gWindow(tmp) || is.invalid(tmp)) 
			{
				#assign("devium.main.window", gwindow("D E V I U M", visible = FALSE), "devium") # no class
				tmp<-gwindow("D E V I U M", visible = TRUE) # FALSE #uses the proto class
				size(tmp) <- c(width, height)
				assign("devium.main.window", tmp, envir=.GlobalEnv)
			} else {
				return()
			}
		}
		
	create.devium.main()
	
	#creation of main GUI objects
	#------------------------------------------------
    assign("devium.notebook", gnotebook(closebuttons = TRUE, dontCloseThese = 1, tearable = FALSE),envir= devium) # notebook for holding more GUIs for plotting and analyses
    assign("devium.statusBar", gstatusbar("", container = NULL),envir= devium) # status bar at the bottom
    mainGroup = ggroup(horizontal = FALSE, spacing = 0, container = get("devium.main.window", envir=.GlobalEnv), expand = TRUE) # top level group for window
   
	#Top menue
	#-------------------------------------------------
	devium.menu<-list()
	devium.menu$File$`Save Analysis`$handler<-function(h,...){ }
	devium.menu$File$`Clear Workspace`$handler<-function(h,...)
		{
			ask.gui("Delete all objects?",OK=function(h,...)
			{
				objs <- ls(pos = ".GlobalEnv")
				#id for functions 
				is.fxn<-c(1:length(objs))[sapply(1:length(objs),function(i){is.function(get(objs[i]))})]
				#id for devium objects
				is.devium<-grep("devium",objs)
				delete<-c(1:length(objs))[-unique(c(is.fxn, is.devium))]
				rm(list = objs[delete], pos = ".GlobalEnv")
				#for now restore workspace from directory using fxn below
			})	

		}
		
	devium.menu$Data$`Load`$handler<-function(h,...)
		{
			add(get("devium.notebook",envir=devium),devium.data.import(), label = "Data Import") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}
	devium.menu$Data$`Export`$handler<-function(h,...)
		{
			add(get("devium.notebook",envir=devium),devium.data.export(), label = "Data Export") 
			#tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			#svalue(tmp)<-.2
		}
		
	devium.menu$Data$`R Dataset`$handler<-function(h,...){devium.load.Rdataset() }
	devium.menu$Data$`Modify`$handler<-function(h,...){
		add(get("devium.notebook",envir=devium),devium.modify.data.gui(), label = "Modify Data") 
		tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
		svalue(tmp)<-.2
	}
	
	#METHODS
	devium.menu$Methods$`Transform`$handler<-function(h,...){
		
			add(get("devium.notebook",envir=devium),devium.transform.data.gui(), label = "Transform Data") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}
		
	devium.menu$Methods$`PCA`$handler<-function(h,...)
		{
			add(get("devium.notebook",envir=devium),devium.PCA.gui(), label = "PCA") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}
		
		devium.menu$Methods$`Network`$handler<-function(h,...)
		{
			#x11() # need to change this for non-windows machines
			add(get("devium.notebook",envir=devium),devium.network.gui(), label = "Network") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}	
		
		
	#PLOTS
	devium.menu$Plots$`Scatter Plot`$handler<-function(h,...)
		{
			#x11() # need to change this for non-windows machines
			add(get("devium.notebook",envir=devium),devium.scatter.plot(), label = "Scatter Plot") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}
		
	devium.menu$Plots$`Dynamic Plot`$handler<-function(h,...)
		{
			#x11() # need to change this for non-windows machines
			add(get("devium.notebook",envir=devium),devium.qplot(), label = "Plot Builder") 
			tmp<-get("devium.bottom.panedgroup",envir=devium) # have to do this to make it work
			svalue(tmp)<-.2
		}	
		
	
		
	
	#make top menue
	assign("devium.menuBar", gmenu(devium.menu, container = NULL),envir= devium) # top most menue
	add(mainGroup, get("devium.menuBar",envir=devium)) # very tedious to do this, no wonder namespace is preffereble
	
	#lower menue (2nd from the top) w/ images
	#----------------------------------------------------------
	
	#make a button bar below menue  
    buttonBar = ggroup(spacing = 0)
    add(mainGroup, buttonBar) # top most menu
	
	#setting options second tool bar
    toolbar = list()
    toolbar$quit$icon = "quit"
	toolbar$quit$handler = function(h, ...) {
		tmp<-get("devium.main.window",envir=.GlobalEnv)
        dispose(tmp)
        devium.closeALL()
		}

	toolbar$save$icon = "save"
	toolbar$tmp1$separator = TRUE
	toolbar$save$handler = function(h, ...) {
		gfile("Save workspace", type = "save", action = "save.image")
		}
	
   
     toolbar$plotnotebook$icon = "plot"
	 toolbar$plotnotebook$handler = function(h, ...) {
		tmp<-tryCatch(get("devium.plotnotebook.window",envir=devium),error= function(e){NULL})
        if (is.null(tmp) || !is.gWindow(tmp) ||  is.invalid(tmp)) {
			tmp<-gwindow("D E V I U M plot notebook", visible = TRUE)  
			add(tmp, ggraphicsnotebook())
			 assign("devium.plotnotebook.window", tmp, envir = devium)
        }
        else {
            tmp<-get("devium.plotnotebook.window",envir=devium)
			focus(tmp)
        }
    }
	
    toolbar$tmp2$separator = TRUE
    toolbar$help$handler = function(h, ...) 
		{
		obj<-tryCatch(get("devium.helpBrowser.window",envir=devium),error= function(e){NULL})
        if (class(obj) == "NULL") 
			{
				assign("devium.helpBrowser.window", ghelpbrowser(), envir=devium)
			} else {
				tmp<-get("devium.helpBrowser.window",envir=devium)
				focus(tmp) <- TRUE
			}
		}
	
    toolbar$help$icon = "help"
    tmp = gtoolbar(toolbar)
    assign("devium.toolBar", tmp,envir=devium)
	add(buttonBar, tmp, expand = TRUE)
	
	#lower major portion of the app
	#--------------------------------------------------------------	
    bottomGroup = ggroup(horizontal = TRUE,container=mainGroup) 	#lower group holding var browser and note bok for calling guis
	
    # left most drop area with small images
	#--------------------------------------------------------------
    devium.droparea = ggroup(horizontal = FALSE, container = bottomGroup) # space for left most drop area
	#making side bar of small images with drag and drop
	tmp<-devium.droparea
	addSpace(tmp, 20)
    editDrop = gimage("edit", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    plotDrop = gimage("plot", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    summaryDrop = gimage("info", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    removeDrop = gimage("delete", dirname = "stock", container = tmp)
    addSpring(tmp)
	assign("devium.droparea",tmp,envir = devium)
	
	#handlers for the small image sidebar
    adddroptarget(summaryDrop, handler = function(h, ...) {
	#obj<-get("devium.cli",envir=devium)
        #svalue(obj) <-Paste("summary(", list(h$dropdata),")")
		rpel(Paste("summary(", list(h$dropdata),")"))
    })
	
    adddroptarget(plotDrop, handler = function(h, ...) {
		#obj<-get("devium.cli",envir=devium)
        #svalue(obj) <- Paste("plot(", list(h$dropdata), ")")
		rpel(Paste("plot(", list(h$dropdata), ")"))
    })
	
	#rpel is eval text strin in a given environment
    adddroptarget(editDrop, handler = function(h, ...) {
        rpel(Paste("fix(", list(h$dropdata), ")"))
    })
	
    adddroptarget(removeDrop, handler = function(h, ...) {
		#obj<-get("devium.cli",envir=devium)
       # svalue(obj) <- Paste("rm(", list(h$dropdata), ")")
	   rpel(Paste("rm(", list(h$dropdata), ")"))
    })
	
	#varbrowser with summary option
     assign("devium.varbrowser", gvarbrowser(handler = function(h, ...) 
		{
			tmp<-get("devium.varbrowser",envir=devium)
			value<- svalue(tmp)
			if(!is.null(tryCatch(gget(value),error=function(e){})))	{tmp<-gget(value);assign(get("value"),tmp)}
			tmp2<-get("devium.notebook",envir=devium)
			add(tmp2, devium.summary(value), label = Paste("Summary of ", value))
		}),envir= devium)
	
	# make group w/  movable separator to hold var browser and gui notebook
	#------------------------------
	#need to do else error...has to better way?
	tmp<-get("devium.notebook",envir=devium)
	size(tmp)<-c(rightWidth*0.67, mainHeight)
    assign("devium.notebook",tmp,envir=devium)
	
	pg = gpanedgroup(container=bottomGroup,expand = TRUE)
	add(pg,get("devium.varbrowser", envir=devium))
	add(pg,get("devium.notebook",envir=devium))
	assign("devium.bottom.panedgroup",pg,envir=devium)
	
	#add bottom most status bar
	add(mainGroup, get("devium.statusBar",envir=devium)) #status

	#data frame viewer
    x = as.numeric(NA)
    df = data.frame(X1 = x)
    assign("devium.data.frame.viewer.nb",gdfnotebook(tab.pos = 1, dontCloseThese = 1),envir= devium)
	
	#initialize notebook object 
    add(get("devium.notebook",envir=devium), get("devium.data.frame.viewer.nb",envir=devium), label = "Data", pageno = 2, override.closebutton = TRUE)
		
	# add about info as notebook pane	
	devium.about<-function (container = NULL) 
		{
			group = ggroup(horizontal = FALSE, container = container)
			size(group) <- c(500, 500)
			theFactsMam <-list() 
			theFactsMam$Author<-"Dmitry Grapov"
			theFactsMam$Version<-"0.01"
			theFactsMam$Title<-"Data Play Land"
			theFactsMam$URL<-"www.imdevsoftware.wordpress.com"
			theFactsMam$Description<-"A GUI for dynamic data exploration and visualization"
			# theFactsMam$License<-"GPL (>= 2)"
			theFactsMam<-do.call("cbind",theFactsMam)
			glabel(Paste("<b>D E V I U M - </b>\n","<b>D</b>ynamic multivariat<b>E</b> data analysis and <b>VI</b>s<b>U</b>alization platfor<b>M</b>\n", "<i>", theFactsMam[1, "Title"], 
				"</i>\n", "Version ", theFactsMam[1, "Version"], "\n\n", 
				theFactsMam[1, "URL"], "\n", "Comments to dgrapov@gmail.com\n", 
				"\n\n", theFactsMam[1, "Author"], "\n\n", theFactsMam[1, 
					"Description"], "\n"), markup = TRUE, container = group)
			addSpring(group, 10)
			return(group)
		}
	
    add(get("devium.notebook",envir=devium),devium.about(), label = "About D E V I U M")
	
	#show form
	tmp<-get("devium.main.window",envir=.GlobalEnv)
    visible(tmp) <- TRUE
	}
