#get information from table based on row and col lookup
extract.DB<-function(id, from="PubChem CID",DB=NULL,object=""){
	#extract rows and columns from DB
	if(is.null("IDB")){ stop("Please supply a data base for 'DB'")}

	#Extract DB rows based on supplied id match to column ="from"
	col<-agrep(tolower(from),tolower(colnames(DB)))[1] # fuzzy matching probably a bad idea
	return.col<-agrep(tolower(object),tolower(colnames(DB)))
	tmp<-DB[,return.col,drop=F]
	key<-fixlc(DB[,col])
	key[key==""]<-"DBempty"
	rownames(tmp)<-make.unique(key)
	#prepare ID, recode duplicates
	id<-make.unique(id)
	
	#return
	return(tmp[id,,drop=FALSE])
}

# # translate index based on lookup table  
translate.index<-function(id, lookup){
	# lookup is a two column data.frame or matrix with 
	# column 1 containing index matching id and
	# columns >=2 containing translation(s)
	id<-as.matrix(id)# needs 2 dims
	lookup<-do.call("cbind",lapply(lookup,fixlc)) # as.matrix adds empty space to numbers
	#need to make sure values can be rownames	
	#remove duplicates
	keep<-!duplicated(lookup[,1])
	tmp.data<-lookup[keep,-1,drop=FALSE]
	rownames(tmp.data)<-fixlc(lookup[keep,1])
	trans<-do.call("cbind",lapply(1:ncol(id),function(i){
		tmp.data[fixlc(id[,i]),] # needs to be a character to get correct rownames
		}))
	error<-tryCatch(colnames(trans)<-colnames(id),error=function(e){NULL}) # relic to keep source/target names
	if(is.null(error)){colnames(trans)<-paste(rep(colnames(id),each=ncol(lookup)-1),colnames(trans),sep="_")}
	return(trans)	
}

#get InchI Key based reaction pairs
get.inchikey.RPAIRS<-function(type="main",url="https://gist.github.com/dgrapov/5674494/raw/9faff56b5f0fe89b554a508bd954605e26b492fc/InchI+Key+Reaction+Pairs"){ 
  #more types should be added based on third column levels
  if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
  text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
  tmp<-strsplit(text,"\\n")
  tmp2<-strsplit(as.character(unlist(tmp)), "\t")
  #fix header errors
  tmp2[[1]]<-strsplit(tmp2[[1]],"\\  ")
  full<-out<-matrix(unlist(tmp2),ncol=4, byrow=TRUE)
  
  if(type =="main"){
    out<-full[full[,3]=="main",1:2]
  } 
  
  if(type =="all"){
    out<-full[,1:2]
  }
  
  if(type =="full"){
    out<-full
  }	
  return(out)
}

# get network edge list and layout from KEGG KGML file
kgml.network<-function(file){
  if(!require("XML")){install.packages("XML");library("XML")} else {library("XML")}
  #file should be a kegg KGML file
  fileName<-file
  kgml<-xmlTreeParse(fileName) #,useInternal = TRUE
  main<-kgml$doc$children$pathway
  main<-xmlParse(fileName)
  
  #get node layout
  #----------------------
  node.id<-getNodeSet(main,"//entry/@id")
  node.name<-getNodeSet(main,"//graphics/@name")
  xpos<- getNodeSet(main, "//@x")
  ypos<-getNodeSet(main, "//@y")
  
  map.layout<-as.matrix(data.frame(id=unlist(node.id), name=unlist(node.name),x=unlist(xpos), y=unlist(ypos)))
  
  #get edge list
  #------------------------
  #not easy to account for XML entries containing multiple edges
  # so hack it for now
  
  main<-kgml$doc$children$pathway
  nodes<-main[names(main)%in%"entry"]
  reactions<-main[names(main)%in%"reaction"]
  reaction.list<-do.call("rbind",lapply(1:length(reactions),function(i)
  {
    x<-reactions[[i]]
    reaction.name<-gsub("\"","",as.character(strsplit(as.character(strsplit(as.character(x)," ")[[2]]),',')[[2]]))
    sub<- t(unlist(x[names(x)%in%"substrate"]))
    s.ids<- sub[,colnames(sub)%in%"substrate.attributes.id"]
    s.names<-  sub[,colnames(sub)%in%"substrate.attributes.name"]
    prod <- t(unlist(x[names(x)%in%"product"])) 
    p.ids<- prod[,colnames(prod)%in%"product.attributes.id"]
    p.names<- prod[,colnames(prod)%in%"product.attributes.name"]
    data.frame(substrate.id=s.ids,product.id=p.ids,substrate.name=gsub("cpd:","",s.names),product.name=gsub("cpd:","",p.names),reaction.id=gsub("rn:","",reaction.name))  	
  }))
  
  #edge list
  edge.list<-data.frame(source=reaction.list$substrate.name,target=reaction.list$product.name)
  
  #return results
  return(list(edge.list=edge.list,layout=data.frame(map.layout)))
  
}

#create a network from a graphNeL object
make.cynet<-function(graph,network.name,layout='jgraph-spring'){
		check.get.packages("RCytoscape")
		
		#check connection 
		con<-tryCatch(CytoscapeConnection (),error=function(e){NULL})
		if(!class(con)=="CytoscapeConnectionClass")
			{
				return(cat("No connection to Cytoscape \n"))
			} else {
				
				#check if cytoscape is open and a connection can be made
				cw <- tryCatch(new.CytoscapeWindow (network.name, graph=graph,overwriteWindow=TRUE),
					error=function(e){"couldn't connect to host"})
				if(class(cw)=="character")
					{
						return(cw)
					} else {	
						displayGraph (cw)
						layoutNetwork(cw, layout.name=layout)
						redraw(cw)
						cw
					}
			}
	}

#set characteristics of edges in an existing cytoscape network
set.edge.attribute<-function(network,edge.names,edge.attribute,edge.attribute.value){
	
		var<-edge.attribute.value
		
		#need to be set one at a time, one level at a time...
		switch(edge.attribute,
			
				color 	= 	sapply(1:nlevels(as.factor(var)),function(i)
							{
								id<-c(1:length(var))[var==levels(as.factor(var))[i]]
								setEdgeColorDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
							}),
						
				opacity = 	sapply(1:nlevels(as.factor(var)),function(i)
								{
									id<-c(1:length(var))[var==levels(as.factor(var))[i]]
									setEdgeOpacityDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
								}),
						
				width 	= 	sapply(1:nlevels(as.factor(var)),function(i)
								{
									id<-c(1:length(var))[var==levels(as.factor(var))[i]]
									setEdgeLineWidthDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
								})
				)		
		redraw (network)
	}

#set characteristics of nodes in existing cytoscape network
set.node.attribute<-function(network,node.names,node.attribute,node.attribute.value){
		#check node names against what is in the network 
		var<-node.attribute.value
		graph.names<-getAllNodes(network)
		union<-seq(along=node.names)[node.names%in%graph.names]
		
		#append what will be set based on the union
		node.names<-node.names[union]
		var<-node.attribute.value[union]
		
		#need to be set one at a time, one level at a time...
		switch(node.attribute,
			
					color 		= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeColorDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
					size		= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeSizeDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),				
						
					opacity 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
								
					border 		=	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
								
					shape 		=	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeShapeDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.width 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderWidthDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.color 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderColorDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.opacity 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									})					
				)		
		redraw (network)
	}

#dynamically link graph node and edge selections	
link.cyto.graph.nodes.select<-function(graphs,visual.style.names=NULL){
		#graphs should be a character string of graph names
		#with out knowing the active graph scan all for active nodes
		selected<-unique(as.character(na.omit(do.call("rbind",lapply(1:length(graphs),function(i)
			{
				assign("tmp",get(graphs[i]))
				getSelectedNodes(tmp)
			})))))
		
		if(!length(selected)==0)
			{	
			#select in all graphs use 
			#for some reason this looses the visual style of the graph in the procces
			sapply(1:(length(graphs)),function(i)
				{
					
					
					assign("tmp",get(graphs[i]))
					selectNodes(tmp,selected)
					#reset style
					if(!class(visual.style.names)=="NULL")
						{
							setVisualStyle (get(graphs[i]), visual.style.names[i]) 
						}
				})
			}
	}

#save cytoscape network styles 
save.cyto.graph.styles<-function(network,prefix=NA){
		sapply(1:length(network),function(i)
		{
			if(is.na(prefix))name<-paste("style-",network[i],sep="") else name<-paste(prefix,network[i],sep="") 
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#delete styles (not validated)	
delete.cyto.graph.styles<-function(network){
		sapply(1:length(network),function(i)
		{
			name<-paste(network[i],"-style",sep="")
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#create a node legend network
cyto.node.legend<-function(network="new",node.attribute,node.attribute.value,node.names,legend.title="node legend",unique.matched=FALSE){
		if(!class(network)=="CytoscapeWindowClass")	
			{
				#get unique properties
				if(unique.matched==FALSE)
					{
						id<-unique.id(node.attribute.value)
						node.attribute.value=node.attribute.value[id]
						node.names=node.names[id]
					}
					
				#create new legend
				tmp<- new("graphNEL", edgemode = "undirected")
				i<-1
				for(i in 1:length(node.names))
					{	
						tmp<-graph::addNode(node.names[i], tmp)
					}
				#draw nodes			
				cw <- CytoscapeWindow(legend.title, graph = tmp)
				displayGraph(cw)	
				network<-cw
			}
			
			if(class(network)=="CytoscapeWindowClass")
			{
				#get unique properties
				if(unique.matched==FALSE)
					{
						id<-unique.id(node.attribute.value)
						node.attribute.value=node.attribute.value[id]
						node.names=node.names[id]
					}
					
				#check to see if node exists else create new
				nodes<-getAllNodes (network)
				new.nodes<-node.names[!node.names%in%nodes]
				
				#create new nodes
				if(!length(new.nodes)==0)
				{
					i<-1
					for(i in 1:length(new.nodes))
						{	
							addCyNode(network, new.nodes[i])
						}
				}
			}
			
		#annotate node properties
				set.node.attribute(network=network,node.names=node.names,
						node.attribute=node.attribute,
						node.attribute.value=node.attribute.value)	
		
		#layoutNetwork(network, 'grid')
		redraw(network)
		return(network)
					
	}
	
#format cytoscape edge names to edge list
convert.to.edge.list<-function(graph){	
		return(do.call("rbind",strsplit(as.character(names(cy2.edge.names (graph@graph) )),"~")))
	}

#convert  qpgraph output to edge list output
mat.to.edge.list<-function(input,graph){
		#parse qpgraph object into pieces
		# generalize later if necessary
		p.cor.r<-input$R
		p.cor.p<-input$P
		
		#initialize objects
		idn<-rownames(p.cor.r)
		ids<-seq(along=idn)
		
		#common network edges names 
		edge.names<-convert.to.edge.list(graph)
		edge.ids<-do.call("rbind",lapply(1:nrow(edge.names),function(i)
				{
					data.frame(columns=ids[idn%in%edge.names[i,1]],
					rows=ids[idn%in%edge.names[i,2]])
				}))
				
		#Extract edge correlation and p-value
	
		edge.cor<-do.call("rbind",lapply(1:nrow(edge.ids),function(i)
			{
				tmp<-data.frame(correlation=p.cor.r[edge.ids[i,1],edge.ids[i,2]],
				p.value=p.cor.p[edge.ids[i,1],edge.ids[i,2]])
				rownames(tmp)<-cy.edge.names[i]
				tmp
			}))
			
		#sort rows to later match cy2.edge.names sort
		edge.cor<-cbind(edge.cor,edge.ids)
		edge.cor<-edge.cor[order(rownames(edge.cor),decreasing=T),]
		
		return(edge.cor)
	}

gen.mat.to.edge.list<-function(mat,symmetric=TRUE,diagonal=FALSE,text=FALSE){
	#create edge list from matrix
	# if symmetric duplicates are removed
	check.get.packages("reshape2")
	mat<-as.matrix(mat)
	id<-is.na(mat) # used to allow missing
	mat[id]<-"nna"
	if(symmetric){mat[lower.tri(mat)]<-"na"} # use to allow missing values
	if(!diagonal){diag(mat)<-"na"}
	obj<-melt(mat)
	colnames(obj)<-c("source","target","value")
	obj<-obj[!obj$value=="na",]
	obj$value[obj$value=="nna"]<-NA
	if(!text){obj$value<-as.numeric(as.character(obj$value))}
	return(obj)
	# remove duplicates
}	
	
#trim edge list based on some reference index 
edge.list.trim<-function(edge.list,index,cut,less.than=FALSE){
			if(less.than==TRUE){
					edge.list[index<=cut,,drop=FALSE]
				}else{
					edge.list[index>=cut,,drop=FALSE]
					}
		}

#----Identify differences in measures between two classes given some 
# threshhold (i.e. p-value) and filter out put based on pcor, combined network
# inputs structure as output from qpgraph
compare.2class.network<-function(network,class1.obj,class2.obj, threshold=0.05){
		#Extract edge correlation and p-value
		#class 1
		edge.cor1<-mat.to.edge.list(class1.obj,network)
			
		#class 2
		edge.cor2<-mat.to.edge.list(class2.obj,network)
			
		#encode id based on cor p <= cut.off
		p.cut.off<-threshold

		#pre sig extraction object	use to hide
		sig.id1<-edge.cor1[,2]<=p.cut.off
		sig.id2<-edge.cor2[,2]<=p.cut.off
		
		#---------------------FINAL ids
		#these can be used to mask redundant information
		#edges common to both classes
		tmp<-seq(1:nrow(edge.cor1))
		not.sig<-!sig.id1==FALSE&sig.id1==sig.id2

		#unique class 1 edges
		class1.sig<-!sig.id1==FALSE&!sig.id1==not.sig

		#unique class 2 edges
		class2.sig<-!sig.id2==FALSE&!sig.id2==not.sig
		
		#output results
		return(list(common.edges=not.sig,data.frame(edge.cor1,meets.threshold=class1.sig),data.frame(edge.cor2,meets.threshold=class2.sig)))
	}

#subset data and compare common and unique connections based on qpnetworks	
qpgraph.compare<-function(data,factor,threshold="auto",...){
		#Use factor to split data by rows  and calculate
		#qpnetworks
		#if threshhold ="auto" then it is estimated where all nodes are connected
		#otherwise could be numeric or "interactive"
		
		out.lists<-lapply(1:nlevels(factor),function(i)
			{
				#data subset
				tmp.data<-data[factor==levels(factor)[i],]
				
				cat("Calculating for:",levels(factor)[i],"\n")
				#qpnetwork
				full.net<-make.ave.qpgraph(tmp.data)#,...
				#overview nodes vs edges to choose threshhold
				if(all(is.numeric(threshold)))
					{
							tmp.threshold<-threshold
					}else{
							tmp.threshold<-choose.qpgraph.threshold(full.net,choose=threshold) [[1]]
						}
						
				#edgelist based on qpgraph
				qpnet <-as.matrix(qpGraph(full.net, threshold=tmp.threshold, return.type="edge.list"))
				out<-list(qpnet)
				names(out)<-paste(levels(factor)[i])
				out
			})
			
			#for edge lists
			unique.edge<-lapply(1:length(out.lists),function(i)
						{
							tmp<-as.matrix(as.data.frame(out.lists[[i]]))
							enames<-paste(tmp[,1],tmp[,2],sep="__")
							search.id<-c(1:length(out.lists))[!c(1:length(out.lists))==i]
							is.common<-sapply(1:length(search.id),function(j)
								{
									tmp2<-as.matrix(as.data.frame(out.lists[[search.id[j]]]))
									enames2<-paste(tmp2[,1],tmp2[,2],sep="__")
									enames%in%enames2
								})					
							obj<-list(data.frame(from=tmp[,1],to=tmp[,2],is.common=is.common))
							names(obj)<-names(out.lists[[i]])
							obj
						})
						
			#get common and unique edges (improve this long winded version later)	
			tmp<-as.data.frame(unique.edge[1])
			common<-tmp[tmp[,3],1:2]		
			unique.output<-lapply(1:length(unique.edge),function(i)
				{
					tmp<-as.data.frame(unique.edge[i])
					tmp[!tmp[,3],1:2]
				})
			
			list(common=common,unique.output)
			}
		
#generate qpgraph
make.ave.qpgraph<-function(data,tests=200,for.col=TRUE,...){
		check.get.packages("qpgraph")
		.local<-function(data,tests=200,for.col=TRUE,...)
		{
			m<-data # some bug in qpgraph
			average.net<-qpAvgNrr(m,nTests=tests,...) 
			return(average.net)
		}
		if(dim(data)[2]<=dim(data)[1]&for.col==TRUE)long.dim.are.variables<-FALSE else long.dim.are.variables<-TRUE
		.local(data,long.dim.are.variables=long.dim.are.variables,tests=tests,...)
	}
	
#plot graph edge/vertex number vs threshhold
choose.qpgraph.threshold<-function(qpnetwork,.threshold=c(0,.6),choose=NULL){
		
		#choose = c("interactive", "auto")
		
		#choose elbow in vertex vs. edge plot
		int<-c(seq(.threshold[1],.threshold[2],by=.01)[-1])
		
		xy<-do.call("rbind",lapply(1:length(int),function(i)
				{
					net <- qpGraph(qpnetwork, threshold=int[i], return.type="graphNEL")
					con.nodes<-tryCatch(net@nodes, error= function(e) {NUL})
					# con.nodes<- tryCatch(unique(unlist(strsplit(names(net@edgeData@data),"\\|"))), error=function(e) {NULL}) # when nothing is connected
					data.frame(threshold=int[i],nodes=length(con.nodes),edges=length(unlist(net@edgeL))/2)
				}))
		#plot
		plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
		lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
		legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
		#show threshold wher all nodes are connected
		tmp<-which.min(abs(nrow(qpnetwork)-xy[,2]))
		abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
		abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
		abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
		title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
		if(choose=="auto") 
			{
				return(list(auto.threshold=xy[tmp,1],list=xy))
			}
			
		if(choose=="interactive")
			{
				tmp<-locator(1)$y
				tmp<-which.min(abs(xy[,1]-tmp))
				plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				#show threshold wher all nodes are connected
				abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
				return(xy[tmp,1])
			}
	}

#partial correlation coefficients and p-values
#some times this can not be calculated due too few observations
#do instead via qpgraph
partial.correl<- function(data, qpnet, verbose=T, for.col=TRUE,...){
		.local<-function(data, qpnet, verbose=verbose,long.dim.are.variables,...)
			{
				qpPAC(data, g=qpnet, verbose=verbose,long.dim.are.variables=long.dim.are.variables,...) 
			}
		if(dim(data)[2]<=dim(data)[1]&for.col==TRUE)long.dim.are.variables<-FALSE else long.dim.are.variables<-TRUE
		.local(data, qpnet=qpnet,verbose=verbose, long.dim.are.variables=long.dim.are.variables,...)
	}

#make graphNEL object from edge list
edge.list.to.graphNEL<-function(edge.list){
		check.get.packages("graph")
		#one way
		edge.l<-split(as.character(edge.list[,2]),as.factor(as.character(edge.list[,1])))
		#reciprocal
		edge.l.r<-split(as.character(edge.list[,1]),as.factor(as.character(edge.list[,2])))
		#bind and break one more tiome to merge duplicated edges
		edge.l.final<-split(c(edge.l,edge.l.r),as.factor(names(c(edge.l,edge.l.r))))
		
		#format to numeric index for names
		tmp.names<-names(edge.l.final)
		out<-sapply(1:length(edge.l.final),function(i)
			{
				tmp<-edge.l.final[[i]]
				if(length(tmp)>1)
					{
						name<-unique(names(tmp))
						tmp<-list(as.character(unlist(tmp)))
						names(tmp)<-name
					}
				index<-tmp.names[tmp.names%in%as.character(unlist(tmp))]
				obj<-list(edges=index)
				names(obj)<-names(tmp)
				obj
			})
		
		obj<-new("graphNEL", nodes=names(out), edgeL=out,edgemode="undirected")
		return(obj)
	}
	
#extract values from a square symmetric matrix based on an edge list
sym.mat.to.edge.list<-function(mat,edge.list){
		# extract position of objects from a
		# square symmetric matrix based on its dimnames
		# according to an edge list
		
		#initialize objects
		idn<-rownames(mat)
		ids<-seq(along=idn)
		
		#common network edges names 
		edge.names<-edge.list
		edge.ids<-do.call("rbind",lapply(1:nrow(edge.names),function(i)
				{
					data.frame(columns=ids[idn%in%edge.names[i,1]],
					rows=ids[idn%in%edge.names[i,2]])
				}))
				
		#Extract value from mat based on index
		edge.val<-do.call("rbind",lapply(1:nrow(edge.ids),function(i)
			{
				tmp<-data.frame(value=mat[edge.ids[i,1],edge.ids[i,2]])
				rownames(tmp)<-paste(edge.names[i,1],edge.names[i,2],sep="~")
				tmp
			}))
		return(edge.val)
	}

#sort edge list/attributes to match order of cytoscape network
#account for reciprical edge naming 
match.cynet.edge.order<-function(obj,cynet){
		cy.order<-names(cy2.edge.names (cynet@graph))
		my.order<-rownames(obj)
		
		#split object, look for non matches and flip this assignment 
		tmp.cy<-do.call("rbind",strsplit(cy.order,"~"))
		tmp.my<-do.call("rbind",strsplit(my.order,"~"))
		
		#identify where there are no matches
		flip<-!my.order%in%cy.order
		
		#flip assignment in tmp.my
		tmp<-tmp.my[flip,1]
		tmp.my[flip,1]<-tmp.my[flip,2]
		tmp.my[flip,2]<-tmp
		
		#now bind and make sure are identical
		my.order<-paste(tmp.my[,1],tmp.my[,2],sep="~")
		if(!identical(sort(my.order),sort(cy.order))){cat("Edges do not match:","\n",my.order[!my.order%in%cy.order]);stop()}
		
		#get index for my.oder to match cy.order
		ord1<-seq(along=cy.order)[order(cy.order)]
		ord2<-seq(along=my.order)[order(my.order)]
		final<-ord2[ord1]
		return(final)
	}	

#filter edges based on some weight or binary index
filter.edges<-function(edge.list,filter,cut.off=NULL){
		#check to see if each side of the edge (to or from) meets requirement
		#if cut.off = NULL 
		#filter must be a two column matrix with node names and logical statement if they should kept
		#else the filter can be a square symmetric matrix with nodes as dimnames whose values will be used 
		#to select edges to keep if the value is <= cutoff
		#returns an index of edges matching criteria
		if(class(cut.off)=="NULL")
			{
				out<-sapply(1:nrow(edge.list),function(i)
					{
						any(as.character(unlist(edge.list[i,]))%in%as.character(filter[filter[,2]==TRUE,1]))
					})
			}
			
		if(class(cut.off)=="numeric")
			{
				#get weights for edges
				tmp<-sym.mat.to.edge.list(filter,edge.list)
				out<-sapply(1:nrow(edge.list),function(i)
					{
						tmp[i,]<=cut.off
					})
			}
		return(out)
	}
	
#limit to X top edges per node( not correct see edge.list.filter2) 
edge.list.filter<-function(edge.list,value, max.edges=10, separate=TRUE, decreasing=TRUE){
	# edge list a two column data frame defining connections
	# value vector of values to select from 
	# max.edges maximum number of allowed edges
	# separate  should positive and negative values be tested together
	# top select top edges values based on magnitude of value
	# result is a row index for edges meeting criteria
	
	nodes<-unique(matrix(unlist(edge.list), ncol=1))
	id<-c(1:nrow(edge.list))
	
	if(separate){
		tmp<-split(as.data.frame(cbind(edge.list, value)), as.factor(value>0))
		#max.edges<-floor(max.edges/2) # allow equal influence of both positive an negative edges
		
		out<-lapply(1:length(tmp), function(j){
		
				edge.list<-tmp[[j]][,-3]
				value<-tmp[[j]][,3]
				sapply(1:length(nodes), function(i){
					index<-id[edge.list[,1]%in%nodes[i]|edge.list[,2]%in%nodes[i]]
					values<-value[index]
					vals<-na.omit(index[order(values, decreasing=decreasing)][1:max.edges])
					if(length(vals)==0){vals<-max(id)+1 } # dummy index to avoid empty
					vals
				})
		})
		
		#combine separated results 
		tmp<-join.columns(data.frame(do.call("rbind",out[[1]]),do.call("rbind",out[[2]])),",")
		tmp2<-sapply(1:length(tmp), function(i){
				as.numeric(unique(unlist(strsplit(tmp[i],","))))
			})
		edge.id<-unique(unlist(tmp2))
		
		
	} else {
	
		out<-sapply(1:length(nodes), function(i){
			index<-id[edge.list[,1]%in%nodes[i]|edge.list[,2]%in%nodes[i]]
			values<-value[index]
			vals<-na.omit(index[order(values, decreasing=decreasing)][1:max.edges])
			if(length(vals)==0){vals<-max(id)+1 } # dummy index to avoid empty
			vals
		})
		
		edge.id<-unique(unlist(out))
	}
	return(edge.id)
}

#limit to X top edges per node (leave only the strongest pairwise connections disconnected from all others)
edge.list.filter.full<-function(edge.list,weight,max.edges=1){

	tmp<-data.frame(rbind(edge.list,edge.list[,2:1]),weight=c(weight,weight))
	colnames(tmp)<-c("source","target","weight")
	mat<-dcast(tmp,source ~ target,mean,value.var="weight") # make adjacency matrix should be better fxn for this
	mat[,1]<-as.factor(mat[,1])
	
	# get top edges (need to run twice to get row and column top ids)
	len<-1:nrow(mat)
	tmp.mat<-as.matrix(mat[,-1])
	keep.mat<-do.call("rbind",lapply(1:(nrow(tmp.mat)),function(i) {
		id<-len[order(tmp.mat[i,],decreasing=TRUE)][1:max.edges]
		tmp.mat[i,id]<-"keep"
		tmp.mat[i,]
	}))
	keep.mat1<-do.call("cbind",lapply(1:(ncol(tmp.mat)),function(i) {
		id<-len[order(tmp.mat[,i],decreasing=TRUE)][1:max.edges]
		tmp.mat[id,i]<-"keep"
		tmp.mat[,i]
	}))
	
	dimnames(keep.mat)<-dimnames(keep.mat1)<-list(mat[,1],colnames(mat)[-1])
	#merge objects to make selection
	kept<-melt(keep.mat)
	kept<-kept[!is.na(kept[,3]),]
	kept1<-melt(keep.mat1)
	kept1<-kept1[!is.na(kept1[,3]),]
	# kept1<-kept1[order(kept1[,1]),]
	
	kept<-kept[kept[,3]=="keep"&kept1[,3]=="keep",]
	
	#need to map back to original edge list order
	len<-1:nrow(edge.list)
	back.order<-sapply(1:nrow(kept),function(i){
		pos1<-len[as.character(edge.list[,1])%in%fixlc(kept[i,1])&as.character(edge.list[,2])%in%fixlc(kept[i,2])]
		pos2<-len[as.character(edge.list[,2])%in%fixlc(kept[i,1])&as.character(edge.list[,1])%in%fixlc(kept[i,2])]
		unique(c(pos1,pos2))
		})
	#row index for kept terms
	unique(unlist(back.order))
}

#allow more than max.nodes to connect otherwise disconnected nodes with strongest relationship
edge.list.filter.partial<-function(edge.list,weight,max.edges=1){
	nodes<-unique(fixlc(edge.list))
	id<-fixlc(edge.list[,2])%in%nodes
	#flip source target (expecting undirected edges) to get all of one index on one side
	#test what happens when source nodes are connected
	tmp<-as.data.frame(edge.list)
	#add index
	tmp$tmp.id<-c(1:nrow(tmp))
	tmp$tmp.weight<-weight
	filter<-lapply(1:length(nodes),function(i){
		id<-tmp[,1]%in%nodes[i] | tmp[,2]%in%nodes[i]
		obj<-tmp[id,]
		obj<-obj[order(obj[,3],decreasing=TRUE),]
		obj[c(1:nrow(obj))<=max.edges,]
	})
	unique(do.call("rbind",filter)[,3])
}

#create edge list and network attributes file from meta.data with CID keys
#data<- bound with CIDS
#remove duplicates from object based on and index/identifier 
unique.obj<-function(data, index){
		#accesory function to return position of first instance of unique object 
		id<-unique.id(index)
		data[id,]
	}

#look up KEGG reactant pairs 
get.KEGG.pairs<-function(type="main",url="https://gist.githubusercontent.com/dgrapov/5548641/raw/6b297fe117b412d733cd217316eb5a3c55b5fb2c/KEGG+RPairs"){ 
		#older repo: "https://gist.github.com/dgrapov/4964564/raw/aec1a5097a3265d22109c9b34edd99a28f4012a3/KEGG+reaction+pairs"
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		tmp2[[1]]<-strsplit(tmp2[[1]],"\\  ")
		full<-out<-matrix(unlist(tmp2),ncol=4, byrow=TRUE)
		
		if(type =="main"){
				out<-full[agrep("main",full[,3]),1:2] # now will get all main types
			} 
			
		if(type =="all"){
				out<-full[,1:2]
			}
			
		if(type =="full"){
				out<-full
			}	
			return(out)
	}

#generic fxn (of get.KEGG.pairs) to get data from GIST which is tab separated (no spaces)
get.GIST.csv<-function(url="https://gist.githubusercontent.com/dgrapov/c079db6f3a31f7fa478f/raw/5f421d716708994e9986d89323d956007398e753/oxylipins%20edge%20list"){ 

	if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
	text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
	tmp<-strsplit(text,"\\n")
	tmp2<-strsplit(as.character(unlist(tmp)), "\t")
	c.names<-tmp2[[1]]# expect headers for every column
	out<-do.call("rbind",tmp2)
	out<-as.matrix(out[-1,]) # expect header
	colnames(out)<-c.names
	return(out)
	
}
	
#look up CID to KEGG translation this function is replaced with the more general get.Reaction.pairs
get.CID.KEGG.pairs<-function(url="https://gist.github.com/dgrapov/4964546/raw/c84f8f209f961b23adbf7d7bd1f704ce7a1166ed/CID_KEGG+pairs"){
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		matrix(unlist(tmp2),ncol=2, byrow=TRUE)
	}

#convert form pubchem CID to KEGG using web services
convert.CID.to.KEGG<-function(query){

	#this can be made parallel
	# for now in series
	sapply(1:length(query),function(i,pb = txtProgressBar(min = 0, max = length(query), style = 3))
		{
			setTxtProgressBar(pb, i)
			url<-paste("http://biodb.jp/hfs_cco.cgi?type=CCO_C_ID&id=",query[i],"&db=KEGGCOMPOUND&lang=en&tax=cco",sep="")
			tmp<-readLines(url)
			if(length(tmp)>50){  # hack to catch no result returned
				kegg.id<-"not found"
			} else {
				kegg.url<-unlist(strsplit(strsplit(tmp,"url=")[[9]][2],"\"; target=\"_top\">"))
				kegg.id<-unlist(strsplit(kegg.url,"cpd:")[[1]][2])
			}
			kegg.id
		})
}

#getting connections from an edge list based on an index, which is translated
get.Reaction.pairs<-function(index,reaction.DB,index.translation.DB,translate=TRUE, parallel=FALSE){
		
		#index identifies analytes to query connection for
		#reaction.DB is a an edge list for connections
		#index.translation DB is a 2 column table to translate index (column 1) to reaction.DB index (column 2)
		
		if(translate==TRUE){
			#translate input index to reaction.DB index
			matched<-index.translation.DB[index.translation.DB[,1]%in%index,] # c(1:nrow(index.translation.DB))[
			#check if something could not be matched
			unmatched<-index[which(!index%in%matched[,1])]
			if(length(unmatched)>0){cat(paste("The following were not found in the index.translation.DB:",unmatched,"\n"))}
			#check and remove pairs (due to duplicate KEGG id for differing InchIkeys)
			dupes<-duplicated(apply(matched,1,paste,collapse="|"))| duplicated(apply(matched[,2:1],1,paste,collapse="|"))
			matched<-matched[!dupes,]
		} else { 
				tmp<-data.frame(unique(index))
				index.translation.DB<-matched<-as.matrix(data.frame(tmp,tmp))
				}
		
		if(parallel==TRUE){ # add progress bar
				cat("Setting up cluster...","\n")
				library("snow");library("doSNOW");library("foreach")
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				
				
				#do work
				cat("Conducting translations...","\n")
				ids<-foreach(i=c(1:nrow(matched))) %dopar% c(which(as.character(matched[i,2])==as.character(reaction.DB[,1])),which(as.character(matched[i,2])==as.character(reaction.DB[,2])))				
				
				names(ids)<-matched[,1]	# index of all paired by index
				# remove unpaired objects
				empty<-sapply(ids,length)
				search<-c(1:length(ids))[empty>0]
				cat("Matching reaction table...","\n")
				#construct symmetric matrix then extract unique edge list do.call("rbind"
				mat<-do.call("rbind",foreach(i=c(1:length(search))) %dopar% sapply(c(1:length(search)), function(j){ sum(ids[[search[j]]]%in%ids[[search[i]]])}))
					 
				#stop parallel
				stopCluster(cl.tmp)	
			} else {
				
				cat("Conducting translations...","\n")
				ids<-sapply(1:nrow(matched),function(i)
					{
						#
						c(which(as.character(matched[i,2])==as.character(reaction.DB[,1])),which(as.character(matched[i,2])==as.character(reaction.DB[,2])))				
					})
					
				names(ids)<-matched[,1]	# index of all paired by index
				
				# remove unpaired objects
				empty<-sapply(ids,length)
				search<-c(1:length(ids))[empty>0]
				
				cat("Matching reaction table...","\n")
				#construct symmetric matrix then extract unique edge list
				mat<-do.call("rbind",lapply(c(1:length(search)),function(i)
					{
						sapply(c(1:length(search)), function(j)
							{
								sum(ids[[search[j]]]%in%ids[[search[i]]])
							})
					}))
			}
			
		dimnames(mat)<-list(names(ids)[search],names(ids)[search])
		#need to make symmetric for edgelist extraction using top triangle
		
		cat("Converting to an edge list...","\n")
		#add top and bottom triangles 
		mat<-mat+t(mat)
		elist<-gen.mat.to.edge.list(mat)
		as.data.frame(elist[fixln(elist[,3])>0,1:2])	#index source to 
	}

#simpler and more generic version of get.Reaction.pairs
get.Reaction.pairs2<-function(id,reaction.DB,source="sourceCID",target="targetCID"){
	#match on KEGG or better CID
	# get connections from custom curated edge list
	
	#clean up
	id<-gsub(" ","",fixlc(oxy$CID))
	id[is.na(id)]<-"error666"
	
	
	tmp<-data.frame(unique(id))
	index.translation.DB<-matched<-as.matrix(data.frame(tmp,tmp))

	ids<-sapply(1:nrow(matched),function(i)
			{
					#
					c(which(as.character(matched[i,2])==as.character(reaction.DB[,source])),which(as.character(matched[i,2])==as.character(reaction.DB[,target])))              
			})		
			
	names(ids)<-matched[,1] # index of all paired by index

	# remove unpaired objects
	empty<-sapply(ids,length)
	search<-c(1:length(ids))[empty>0]

	
	#construct symmetric matrix then extract unique edge list
	mat<-do.call("rbind",lapply(c(1:length(search)),function(i)
			{
					sapply(c(1:length(search)), function(j)
							{
									sum(ids[[search[j]]]%in%ids[[search[i]]])
							})
			}))

	dimnames(mat)<-list(names(ids)[search],names(ids)[search])
	#need to make symmetric for edgelist extraction using top triangle

	#add top and bottom triangles 
	mat<-mat+t(mat)
	elist<-gen.mat.to.edge.list(mat)
	as.data.frame(elist[fixln(elist[,3])>0,1:2])    #
}

#get various Database IDs and pathway information (from IDEOM)
IDEOMgetR<-function(url="https://gist.githubusercontent.com/dgrapov/5548790/raw/399f0958306c1018a6be846f58fd076ae83f1b78/IDEOM+small+list"){
		options(warn=-1)
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		DB<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(DB,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#convert to matrix
		obj<-t(do.call("cbind",sapply(tmp2,unlist)))
		#try to fix colnames
		names<-unlist(strsplit(obj[1,],"  "))[1:ncol(obj)]
		tmp<-obj[-1,]
		colnames(tmp)<-gsub(" ",".",names)
		return(as.data.frame(tmp))
	}	

#making an edge list based on CIDs from KEGG reactant pairs
CID.to.KEGG.pairs<-function(cid,database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs()){
		
		matched<-lookup[c(1:nrow(lookup))[fixln(lookup[,1])%in%cid],]
		ids<-sapply(1:nrow(matched),function(i)
			{
				#
				c(which(as.character(matched[i,2])==as.character(database[,1])),which(as.character(matched[i,2])==as.character(database[,2])))				
			})
			
		names(ids)<-fixln(matched[,1])	# cid of all paired by cid
		
		#construct symmetric matrix then extract unique edge list
		mat<-do.call("rbind",lapply(1:length(ids),function(i)
			{
				obj<-ids[[i]]
				match<-sapply(1:length(ids), function(j)
					{
						tmp<-ids[[j]]
						sum(tmp%in%obj)
					})
			}))
		dimnames(mat)<-list(names(ids),names(ids))
		elist<-gen.mat.to.edge.list(mat)
		as.data.frame(elist[fixln(elist[,3])>0,1:2])	#cid source to cid target based on kegg pairs	
	}

#calculate correlations and p-values
devium.calculate.correlations<-function(data,type="spearman", results = "edge list"){
		check.get.packages(c("impute","WGCNA","Hmisc"))
		#data will be coerced to a matrix
		# type includes pearson (WGCA), biweight(WGCA), spearman
		switch(type,
			pearson 	= .local<-function(data){
							obj<-corAndPvalue(as.matrix(data), use = "pairwise.complete.obs", alternative = "two.sided")
							list(cor=obj$cor,p.value=obj$p)
							},		
			biweight 	= .local<-function(data){
							obj<-bicorAndPvalue(as.matrix(data),use = "pairwise.complete.obs", alternative = "two.sided")
							list(cor=obj$bicor,p.value=obj$p)
							},
			spearman    = .local<-function(data){
							obj<-rcorr(as.matrix(data),type="spearman")
							list(cor=obj$r,p.value=obj$P)
							})
			
			res<-.local(data)	
			
			#add option for matrix or edge list	out put # there is a faster way to get edge list!
			if (results == "edge list"){
				cor<-gen.mat.to.edge.list(res$cor)
				p.vals<-gen.mat.to.edge.list(res$p.value)
				fdr<-p.adjust(p.vals$value,method="BH")
				res<-data.frame(cor,p.values = p.vals[,3],fdr.p.value=fdr)
				
			}		
			
		return(res)			
	}

#test only specific correlations between x and y matrices
# and calculate FDR for only x to y comparisons
xy.correlations<-function(x,y,method="spearman",fdr.method="BH",...){
	#where x and y are data frames
	res<-do.call("rbind",lapply(1:ncol(x),function(i){
		tmp<-do.call("rbind",lapply(1:ncol(y),function(j){
			res<-cor.test(x[,i],y[,j],method=method,...)
			data.frame(source=colnames(x)[i],target=colnames(y)[j],cor=res$estimate,p.value=res$p.value)
		}))
	}))
	res$fdr.p.value<-p.adjust(res$p.value,method=fdr.method)
	return(res)
}	
	
	
# #(using ChemmineR API)get tanimoto distances from cids 
# CID.to.tanimoto<-function(cids, cut.off = .7, parallel=FALSE, return="edge list"){
	# #used cids = PUBCHEM CIDS to calculate tanimoto distances
	# need<-c("snow","doSNOW","foreach","ChemmineR") # need to use others for mac
	# for(i in 1:length(need)){check.get.packages(need[i])}
	# #get fingerprint for calcs
	# data(pubchemFPencoding)
	# # cid.objects<-na.omit(unique(as.numeric(as.character(unlist(cids))))) # need to report excluded NA
	
	# #remove and print to screen error vars
	# #removal ids
	# objc<-as.character(unlist(cids))
	# objn<-as.numeric(unlist(cids))
	# dup.id<-duplicated(objn)
	# na.id<-is.na(objn)
	# if(sum(c(dup.id,na.id))>0){
			
			# objc<-as.character(unlist(cids))
			# objn<-as.numeric(unlist(cids))
			
			# #remove duplicated
			# cat(paste("The following duplicates were removed:","\n"))
			# cat(paste(objc[dup.id]),sep="\n")
			# # remove NA
			# cat(paste("Bad inputs were removed:","\n"))
			# cat(paste(objc[na.id]),sep="\n")
			
			# cid.objects<-objn[!(na.id | dup.id)]
		# } else { cid.objects<-objn }
	# cat("Using PubChem Power User Gateway (PUG) to get molecular fingerprint(s). This may take a moment.","\n")
	
	# compounds <- getIds(cid.objects) # get sdfset
	# cid(compounds) <- sdfid(compounds)
	
	# # Convert base 64 encoded fingerprints to character vector, matrix or FPset object
	# fpset <- fp2bit(compounds, type=2)
	
	# if(parallel==TRUE)
		# {
				# #change this later
				# cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				# registerDoSNOW(cl.tmp) 
				# out<-foreach(j=c(1:length(rownames(fpset))),.combine="cbind") %dopar% ChemmineR::fpSim(fpset[j,], fpset)#length(codes)
				# stopCluster(cl.tmp)	
		# } else {
					# out<-sapply(rownames(fpset), function(x) ChemmineR::fpSim(x=fpset[x,], fpset,sorted=FALSE)) 
		# }
		
	# #edgelist
	# #colnames(out)<-unlist(dimnames(out)[1])1
	
	# #optionally filter based on score based on score
	# obj<-as.matrix(out)
	
	# if(return=="edge list"){
		# e.list<-gen.mat.to.edge.list(obj)
		# final<-edge.list.trim(e.list,index=fixln(e.list[,3]),cut=cut.off,less.than=FALSE)
	# }else{
		# obj[obj<cut.off]<-0
		# final<-obj
	# }
	# return(final)
# }

#get metabolite structure encoding (SDF) from local database or using PubChem webservices
get.SDF.from.CID<-function(cids,DB=NULL,query.limit=25,update.DB=TRUE,save.as="CID.SDF.DB",progress=TRUE,...){
	
	#retrieve metabolite SDF from DB
	#DB should be a list with cids as names
	#for all missing in DB, look up using PubChem PUG
	#if update then update DB with cid entries
	#return list of SDF files for each cid
	
	#remove and print to screen error cids
	#removal ids
	objc<-as.character(unlist(cids))
	objn<-as.numeric(objc)
	dup.id<-duplicated(objn)
	na.id<-is.na(objn)
	if(sum(c(dup.id,na.id))>0){
			
			objc<-as.character(unlist(cids))
			objn<-as.numeric(unlist(cids))
			
			#remove duplicated
			message(cat(paste("The following duplicates were removed:","\n")))
			message(cat(paste(unique(objc[dup.id])),sep="\n"))
			# remove NA
			message(cat(paste("Bad inputs were removed:","\n")))
			message(cat(paste(unique(objc[na.id])),sep="\n"))
			
			cid.objects<-objn[!(na.id | dup.id)]
		} else { 
			cid.objects<-objn 
	}
	
	#check for cid object in local DB
	DB.ids<-names(DB)
	need.id<-!cid.objects%in%DB.ids
	have.id<-DB.ids%in%cid.objects
	cmpd.DB<-DB[have.id]
	
	#use PUG webservices to get missing 
	if(sum(need.id)>0){
		tmp.cids<-unique(cid.objects[need.id])
	
		if(progress){
			message(cat("Using PubChem Power User Gateway (PUG) to get molecular fingerprint(s).\nThis may take a moment...","\n"))
		}
		
		#translate sdf file, modified from ChemmineR
		read.sdf<-function (sdfstr) {
				
				#number of queries controlled in url
				if (length(sdfstr) > 1) {
					mysdf <- sdfstr
				} else {
					mysdf <- readLines(sdfstr)
				}
				
				y <- regexpr("^\\${4,4}", mysdf, perl = TRUE)
				index <- which(y != -1)
				indexDF <- data.frame(start = c(1, index[-length(index)] + 
					1), end = index)
				mysdf_list <- lapply(seq(along = indexDF[, 1]), function(x) mysdf[seq(indexDF[x, 
					1], indexDF[x, 2])])
				if (class(mysdf_list) != "list") {
					mysdf_list <- list(as.vector(mysdf_list))
				}
				names(mysdf_list) <- 1:length(mysdf_list)
				#mysdf_list <- new("SDFstr", a = mysdf_list)
				return(mysdf_list)
			}
		
		#due to url string size limit query query.limit sdf obj at a time
		blocks<-c(seq(1,length(tmp.cids),by=query.limit),length(tmp.cids))
		# should use cv fold generating fxn to avoid boundary overlaps 
		compounds<-list() #breaks=ceiling(length(cids)/25),include.lowest = TRUE)
		for(i in 1:(length(blocks)-1)){
			url<-paste0("http://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",paste(tmp.cids[blocks[i]:blocks[(i+1)]],collapse=","),"/SDF")
			compounds[[i]]<-read.sdf(url) 
		}
		
		#create cmpd.list holding sdf strings
		cmpd.list<-list()
		names<-tmp.cids#paste0("CMP",1:length(cid.objects)) # name doesn't matter? should be cid
		for(i in 1:length(compounds)){
			tmp<-unclass(compounds[[i]])
			names(tmp)<-names[blocks[i]:blocks[(i+1)]]
			cmpd.list<-c(cmpd.list,tmp)
		}
		#make sure all are unique
		cmpd.list<-cmpd.list[fixlc(names[!duplicated(names)])] #could get duplicated based on web query
		#combine with DB
		cmpd.DB<-c(cmpd.DB,cmpd.list)
			
		#add new records to DB and save
		if(update.DB) {
			new.DB<-c(DB,cmpd.list)
			assign(save.as,new.DB)
			save(save.as,list=save.as,file=save.as)
		}
	} 
	
	return(cmpd.DB)

}

get.tanimoto.from.SDF<-function(cmpd.DB,type="list",cut.off=0,...){	
	#convert to SDFstr
	#depends on ChemmineR 
	require(ChemmineR)
	cmpd.sdf.list<-new("SDFstr", a = cmpd.DB)
	sd.list<-as(cmpd.sdf.list, "SDFset")
	cid(sd.list) <- sdfid(sd.list)
	
	# Convert base 64 encoded fingerprints to character vector, matrix or FPset object
	fpset <- fp2bit(sd.list, type=2)
	# # # remove duplicates due to query boundary repeats
	# # fpset <- fpset[!duplicated(rownames(fpset)),,drop=FALSE]
	# if(parallel==TRUE)
		# {
				# #change this later
				# cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				# registerDoSNOW(cl.tmp) 
				# out<-foreach(j=c(1:length(rownames(fpset))),.combine="cbind") %dopar% ChemmineR::fpSim(fpset[j,], fpset)#length(codes)
				# stopCluster(cl.tmp)	
		# } else {
	out<-sapply(rownames(fpset), function(x) ChemmineR::fpSim(x=fpset[x,], fpset,sorted=FALSE)) 
	obj<-as.matrix(out)
	
	if(type=="list"){
		e.list<-gen.mat.to.edge.list(obj)
		final<-edge.list.trim(e.list,index=fixln(e.list[,3]),cut=cut.off,less.than=FALSE) # probably overkill
	}else{
		obj[obj<cut.off]<-0
		final<-obj
	}
	
	return(final)
}

#wrapper to get tanimoto from cid
CID.to.tanimoto<-function(cids,...){
	#wrapper for get sdf from cid 
	#convert sdf to tanimoto similarity
	cmpd.DB<-get.SDF.from.CID(cids,...)
	get.tanimoto.from.SDF(cmpd.DB,...)
}

#OBSOLETE
# #get tanimoto distances from cids (using PUG REST instead of Chemminer web api to get sdf)
# CID.to.tanimoto<-function(cids, cut.off = .7, parallel=FALSE, return="edge list"){
	# #used cids = PUBCHEM CIDS to calculate tanimoto distances
	# need<-c("snow","doSNOW","foreach","ChemmineR") # need to use others for mac
	# for(i in 1:length(need)){check.get.packages(need[i])}
	# #get fingerprint for calcs
	# data(pubchemFPencoding)
	
	# #remove and print to screen error vars
	# #removal ids
	# objc<-as.character(unlist(cids))
	# objn<-as.numeric(unlist(cids))
	# dup.id<-duplicated(objn)
	# na.id<-is.na(objn)
	# if(sum(c(dup.id,na.id))>0){
			
			# objc<-as.character(unlist(cids))
			# objn<-as.numeric(unlist(cids))
			
			# #remove duplicated
			# message(cat(paste("The following duplicates were removed:","\n")))
			# message(cat(paste(unique(objc[dup.id])),sep="\n"))
			# # remove NA
			# message(cat(paste("Bad inputs were removed:","\n")))
			# message(cat(paste(unique(objc[na.id])),sep="\n"))
			
			# cid.objects<-objn[!(na.id | dup.id)]
		# } else { cid.objects<-objn }
	# message(cat("Using PubChem Power User Gateway (PUG) to get molecular fingerprint(s). This may take a moment.","\n"))
	
	# #custom read sdf, avoid class for latter combining of PUG queries
	# read.sdf<-function (sdfstr) 
		# {
			# #downloads the data could replace with RCurl
			# if (length(sdfstr) > 1) {
				# mysdf <- sdfstr
			# }
			# else {
				# mysdf <- readLines(sdfstr)
			# }
			# y <- regexpr("^\\${4,4}", mysdf, perl = TRUE)
			# index <- which(y != -1)
			# indexDF <- data.frame(start = c(1, index[-length(index)] + 
				# 1), end = index)
			# mysdf_list <- lapply(seq(along = indexDF[, 1]), function(x) mysdf[seq(indexDF[x, 
				# 1], indexDF[x, 2])])
			# if (class(mysdf_list) != "list") {
				# mysdf_list <- list(as.vector(mysdf_list))
			# }
			# names(mysdf_list) <- 1:length(mysdf_list)
			# #mysdf_list <- new("SDFstr", a = mysdf_list)
			# return(mysdf_list)
		# }
	
	# #due to url string size limit query 25 sdf obj at a time
	# blocks<-c(seq(1,length(cid.objects),by=25),length(cid.objects))
	# # should use cv fold generating fxn to avoid boundary overlaps 
	# compounds<-list() #breaks=ceiling(length(cids)/25),include.lowest = TRUE)
	# for(i in 1:(length(blocks)-1)){
		# url<-paste0("http://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",paste(cid.objects[blocks[i]:blocks[(i+1)]],collapse=","),"/SDF")
		# compounds[[i]]<-read.sdf(url) # add class after combing all sdf new("SDFstr", a = mysdf_list)
		# # compounds[[i]]<-read.SDFstr(url)
	# }
	# #reformat to unnested list for conversion to sdfset 
	# cmpd.list<-list()
	# names<-paste0("CMP",1:length(cid.objects))
	# for(i in 1:length(compounds)){
		# tmp<-unclass(compounds[[i]])
		# names(tmp)<-names[blocks[i]:blocks[(i+1)]]
		# cmpd.list<-c(cmpd.list,tmp)
	# }
	# cmpd.list2<-new("SDFstr", a = cmpd.list)
	# sd.list<-as(cmpd.list2, "SDFset")
	# cid(sd.list) <- sdfid(sd.list)
	
	
	# # Convert base 64 encoded fingerprints to character vector, matrix or FPset object
	# fpset <- fp2bit(sd.list, type=2)
	# # remove duplicates due to query boundary repeats
	# fpset <- fpset[!duplicated(rownames(fpset)),,drop=FALSE]
	# if(parallel==TRUE)
		# {
				# #change this later
				# cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				# registerDoSNOW(cl.tmp) 
				# out<-foreach(j=c(1:length(rownames(fpset))),.combine="cbind") %dopar% ChemmineR::fpSim(fpset[j,], fpset)#length(codes)
				# stopCluster(cl.tmp)	
		# } else {
					# out<-sapply(rownames(fpset), function(x) ChemmineR::fpSim(x=fpset[x,], fpset,sorted=FALSE)) 
		# }
		
	# #edgelist
	# #colnames(out)<-unlist(dimnames(out)[1])1
	
	# #optionally filter based on score based on score
	# obj<-as.matrix(out)
	
	# if(return=="edge list"){
		# e.list<-gen.mat.to.edge.list(obj)
		# final<-edge.list.trim(e.list,index=fixln(e.list[,3]),cut=cut.off,less.than=FALSE)
	# }else{
		# obj[obj<cut.off]<-0
		# final<-obj
	# }
	# return(final)
# }

#querry chemical translation service (CTS) to get tanimoto from inchis (very slow)
CID.to.tanimoto.CTS<-function(cid,lookup=get.CID.INCHIcode.pairs()){
		check.get.packages("XML")
		matched<-lookup[c(1:nrow(lookup))[lookup[,1]%in%cid],]
		matched<-matched[!matched[,2]=="",]
		codes<-gsub("=", "%3D", matched[,2])
		#use webservice to get tanimoto score between cids based in inchi key
		#do in parallel
		check.get.packages(c("snow","doSNOW","foreach"))

		i<-1
		out<-list()
		for(i in 1:length(codes))
			{
				
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				
				#fxn accesing web  can't be parallel?
				.local<-function(i,j,codes)
					{
						url=paste("http://vulcan.fiehnlab.ucdavis.edu:8080/tanimoto-service-1.2/rest/xml/calc.xml?from=",codes[i],"&to=",codes[j],sep="")
						
						text<-tryCatch(XML::xmlTreeParse(url),error=function(e){NULL})
						if(is.null(text))
							{
								return()
							}else{
								as.numeric(strsplit(unlist(text$doc$children$result[[3]]),">")[3])
							}
					}
					
			out[[i]]<-foreach(j=c(1:length(codes)),.combine="c") %dopar% .local(i,j,codes=codes)#length(codes)
			stopCluster(cl.tmp)	
			}
			
		names(ids)<-matched[,1]	# cid of all paired by cid
		
		#construct symmetric matrix then extract unique edge list
		mat<-do.call("rbind",lapply(1:length(ids),function(i)
			{
				obj<-ids[[i]]
				match<-sapply(1:length(ids), function(j)
					{
						tmp<-ids[[j]]
						sum(tmp%in%obj)
					})
			}))
		dimnames(mat)<-list(names(ids),names(ids))
		elist<-gen.mat.to.edge.list(mat)
		as.data.frame(elist[elist[,3]==1,1:2])	#cid source to cid target based on kegg pairs	
	}

#functions for devium network GUI to calculate edge list
devium.network.execute<-function(object,filter=as.numeric(object$devium.network.edge.list.weight.cutoff),FDR=as.logical(object$devium.network.edge.list.weight.cutoff.use.FDR)){
		check.get.packages(c("WGCNA","Hmisc"))
		#stored in get("devium.network.object",envir=devium)
		#switch for data  of CIDs
		if(object$devium.network.target.type=="Data"){
					main<-tryCatch(get(object$devium.network.target.object),error=function(e){NULL})
			} else{
					main<-tryCatch(get(object$devium.network.target.object),error=function(e){NULL})
					#check if it is inside a data frame and try to get it
					if(is.null(main)) {main<-tryCatch(as.matrix(as.numeric(as.character(unlist(gget(object$devium.network.target.object))))),error=function(e){NULL})}
			}
			
			if(is.null(main))
				{ 
						return()
				} else {
				
					#routing functions
					edge.list.type<-object$devium.network.edge.list.type
					tmp.data<-main[sapply(1:ncol(main), function(i) {class(main[,i])=="numeric"})]#cut out factors if present need for data
					switch(edge.list.type,
					"spearman correlations" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="spearman") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
													
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
																				
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},
					"pearson correlations" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="pearson") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
					
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},
					"biweight mid-correlation" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="biweight") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
													
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},							
					"KEGG reaction pairs" 	= .local<-function()
												{
													#return edge list
													obj<-CID.to.KEGG.pairs(as.matrix(main),database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs())
													out<-data.frame(cbind(obj,1))
													list(full.edge.list=out,weights = data.frame(cbind(obj[,1:2],1)[,3],drop=FALSE), filtered.edge.list = obj )
												},
												
					"Tanimoto distances"	= .local<-function()
												{
													#return edge list
													obj<-CID.to.tanimoto(as.matrix(main), cut.off = filter, parallel=TRUE)
													filtered<-obj[as.numeric(as.character(unlist(obj[,3])))>=filter,]
													list(full.edge.list=obj,weights = data.frame(obj[,3],drop=FALSE), filtered.edge.list = filtered )
												}		
							)
					
				elist<-.local()	
				d.assign("devium.network.edge.list.full",elist$full.edge.list,main.object="devium.network.object")
				d.assign("devium.network.edge.list.weights",elist$weights,main.object="devium.network.object")
				d.assign("devium.network.edge.list.calculated",elist$filtered.edge.list,main.object="devium.network.object")
				
				#may want to to also assign to global	
				assign(paste(object$devium.network.target.object,"network.edge.list", sep="."),elist$filtered.edge.list, envir=globalenv())
				}
	}

#function to add to existing igraph.plot
devium.igraph.plot<-function(edge.list,graph.par.obj=NULL,plot.type="static",add=FALSE,not.dev=FALSE){
		#p1lot.type = c("static","interactive","3D-plot")
		check.get.packages(c("igraph","graph")) 
		
		#test if new graph needs to be created
		if(is.null(graph.par.obj$x)){
			#create grapNEL object from edge list
			graph.obj<-edge.list.to.graphNEL(edge.list)
			# could calculate this directly but for now going through NEL because it is also used for Cytoscape graphs
			igraph.obj<-igraph.from.graphNEL(graph.obj, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
		} 
		
		#default options for igraph.plot
		defaults<-list(
		x = igraph.obj,
		mark.groups = NULL,
		mark.col = NULL,		# needs to be "NULL" else skipped below
		layout = "layout.fruchterman.reingold",  #have to get this later
		vertex.label = unclass(igraph.obj)[[9]][[3]]$name, #this it the graph object later 
		vertex.color ="gray",
		vertex.size = 6,
		vertex.label.dist=-.3)
		
		
		#join defaults with graph.par.obj
		graph.par<-defaults
		i<-1
		for(i in 1:length(defaults))
			{
				j<-1
				for(j in 1:length(names(graph.par.obj)))
					{
						if(!is.null(graph.par.obj)){
							if(as.character(names(defaults)[i])==as.character(names(graph.par.obj)[j]))
								{
										graph.par[[i]]<- graph.par.obj[[j]] #tryCatch(get(unlist(graph.par.obj[j])),error=function(e){graph.par.obj[j]})
								}
						}								#else { 
									# tmp<-graph.par[i]
									# graph.par[i]<-tmp
							# }
					}
			}
		
		#translate names to use call for tkplot and rgl plot
		#switch names to generate calls for tkplot 
		switch(plot.type,
		"interactive" 	= names(defaults)[c(1,2)]<-c("graph","..."),
		"3D-plot" 		= names(defaults)[c(2)]<-"...")
		
		names(graph.par)<-names(defaults)
		
		#calculate layout to be shared by all plots
		message(cat("Calculating layout","\n") )
		
		# test layout to see if it is matrix
		# else calculate and replace
		if(ncol(as.data.frame(graph.par[names(graph.par)=="layout"]))==1)
			{
				graph.par[names(graph.par)=="layout"][[1]]<-as.matrix(do.call(unlist(graph.par[names(graph.par)=="layout"]),list(graph.par[[1]])))
			}
		
		#try to get objects ... if error stay with default ( was in the loop above, but need mechanism to not get for layout)
		
		
		#add third dimension if using 2D layout for 3D-plot
		if(plot.type=="3D-plot" & ncol(as.data.frame(graph.par[names(graph.par)=="layout"]))<3)
			{
				graph.par[names(graph.par)=="layout"][[1]]<-as.matrix(cbind(as.data.frame(graph.par[names(graph.par)=="layout"]),0))
			}
	
		#call plot
		switch(plot.type,
				static 		= do.call("plot",graph.par),
				interactive = do.call("tkplot",graph.par),
				"3D-plot" 	= do.call("rglplot",graph.par))
	}

#use chemical resolver to get inchi keys from smiles
get.inchikey.from.smiles<-function(smiles,progress=TRUE){
		# smiles are coerced to a 1 column data frame
		obj<-data.frame(matrix(unlist(smiles),ncol=1))
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)} # need RCurl for web querry
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = nrow(obj), style = 3)} # show progress bar
	
		start<-"http://cactus.nci.nih.gov/chemical/structure/"
		end<-"/stdinchikey"
		out<-sapply(1:nrow(obj),function(i)
			{
				if (progress == TRUE){setTxtProgressBar(pb, i)}
			
				close(pb)
				url<-paste(start,as.character(unlist(obj[i,])),end,sep="")
				url<-gsub("\\ ","%20",url) # fix spaces 
				tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){"error"})
					
			})
			
			if (progress == TRUE){close(pb)}
			
		#format output to only return InchI
		bad<-is.na(smiles)
		out<-as.character(unlist(out))
		out[bad]<-"InChIKey=error"
		#results<-matrix(as.character(unlist(as.data.frame(strsplit(out,"="))[2,])),ncol=1)
		results<-matrix(out,ncol=1)
		colnames(results)<-"InchI Key"
		return(results)
		}

#plot nodes vs edges for edge list and at some given threshold based on an index
plot.nodes.and.edges<-function(edge.list,index=NULL,.threshold=c(0,0.05), levels=10, plot="seperate"){
		
		#choose = c("interactive", "auto")
		#plot = c("seperate","ratio")
		#choose elbow in vertex vs. edge plot
		int<-c(seq(.threshold[1],.threshold[2],length.out=levels)[-1])
		
		xy<-do.call("rbind",lapply(1:length(int),function(i)
				{
					net <- edge.list[index<=int[i],]
					nodes<- unique(as.character(unlist(net)))
					data.frame(threshold=int[i],nodes=length(nodes),edges=nrow(net))
				}))
		#plot
		if(plot=="ratio"){ 
				plot(xy[,2]/xy[,3],xy[,1],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="nodes / edges")
			} else {
				plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				#show threshold wher all nodes are connected
				tmp<-which.min(abs(nrow(edge.list)-xy[,2]))
				abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
		}
		# if(choose=="auto") 
			# {
				# return(list(auto.threshold=xy[tmp,1],list=xy))
			# }
			
		# if(choose=="interactive")
			# {
				# tmp<-locator(1)$y
				# tmp<-which.min(abs(xy[,1]-tmp))
				# plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				# lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				# legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				# #show threshold wher all nodes are connected
				# abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				# abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				# abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				# title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
				# return(xy[tmp,1])
			# }
	}

#convert mass spectra input as m/z:intensity string to matrix
spectra.string.to.matrix<-function(spectra, encode.char = ":"){
		library(plyr)
		#get m/z intensity pairs 
		tmp<-lapply(1:length(spectra),function(i){
			strsplit(fixlc(strsplit(fixlc(spectra[i])," ")),encode.char)
		})
		#result in list form
		mat<-lapply(1:length(tmp),function(i){ data.frame(analyte=i,do.call("rbind",tmp[[i]]))})
		#melted object
		mat2<-do.call("rbind",mat)
		colnames(mat2)<-c("analyte","m_z","intensity")
		#cast into a matrix, change NA to zeros
		spec.mat<-dcast(mat2,m_z ~ analyte,value.var="intensity")
		spec.mat[is.na(spec.mat)]<-0
		#convert to numeric and order by m_z
		spec.mat<-data.frame(sapply(spec.mat,fixln))
		spec.mat<-spec.mat[order(as.numeric(spec.mat$m_z)),]
		#format into matrix with m_z as rows
		tmp<-spec.mat[,-1]
		dimnames(tmp)<-list(spec.mat$m_z,c(1:ncol(tmp)))
		return(as.matrix(tmp))
}

#get cosine correlations from mz/intensity strings
get.spectral.edge.list<-function(spectra, known = 0, cutoff = 0.7, edge.limit = max(1:length(spectra)),retention.index=NULL,retention.cutoff=100){
	#spectra = encoded mass spectar of type "mz : intensity" string
	#known = row index for "known" metabolites to allow unknown connections too
	#cutoff = cosine correlation coefficient >= to accept 
	#edge.limit = maximum number of connections between each unknown and known(s)
	# library(lsa) # when cannnot load library like on shiny server due to JAVA dependancie mismatch
	cosine<-function (x, y = NULL) 
		{
			if (is.matrix(x) && is.null(y)) {
				co = array(0, c(ncol(x), ncol(x)))
				f = colnames(x)
				dimnames(co) = list(f, f)
				for (i in 2:ncol(x)) {
					for (j in 1:(i - 1)) {
						co[i, j] = cosine(x[, i], x[, j])
					}
				}
				co = co + t(co)
				diag(co) = 1
				return(as.matrix(co))
			}
			else if (is.vector(x) && is.vector(y)) {
				return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
			}
			else {
				stop("argument mismatch. Either one matrix or two vectors needed as input.")
			}
		}


	
	if(all(na.omit(as.numeric(known)) == 0) ){ known <- 0} # long story
	
	#get cosine correlations between spectra (link unknown)
	spec.mat<-spectra.string.to.matrix(spectra)
	
	cos.cor<-cosine(spec.mat)
	cos.cor<-as.data.frame(cos.cor)
	dimnames(cos.cor)<-list(colnames(spec.mat),colnames(spec.mat)) # cosine correlations
	# convert to edge.list
	# extract known in network
	# create edges between knowns and unknowns 
	# based on max cosine cor and above some threshold

	edge.list<-gen.mat.to.edge.list(mat=cos.cor)
	#filter list now to speed up
	edge.list<-matrix(fixln(edge.list[abs(fixln(edge.list[,3]))>=cutoff,]),ncol=3)
	
	if(length(edge.list) > 0) {
		#id for unknown
		if ( all(known == 0)) { 
			unknowns<-1:length(spectra) 
		} else { 
			known<-c(1:length(spectra))[!known == 0 & !is.na(known)]
			unknowns<-c(1:length(spectra))[-known]
		}
		
		# scan edge.list looking for known to unknown connections
		known.id1<-edge.list[,1]%in%known & !edge.list[,2]%in%known
		known.id2<-edge.list[,2]%in%known & !edge.list[,1]%in%known 
		known.id<-known.id1|known.id2
		
		if(sum(known.id)>0){
			query.index<-c(1:nrow(edge.list))[known.id] # position in list for knowns
			edges<-edge.list[query.index,]
			#make sure known is listed as source
			edge.list[known.id2,1:2]<-edge.list[known.id2,2:1]
		}	else {
			edges<-edge.list
		}

		# tmp2<-split(data.frame(edges),as.factor(edges[,1]))
		# #limit top edges per node 
		# #need to fix, not working properly
		# top.edges<-lapply(1:length(tmp2),function(i){
			# obj<-tmp2[[i]][order(tmp2[[i]][,3],decreasing=TRUE),]
			# obj[c(1:nrow(obj))<=edge.limit,]
		# })
		
		top.id<-edge.list.filter.partial(edge.list=edges[,1:2,drop=FALSE],weight=abs(fixln(edges[,3])),max.edges=edge.limit)
		#could use edge.list.full for more extreme filtering
		results<-edges[top.id,,drop=FALSE]
		
		# results<-do.call("rbind",top.edges)
		results<-data.frame(results[!is.na(results[,1]),,drop=FALSE])
		colnames(results)<-c("source", "target", "weight")
		
		#filter connections based retention time
		if(!is.null(retention.index)) {
			tr1<-retention.index[results[,1]]
			tr2<-retention.index[results[,2]]
			delta<-abs(tr1-tr2)
			#add to results for output--breaking pattern of 3 column edge list output
			results$delta.retention.time<-delta
			selected<-delta<=retention.cutoff
			if(sum(selected	)>=1){
				results<-results[selected,]
			} else { message("No edges met criteria");results<-NULL}		
		}
		
	} else {message("No edges met criteria");results<-NULL}	
		
	return(results)
}

#extract spectra from .mgf files (then use spectra.string.to.matrix to convert to a more usable form)
mgf.to.spectra.string<-function(file){
	data<-as.matrix(read.delim(file))
	start<-grep("RTINSECONDS",data)
	end<-grep("END IONS",data)
	parent<-gsub("PEPMASS=","",gsub(" ","",data[grep("PEPMASS",data),]))
	retention<-gsub("RTINSECONDS=","",gsub(" ","",data[grep("RTINSECONDS=",data),]))
	#extract MS/MS spectra as m/z:intensity with separate values separated by " "
	spectra<-sapply(1:length(start), function(i){
		tmp<-data[(start[i]+1):(end[i]-1),]
		paste(join.columns(t(matrix(tmp,nrow=2)),":"), collapse=" ")
	})
	data.frame(parent=parent, retention.time=retention, spectra=spectra)
}

# convert KEGG kgml to edgelist
kgml.to.edgelist<-function(kgml){
	doc<-xmlParse(kgml)	
	x<-xmlToList(doc)
	id<-names(x)=="reaction"
	tmp<-x[id]
	do.call("rbind",lapply(1:sum(id), function(i){

		obj<-tmp[[i]]
		sub<-names(obj)=="substrate"
		prod<-names(obj)=="product"
		type<-names(obj)==".attrs"
		
		#construct edge list
		#expand relationships for multi-component reactions
		source<-gsub("cpd:","",matrix(unlist(obj[sub]),ncol=2, byrow=TRUE)[,2])
		target<-gsub("cpd:","",matrix(unlist(obj[prod]),ncol=2, byrow=TRUE)[,2])
		rtype<-gsub("rn:","",matrix(unlist(obj[type]),ncol=3, byrow=TRUE)[,-1])
		
		cbind(data.frame(source = rep(source,length(target)), target=rep(target,each=length(source)) ), data.frame(reaction=rtype[1],direction=rtype[2]))

	}))
}	
#
#relic needs to be replaced elsewhere
make.edge.list.index<-function(edge.names, edge.list){
	names<-colnames(edge.list)
	edge.list<-do.call("cbind",lapply(1:ncol(edge.list),function(i) fixlc(edge.list[,i])))
	colnames(edge.list)<-names
	tmp<-data.frame(translate.index(id = edge.list[,1:2,drop=FALSE], lookup = edge.names))
	tmp[,1]<-fixln(tmp[,1])
	tmp[,2]<-fixln(tmp[,2])
	colnames(tmp)<-c("source","target")
	return(as.matrix(tmp)) #
}

#functions to create node attributes
set.node.size<-function(obj,type="FC", min=40, max=100, log=TRUE){
	if(type=="FC"){ 
	#need to deal with Inf = 1/0, 0 = 0/1 and NaN = 0/0
	# make slightly bigger (Inf, 0) or the set to min scale (NaN)
		tmp<-obj
		clean<-range(tmp[!tmp=="Inf"&!tmp==0&!tmp=="NaN"])[2]*6/5
		# rencode
		tmp[tmp=="Inf"]<-clean
		tmp[tmp==0]<-clean
		n.id<-tmp=="NaN"
		tmp[n.id]<-clean # fix after scaling
		#remove direction
		tmp[tmp<1]<-1/tmp[tmp<1]
		
		if(log){tmp<-log(tmp+1)}
	}
	#scale sizes 
	
	scale.sizes<-function(obj, new.min=40, new.max=100){
		((obj-min(obj))*(new.max-new.min))/ (max(obj)+min(obj))+ new.min}
	
	res<-scale.sizes(tmp,new.min=min,new.max=max)
	res[n.id]<-min(res)# fix NaN
	return(res)
}

#shape
set.node.shape<-function(obj,increase="triangle",decrease="vee", no.change="ellipse"){
	tmp<-obj
	#need to deal NaN (0/0) coming from ratios
	tmp[obj<1]<-decrease
	tmp[obj=="NaN"]<-no.change # comes from 0/0
	tmp[obj>1]<-increase
	tmp[obj==1]<-no.change
	return(tmp)
}

#color
set.node.color<-function(obj,inc.lev="triangle",dec.lev="vee",no.lev="ellipse",inc.col="red",dec.col="blue", no.col="gray"){
	library(gplots) # convert color name to hex
	
	tmp<-obj
	tmp[obj==inc.lev]<-inc.col
	tmp[obj==dec.lev]<-dec.col
	tmp[obj==no.lev]<-no.col
	tmp<-col2hex(tmp)
	return(tmp)
}

# #remove self edges and duplicated edges based on edgelist type
# clean.edgeList<-function( data=edge.list,source="source",target="target",type="type"){
   
    # library(igraph)
    # #remove self edges else if all self passed will cause an error
    # el<-data[,c(source,target)]
    # self<-el[,1]==el[,2]
    # el<-el[!self,]
    # tmp.data<-as.data.frame(as.matrix(data)[!self,])
    # lel<-split(el,tmp.data$type)
    
    # el.res<-do.call("rbind",lapply(1:length(lel),function(i){
		# nodes<-matrix(sort(unique(matrix(as.matrix(lel[[i]]),,1))),,1)
		# g<-graph.data.frame(lel[[i]],directed=FALSE,vertices=nodes)
		# g.adj<-get.adjacency(g,sparse=FALSE,type="upper")
		# g.adj[g.adj>0]<-1
		# adj<-graph.adjacency(g.adj,mode="upper",diag=FALSE,add.rownames="code")
		# get.edgelist(adj)
	# }))
    
  # ids<-unique(join.columns(el.res))  
  # tmp<-data.frame(tmp.data[,!colnames(tmp.data)%in%c(source,target)])
  # rownames(tmp)<-make.unique(join.columns(el[,1:2])) 
  # flip<-!ids%in%rownames(tmp) 
  # ids[flip]<-unique(join.columns(el.res[,2:1]))[flip]
  # res<-data.frame(el.res,tmp[ids,])	
  # colnames(res)<-colnames(data)	
  # return(res)  
    
# }

#remove self and duplicated edges
#simpler of the one above
clean.edgeList<-function(data,source="source",target="target",type=NULL){
	
	#source and target should be numeric or will be coerced to numeric
	data[,source]<-fixln(data[,source])
	data[,target]<-fixln(data[,target])
	
	#remove self edges
	id<-data[,source]==data[,target] 
	data<-data[!id,]
	
	if(!is.null(type)){
		data<-do.call("rbind",split(data,data[,"type"])) # ugly but works
	}
	#remove duplicated connections
	#format edge names by sorting (need to be numeric)
	#could add mechanism to handle character via conversion to factor then to numeric
	# and add look up table for back conversion
	id<-data[,source]>data[,target]
	tmp<-data[,source]
	data[,source][id]<-data[,target][id]
	data[,target][id]<-tmp[id]
	#remove duplicated
	id<-paste0(data[,source],"|",data[,target])
	dupes<-duplicated(id)
	data[!dupes,]

}




test<-function(){


kegg.id<-c("C00212","C00020","C00105", "C00299")

#KEGG rxn DB
DB<-get.KEGG.pairs(type="main")
#create KEGG and CID based biochemical/chemical similarity network
kegg.list<-get.Reaction.pairs(kegg.id,DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE)


#get tanimoto similarity
cids<-c("70","51","204")
DB2<-tryCatch(get(load("data/CID.SDF.DB")[1]),error=function(e) {NULL})
cid.list<-CID.to.tanimoto(cids,DB=DB2,update.DB=FALSE)

#merge the two edge list maintaining some hierarchy of edges
obj<-c(kegg.id,cids)
id<-data.frame(obj,1:length(obj))
tmp<-list()
tmp$source<-translate.index(fixlc(cid.list[,1]), lookup=id)

clean.list<-clean.edgeList(data=res)


#translate to a common edge list
ID<-1:nrow(1)
trans.s<-translate.index(fixlc(res[,1]), lookup=cbind(,fixlc(tmp.id)))
trans.t<-translate.index(fixlc(res[,2]), lookup=cbind(1:nrow(getdata()),fixlc(tmp.id)))


}