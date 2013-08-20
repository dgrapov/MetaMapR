# move elsewhere post debugging
#ggplot based network drawing fxn
ggplot2.network<-function(edge.list, edge.color.var = NULL, edge.color = NULL, directed = FALSE,
						node.color = NULL, show.names = TRUE,
						bezier = TRUE, node.size = 7,node.label.size = 5, max.edge.thickness = 2){
	# edge list  = 2 column data.frame representing source and target. Columns over 2 will be sorted with edgelist. 
	# edge.color.var = name of variable in edge list to use to color
	# edge.color = color for each level of object edge.color.var
	# directed = logical, if FALSE edge will be transposed and duplicated making undirected
	# node.color = colors for nodes, need to take into account node name ordering
	# node.names = names of nodes
	
	# Function to generate paths between each connected node
	# adapted from : https://gist.github.com/dsparks/4331058
	edgeMaker <- function(whichRow, len = 100, curved = TRUE){
	  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
	  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
	 
	  # Add curve:
	  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
	  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
	  distance1 <- sum((graphCenter - bezierMid)^2)
	  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
		bezierMid <- c(toC[1], fromC[2])
		}  # To select the best Bezier midpoint
	  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
	  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
	 
	  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
								c(fromC[2], bezierMid[2], toC[2]),  # X & y
								evaluation = len))  # Bezier path coordinates
	  edge$Sequence <- 1:len  # For size and colour weighting in plot
	  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
	   if(ncol(adjacencyList)>2){
			tmp<-data.frame(matrix(as.matrix(adjacencyList[whichRow, -c(1,2),drop=FALSE]),nrow = nrow(edge), ncol=ncol(adjacencyList)-2, byrow=TRUE))
			colnames(tmp)<-colnames(adjacencyList)[-c(1:2)]
			edge$extra<-tmp
			edge<-do.call("cbind",edge)
			colnames(edge)<-gsub("extra.","",colnames(edge))
		}
	  return(edge)
	  }
	  
	edgeMaker2<-function(whichRow){
	  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
	  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
	 
	  edge <- data.frame(c(fromC[1], toC[1]), c(fromC[2] ,toC[2]))  # Generate
								 # X & )  # Bezier path coordinates
	  edge$Sequence <- 1 # For size and colour weighting in plot
	  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
	  #get other info if supplied with edge list
	  if(ncol(adjacencyList)>2){
			tmp<-data.frame(matrix(as.matrix(adjacencyList[whichRow, -c(1,2),drop=FALSE]),nrow = nrow(edge), ncol=ncol(adjacencyList)-2, byrow=TRUE))
			colnames(tmp)<-colnames(adjacencyList)[-c(1:2)]
			edge$extra<-tmp
			edge<-do.call("cbind",edge)
			colnames(edge)<-gsub("extra.","",colnames(edge))
		}
	  colnames(edge)[1:2]<-c("x","y")
	  return(edge)
	  }
	
	# adding transposed source target edges to make undirected bezier curves
	if (bezier == TRUE) {
		if(all(!directed)) { is.rev<-rep(TRUE, nrow(edge.list)) } else { is.rev<-directed==TRUE }
		rev.edge.list<-data.frame(rbind(as.matrix(edge.list[,1:2]),as.matrix(edge.list[is.rev,2:1]))) # need matrix else no reordering of columns?
	} else{ 
		rev.edge.list<-edge.list[,1:2]
	}
	#extra info (separate now, later recombine)
	info<-edge.list[,-c(1:2)]
	
	#getting layout and making sure edeg list ids are in the same order
	g<-as.network(rev.edge.list[,1:2],matrix.type = "edgelist")
	#control remaking of the layout (only update if edge.list has changed)
	if(!exists("node.layout")){
		node.layout<<-gplot.layout.fruchtermanreingold(g[,], layout.par = NULL)
		values$network_state<-g
	}
	#marker of a change in state
	if(!identical(g,values$network_state)){
		node.layout<<-gplot.layout.fruchtermanreingold(g[,], layout.par = NULL)
		values$network_state<-g
	}
	n.edge.list<-as.matrix.network.edgelist(g)
	dimnames(node.layout)<-list(rownames(g[,]),c("x","y"))

	#preparing for edge path
	layoutCoordinates<-node.layout
	adjacencyList<-data.frame(n.edge.list,info)

	if (bezier == TRUE) {
		allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE)
		allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
	 } else {
		#straight edges using same controls(faster)
		allEdges <- lapply(1:nrow(adjacencyList), edgeMaker2)
		allEdges <- do.call(rbind, allEdges)
	}
	allEdges$neg.Sequence<- - allEdges$Sequence

	
	#set up for plotting
	#theme 
	new_theme_empty <- theme_bw()
	new_theme_empty$line <- element_blank()
	new_theme_empty$rect <- element_blank()
	new_theme_empty$strip.text <- element_blank()
	new_theme_empty$axis.text <- element_blank()
	new_theme_empty$plot.title <- element_blank()
	new_theme_empty$axis.title <- element_blank()
	new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
                                        
	#set default plotting variables
	# Edge colors
	if(is.null(edge.color)){
		if(is.null(edge.color.var)){
			edge.color=rep("gray20",2) # no clue why 2 are needed as a default
			edge.guide = FALSE
		} else {
			edge.color<-rainbow(nlevels(as.factor(with (edge.list, get(edge.color.var)))))
			edge.guide = TRUE
		}
	}
	# node colors
	if(is.null(node.color)){node.color <-"red"; node.guide = FALSE} else {node.guide = TRUE}
	# node names
	if(length(show.names) == attr(n.edge.list,"vnames")) { node.names <- show.names} 
	if (show.names) { node.names<-attr(n.edge.list,"vnames") } 
	if(!show.names){node.names<-rep("",nrow(node.layout))}
	#make plot
	zp1 <- ggplot(allEdges)  # Pretty simple plot code
	#bezier edges	
	zp1 <- zp1 + geom_path(aes_string(x = "x", y = "y", group = "Group",  # Edges with gradient
							   colour = edge.color.var, size = "neg.Sequence"))  # and taper # Customize taper					   
	#nodes					   
	zp1 <- zp1 + geom_point(data = data.frame(layoutCoordinates, color = node.color),  # Add nodes
							aes(x = x, y = y, fill = color), size = node.size, pch = 21,colour = "black",  show_guide = node.guide)# Add
	zp1<-zp1 + geom_text(data = data.frame(layoutCoordinates, label = node.names),  
							aes(x = x, y = y-.2, label = label), size = node.label.size)	# node names
	zp1 <- zp1 + scale_colour_manual(values = edge.color, guide = edge.guide)
	zp1 <- zp1 + scale_size(range = c(1/100, max.edge.thickness), guide = "none")  #edge thickness
	zp1 <-zp1 + guides(color = guide_legend(override.aes = list (size = 1.5 ))) + labs(color='Edge Type')	
	# Customize gradient 
	zp1 <- zp1 + new_theme_empty  # Clean up plot
	print(zp1)
}

#debugging  print all names and values in input
output$debug<- renderPrint({
	obj<-names(input)
	input.obj<-lapply(1:length(obj), function(i) { input[[obj[i]]]})
	names(input.obj)<-obj
	obj<-names(values)
	values.obj<-lapply(1:length(obj), function(i) { values[[obj[i]]]})
	names(values.obj)<-obj
	
	return(list(input = input.obj,values = values.obj))
})

getdata <- function(dataset = input$datasets) {
  values[[dataset]]
}	

loadUserData <- function(uFile) {

	# ext <- file_ext(uFile)  # for some reason at tmp file is being used
	# objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	tmp<- unlist(strsplit(as.character(unlist(input$serv_upload[1,1])),"\\."))
	ext <-tmp[length(tmp)]
	
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",as.character(unlist(input$serv_upload[1,1])))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  objname <- robjname <- load(uFile)
		values[[robjname]] <- get(robjname)
	}

	if(datasets[1] == '') {
		datasets <<- c(objname)
	} else {
		datasets <<- unique(c(objname,datasets))
	}

	if(ext == 'sav') {
		values[[objname]] <- read.sav(uFile)
	} else if(ext == 'dta') {
		values[[objname]] <- read.dta(uFile)
	} else if(ext == 'csv') {
		# values[[objname]]<-read.csv(uFile)
		if(input$csv_row_header) { if(input$csv_col_header){ row.names<-1 } else{}} else {row.names<-NULL}
		values[[objname]] <- data.frame(read.csv(uFile,header = input$csv_col_header, row.names=row.names))
		# get pesky ".variables" in column names
	}
	datasets <<- unique(c(robjname,datasets))
}

#load copy and paset field
loadcopyAndPaste <- function(pFile) {
	if(input$csv_row_header) { if(input$csv_col_header){ row.names<-1 } else{}} else {row.names<-NULL}
	robjname <- "clipboard"
	dat <- read.table(header = input$csv_col_header, row.names = row.names, text= pFile)
	
	if(is.null(ncol(dat))) {
		return()
	}

	values[[robjname]] <- dat

	if(datasets[1] == '') {
		datasets <<- c(robjname)
	} else {
		datasets <<- unique(c(robjname,datasets))
	}
}

#################################################
# reactive barrowed from radyant
#################################################

uploadfunc <- reactive({

  # if(input$upload == 0) return("")
 	# fpath <- try(file.choose(), silent = TRUE)
 	# if(is(fpath, 'try-error')) {
  # 	return("")
  # } else {
  # 	return(fpath)
  # }

 	values$fpath <- ""

   	if (!is.null(input$serv_upload)) {
 	    values$fpath <- input$serv_upload[1,'datapath'] 
 	  }

  values$fpath
})

output$upload_local_server <- renderUI({ # data upload function
	   withTags(div(class='row-fluid',
                 div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
                 div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE))))
	  fileInput('serv_upload','')
})

output$downloadData <- downloadHandler(
	filename = function() { paste(input$datasets[1],'.',input$saveAs, sep='') },
  content = function(file) {

	  ext <- input$saveAs
	  robj <- input$datasets[1]
	  assign(robj, getdata())

		if(ext == 'rda' || ext == 'rdata') {
	    save(list = robj, file = file)
		} 
		else if(ext == 'dta') {
			write.dta(get(robj), file)
		} else if(ext == 'csv') {
			write.csv(get(robj), file)
		}
  })


output$datasets <- renderUI({

	fpath <- uploadfunc()
	# loading user data
	if(fpath != "" ) loadUserData(fpath)
	
	
	
	# # copyAnd paste
	if(input$copyAndPaste != "") {
		if(input$copyAndPaste != values$clipboard) {
			loadcopyAndPaste(input$copyAndPaste)
		}
	}
	
	
	# Drop-down selection of data set
	selectInput(inputId = "datasets", label = "Select:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$view_data <-renderTable({
	data.frame(getdata())
})

#choose network inddex from the data and specify its type for carrying out translations
varnames <- function() {
	if(is.null(input$datasets)) return()
	colnames(getdata())
}

#names of databas identifiers
DB.names <- function() {
	if(is.null(input$datasets)) return()
	list("Chemical Name" = "name", "KEGG" = "kegg", "PubChem CID" = "pubchemCID", "BioCyc" =  "biocyc" ,"InChiKey" = "inchikey") #hmdb = "HMDB"
}

#mass spect encoding types
MZ.encode<-function(){
	list("m/z : intensity" = "mz_int")
}

#get network_spec

#function to rencode edge.list index
make.edge.list.index<-function(edge.names, edge.list){
	#need to replace old ids with ne code in multiple edge.lists
	e1<-translate.index(id= matrix(fixlc(edge.list[,1])), lookup = edge.names)
	e2<-translate.index(id= matrix(fixlc(edge.list[,2])), lookup = edge.names)
	data.frame(source = e1, target = e2)
}

# function for edge list calculations
#translate index and calculate edges
calculate_edgelist<-reactive({#function(){
	
	#will be results
	res<-data.frame(NULL)
	node.attr<-data.frame(network.index = 1:nrow(getdata())) # size depends on rows of data
	
	#Use KEGG RPAIRS for biochemical  connections (could add option for reaction type, currently only reporting "main" reactions)
	if(input$bio_edges){
		index<-getdata()[,input$network_index_bio]
		index.type<-switch(input$network_index_type_bio,
					kegg 	=	"KEGG",
					pubchemCID = "PubChem CID",
					name = "Chemical Name",
					biocyc = "BioCyc",
					inchikey = "InChiKey"
				)
		
	
		trans.id<- !"KEGG"%in%index.type
		if(trans.id){
			kegg.id<-CTSgetR(id = index, from=index.type,to="KEGG", async=TRUE)
		} else {kegg.id<-index}
		res<-data.frame(NULL)
		reaction.DB<-get.KEGG.pairs(type="main") # can add other types of relationships, main = direct precursor -> direct transformations
		# index.translation.DB<-data.frame(cids,kegg.ids)
		#get reaction pairs
		kegg.edges<-get.Reaction.pairs(kegg.id,reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE)
		
		#create shared index between diffrent edge ids
		index<-kegg.id
		edge.names<-data.frame(index, network.id = c(1:length(index)))
		kegg.edges<-make.edge.list.index(edge.names,kegg.edges)
		
		if(length(kegg.edges)>0){
			res<-data.frame(rbind(res,as.matrix(kegg.edges)),type = "KEGG", weight = 2)	
			node.attr<-data.frame(cbind(node.attr,biochemical.edge.index  = index))	
		}
		
	}
	
	#chemical similarity edges based on tanimoto coefficients from PubChem CID
	if(input$chem_edges){
		index<-getdata()[,input$network_index_chem]
		index.type<-switch(input$network_index_type_chem,
					kegg 	=	"KEGG",
					pubchemCID = "PubChem CID",
					name = "Chemical Name",
					biocyc = "BioCyc",
					inchikey = "InChiKey"
				)
	
		trans.id<- !"PubChem CID"%in%index.type
		if(trans.id){
			CID.id<-CTSgetR(id = index, from=index.type,to="PubChem CID")
		} else {CID.id<-index}
		
		# get tanimoto similarity
		tani.edges<-CID.to.tanimoto(cids=fixlc(CID.id), cut.off = input$tanimoto_cutoff, parallel=FALSE)
		
		#create shared index between diffrent edge ids
		index<-CID.id
		edge.names<-data.frame(index, network.id = c(1:length(index)))
		tani.edges[,1:2]<-make.edge.list.index(edge.names,tani.edges)
		
		if(nrow(tani.edges)>0){
			res<-data.frame(rbind(res,data.frame(as.matrix(tani.edges[,1:2]),type = "Tanimoto", weight = tani.edges[,3,])))
			node.attr<-data.frame(cbind(node.attr,chemical.edge.index  = index))	
		} 
	}
	
	#spectral similarity edges based on cosine correlation between m/z spectra
	if(input$spec_edges){
		index<-getdata()[,input$network_index_spec]
		known<-input$network_spec_primary_nodes
		if(!known == "0"){known<-getdata()[,known]} # long story
		
		spec.edges<-get.spectral.edge.list(spectra = index, known = known, cutoff = input$spec_cutoff, edge.limit = input$network_spec_nodes_max)
		if(nrow(spec.edges)>0){
			res<-data.frame(rbind(res,data.frame(as.matrix(spec.edges[,1:2]),type = "m/z", weight = spec.edges[,3])))
			node.attr<-data.frame(cbind(node.attr,mass.spectral.edge.index  = index))	
		} 
	}
	
	#edges based on correlation
	if(input$cor_edges){
		data<-getdata()[,input$network_index_cor]
		tmp<-devium.calculate.correlations(data,type=input$network_index_type_cor, results = "edge list")            
		
		#fdr adjust trade p-value for q-value
		if(input$cor_edges_fdr) { 
				# q.val<-FDR.adjust(obj = fixln(tmp[,4]),type="pvalue")
				adj.p<-p.adjust(fixln(tmp[,4]), method="BH")
				adj.p[is.na(as.numeric(adj.p))]<- 0 # error vars, assume due cor =1
				tmp[,4]<-adj.p
			}
		 
		#filter
		cor.edges<-tmp[fixln(tmp[,4]) <= input$cor_cutoff,]
		
		if(nrow(cor.edges)>0){
			type<-paste("positive",input$network_index_type_cor)
			res<-data.frame(rbind(res,data.frame(as.matrix(cor.edges[,1:2]),type = type, weight = abs(fixln(cor.edges[,3])))))
			res$type[fixln(cor.edges[,3])<=0]<-paste("negative",input$network_index_type_cor)
			res[,1]<-gsub("X","",fixlc(res[,1]))
			res[,2]<-gsub("X","",fixlc(res[,2]))
			#Which nodes are correlations calculated for
			cor.index<-rep(0,nrow(getdata()))
			cor.index[rownames(getdata())%in%gsub("X","",input$network_index_cor)]<-1
			node.attr<-data.frame(cbind(node.attr,correlation.edge.index  = cor.index))	
		} 
		
	}
	
	#remove duplicate edges (may fail if transposed)
	if(input$unique_edges) {
		id<-!duplicated(join.columns(res[,1:2]))
		values$id<-id
		res<-res[id,]
	}
	
	#save for other functions access
	values$edge.list<-res
	values$node.attributes<-node.attr
	return(res)
})


#function to rencode index
make.edge.list.index<-function(edge.names, edge.list){
	#need to replace old ids with ne code in multiple edge.lists
	e1<-translate.index(id= matrix(fixlc(edge.list[,1])), lookup = edge.names)
	e2<-translate.index(id= matrix(fixlc(edge.list[,2])), lookup = edge.names)
	data.frame(source = e1, target = e2)
}

#biochemical connections args
output$network_index_info_bio<-renderUI({
		
	wellPanel(
	 # withTags(div(class='row',
	 # div(class='span', checkboxInput(inputId = "bio_edges", label = "",value=FALSE)),
	 # div(class='span', h4('Biochemical')))),
	 checkboxInput(inputId = "bio_edges", label = "",value=FALSE),
	 h4('Biochemical'),
	 # tags$style(type='text/css', "#bio_edges { font-weight: bold; font-size:16px;}"),
		conditionalPanel(condition = "input.bio_edges",
			selectInput(inputId = "network_index_bio", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
			selectInput(inputId = "network_index_type_bio", label = "Index type:", choices = DB.names(), selected = DB.names()[2], multiple = FALSE)
		)	
	)
		
})
	
#chemical similarity args
output$network_index_info_chem<-renderUI({
	wellPanel(
	checkboxInput(inputId = "chem_edges", label = "",value=FALSE),
	h4('Chemical Similarity'),
		conditionalPanel(condition = "input.chem_edges",
		selectInput(inputId = "network_index_chem", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		selectInput(inputId = "network_index_type_chem", label = "Index type:", choices = DB.names(), selected = DB.names()[3], multiple = FALSE),
		numericInput(inputId = "tanimoto_cutoff" , "cutoff", value = 0.7, min = 0, max = 1, step = .005)
	))
})

#spectral similarity args
output$network_index_info_spec<-renderUI({
	wellPanel(
	checkboxInput(inputId = "spec_edges", label = "",value=FALSE),
	h4('Spectral Similarity'),
		conditionalPanel(condition = "input.spec_edges",
		selectInput(inputId = "network_index_spec", label = "Mass spectra:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		selectInput(inputId = "network_index_type_spec", label = "Encode type:", choices = MZ.encode(), selected = MZ.encode()[1], multiple = FALSE),
		selectInput(inputId = "network_spec_primary_nodes", label = "Primary nodes:", choices = c("none" = 0,varnames()), selected = "none", multiple = FALSE),
		numericInput(inputId = "network_spec_nodes_max", "Maximum connections", min = 1, max = 1000, value = 5, step = 1), # need to dynamixcally update max, or make it big for now?
		numericInput(inputId = "spec_cutoff" , "cutoff", value = 0.7, min = 0, max = 1, step = .005)
	))
})

#correlation args	
output$network_index_info_cor<-renderUI({
	 
wellPanel(
	checkboxInput(inputId = "cor_edges", label = "",value=FALSE),
	h4('Correlation'),
		conditionalPanel(condition = "input.cor_edges",
		selectInput(inputId = "network_index_cor", label = "Metabolite index:", choices = varnames(), selected = varnames(), multiple = TRUE),
		selectInput(inputId = "network_index_type_cor", label = "Method:", choices = list(pearson ="pearson", spearman = "spearman", biweight = "biweight"), selected = "spearman", multiple = FALSE),
		numericInput(inputId = "cor_cutoff" , "p-value", value = 0.05, min = 0, max = 1, step = .005),
		checkboxInput(inputId = "cor_edges_fdr", label = "FDR correct", value=TRUE)
	))
})

# Generate output for the summary tab
# output$summary <- renderUI(function() {
output$edge_list <- renderTable({
	if (input$create_edgelist == 0) 
		return(data.frame(NULL))
		
	#calculate edges		
	isolate({
		calculate_edgelist()
	})
	
})



# # Generate output for the plots tab
# output$network <- renderPlot({
		
		# # if(!input$metabomapr == "Network") return()
		# if(is.null(values$edge.list)) { 
			# plot(x = 1, type = 'n', main="Please calculate edge list first.", axes = FALSE, xlab = "", ylab = "")
		# } else if(length(values$edge.list) == 0) { 
			# plot(x = 1, type = 'n', main="No connections fit set criteria.", axes = FALSE, xlab = "", ylab = "")
		# } else {
		
		
			# #igraph network
			# #options for igraph.plot
			# #see all options http://127.0.0.1:10494/library/igraph/html/plot.common.html
			# # graph.par.obj<-list(
			# # x = NULL,# this is also calculated by the function should control
			# # mark.groups = NULL,
			# # mark.col = NULL,
			# # layout = DB[,c("x.pos","y.pos")],#"layout.fruchterman.reingold",
			# # vertex.label = node.par$name, 
			# # vertex.color = mark.col,
			# # vertex.size = 2,
			# # vertex.label.dist=-2)
			# graph.par.obj<-NULL
			# #need mechanism to selectively color edges
			# #make plot
			# edge.list<-values$edge.list[,1:2]
			# devium.igraph.plot(edge.list,graph.par.obj,plot.type="static",add=FALSE) # if running in local mode plot can be interactive or 3D
		# }
	# })
	
# Generate output for the plots tab
output$network <- renderPlot({
	
	if (input$create_edgelist_network == 0) 
		plot(x = 1, type = 'n', main="Please select options and draw the network.", axes = FALSE, xlab = "", ylab = "")

		#calculate edges and plot 		
	isolate({
		calculate_edgelist()
		# if(!input$metabomapr == "Network") return()
		if(is.null(values$edge.list)) { 
			plot(x = 1, type = 'n', main="Please calculate edge list first.", axes = FALSE, xlab = "", ylab = "")
		} else if(length(values$edge.list) == 0) { 
			plot(x = 1, type = 'n', main="No connections fit set criteria.", axes = FALSE, xlab = "", ylab = "")
		} else {
		
			edge.list<-values$edge.list
			ggplot2.network(edge.list, edge.color.var = "type", edge.color = NULL, directed = FALSE,
						node.color = NULL, show.names = input$network_plot_show_name,
						bezier = input$network_plot_bezier, node.size = input$network_plot_node_size, 
						node.label.size = input$network_plot_name_size, max.edge.thickness = input$network_plot_edge_size)				
						
		}
		})
	})	

#network attributes table
output$node.attributes <- renderTable({
	if (is.null(values$node.attributes)) 
		return(data.frame(NULL))
		
	#calculate edges		
	isolate({
		values$node.attributes
	})
	
})