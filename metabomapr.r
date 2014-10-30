
#custom mods to shiny UI to get
mainPanel2<-function (..., width1 = 4, width2=10) 
{
    div(class = paste0("span", width2," ","push",width1), ...)
}


#ggplot based network drawing fxn
ggplot2.network<-function(edge.list, edge.color.var = NULL, edge.color = NULL, directed = FALSE,
						node.color = NULL, show.names = TRUE, node.names=NULL,
						bezier = TRUE, node.size = 7,node.label.size = 5, max.edge.thickness = 2,file=NULL){
	# edge list  = 2 column data.frame representing source and target. Columns over 2 will be sorted with edgelist. 
	# edge.color.var = name of variable in edge list to use to color
	# edge.color = color for each level of object edge.color.var
	# directed = logical, if FALSE edge will be transposed and duplicated making undirected
	# node.color = colors for nodes, need to take into account node name ordering
	# show.names = can be supplied names for nodes, TRUE = network index, FALSE = nothing

	
	# Function to generate paths between each connected node (very slow when transparent!)
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
		rev.edge.list<-edge.list[,1:2,drop=FALSE]
	}
	#extra info (separate now, later recombine)
	info<-edge.list[,-c(1:2)]
	
	#getting layout and making sure edge list ids are in the same order
	g<-as.network(rev.edge.list[,1:2],matrix.type = "edgelist") # 
	#control remaking of the layout (only update if edge.list has changed)
	if(!exists("node.layout")){
		node.layout<<-gplot.layout.fruchtermanreingold(g[,], layout.par = NULL)
		values$network_state<-g
	}
	
	#marker of a change in state # not sure if used?
	if(!identical(g,values$network_state)){
		node.layout<<-gplot.layout.fruchtermanreingold(g[,], layout.par = NULL)
		values$network_state<-g
	}
	n.edge.list<-as.matrix.network.edgelist(g)
	dimnames(node.layout)<-list(rownames(g[,]),c("x","y"))

	
	if(is.null(node.names)){ 
			node.names<-attr(n.edge.list,"vnames") # default
		} else {
			#get from data set or node attributes
			tmp<-as.character(unlist(Nodeobjects()[,node.names]))
			node.names<-tmp[attr(n.edge.list,"vnames") ]
		}
	#if (show.names==TRUE) {node.names<-attr(n.edge.list,"vnames") } #default network index
	if(show.names==FALSE){node.names<-rep("",nrow(node.layout))} #nothing
	values$node.names<-node.names
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
	#need to maintain order of original edge.list$type
	if(!is.null(edge.color.var)){
		tmp<-with (edge.list, get(edge.color.var))
		ord<-fixlc(levels(tmp))
		allEdges[,edge.color.var]<-factor(allEdges[,edge.color.var],levels=ord,ordered=TRUE)
	}	

	#theme 
	new_theme_empty <- theme_bw()
	new_theme_empty$line <- element_blank()
	new_theme_empty$rect <- element_blank()
	new_theme_empty$strip.text <- element_blank()
	new_theme_empty$axis.text <- element_blank()
	new_theme_empty$plot.title <- element_blank()
	new_theme_empty$axis.title <- element_blank()
	new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
    new_theme_empty$legend.text <-element_text( size = 20)
	new_theme_empty$legend.title    <-element_text(size = 20 )  
	new_theme_empty$legend.position <- "top"
	new_theme_empty$plot.margin <- unit(x=c(5,5,5,5),units="mm") # need grid
	
	#set default plotting variables
	# Edge colors
	edge.guide <- TRUE
	if(is.null(edge.color)){
		if(is.null(edge.color.var)){
			edge.color=rep("gray20",2) # no clue why 2 are needed as a default
			edge.guide = FALSE
		} else {
			# edge.color<-rainbow(nlevels(as.factor(with (edge.list, get(edge.color.var)))))
			edge.color<-rainbow(nlevels(with (edge.list, get(edge.color.var))))
		}
	}
	# node colors
	if(is.null(node.color)){node.color <-"red"; node.guide = FALSE} else {node.guide = TRUE}
	
	#TODO: calculate x and y limits with some padding or ability to zoom out?
	
	
	# # node names (set above)
	# if(length(show.names) == attr(n.edge.list,"vnames")) { node.names <- show.names} 
	# if (show.names) { node.names<-attr(n.edge.list,"vnames") } 
	# if(!show.names){node.names<-rep("",nrow(node.layout))}
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
	#style edges
	zp1 <-zp1 + guides(color = guide_legend(override.aes = list (size = 3 ),title.position="top"))
	# zp1 <-zp1 + scale_color_manual(values=edge.color,guide = guide_legend(legend.position ="top",direction="horizontal", nrow= 1))
	zp1 <- zp1 + new_theme_empty + labs(color='Edge Type')	  # theme
	if(is.null(file)){
		print(zp1)
	} else {	
	
	# create svg "panzoom_ggplot2.html"
	# with zooming and panning (based on: http://www.r-bloggers.com/ggplot2-meet-d3/)
	#--------------------------------
	
	#define a simple html head template
		htmlhead <- 
		'<!DOCTYPE html>
		<head>
		  <meta charset = "utf-8">
		  <script src = "http://d3js.org/d3.v3.js"></script>
		</head>

		<body>
		'
		print(zp1)
		#use gridSVG to export our plot to SVG
		mysvg <- grid.export("panzoom1.svg")


		#define a simple pan zoom script using d3  #gridSVG
		panzoomScript <-
		'  
		<script>
			var svg = d3.selectAll("#gridSVG");
			svg.call(d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoom))
			
			function zoom() {
			  svg.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
			} 
		  </script>
		</body>
		'

		#combine all the pieces into an html file
		# sink(paste0(file,".html"))
		return(cat(htmlhead,saveXML(mysvg$svg),panzoomScript))# create html with cat
		# close our file
		# sink(file=NULL)
		
		# return(c(htmlhead,saveXML(mysvg$svg),panzoomScript)) # return as well
		
	}	
}

#debugging  print all names and values in input
output$debug<- renderPrint({
	if(!is.initialized()) return()
	if(!input$metabomapr=="Debug") return()
	
	obj<-names(input)
	input.obj<-lapply(1:length(obj), function(i) { input[[obj[i]]]})
	names(input.obj)<-obj
	obj<-names(values)
	values.obj<-lapply(1:length(obj), function(i) { values[[obj[i]]]})
	names(values.obj)<-obj
	
	return(list(input = input.obj,values = values.obj))
})

getdata <- function(dataset = input$datasets) {
  tryCatch(values[[dataset]],error=function(e) {NULL})
  # values[[dataset]]
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
	# datasets <<- unique(c(robjname,datasets))
	values$datasets <<- unique(c(robjname,datasets))
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
		# datasets <<- c(robjname)
		values$datasets <<- c(robjname)
	} else {
		# datasets <<- unique(c(robjname,datasets))
		values$datasets <<- unique(c(robjname,datasets))
	}
}

#reactive datasets also disable use in output$datasets
r.datasets<-reactive({
values$datasets
# tmp<-values$datasets
# if(is.null(tmp)){tmp<-""}
# return(tmp)

})

#################################################
# reactive from radiant
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
	# selectInput(inputId = "datasets", label = "Select:", choices = datasets, selected = datasets[1], multiple = FALSE)
	# selectInput(inputId = "datasets", label = "Select:", choices = values$datasets, selected = values$datasets[1], multiple = FALSE)
	selectInput(inputId = "datasets", label = "Select:", choices = r.datasets(), selected = r.datasets()[1], multiple = FALSE)
})

output$view_data <-renderTable({
	data.frame(getdata())
})

#choose network inddex from the data and specify its type for carrying out translations
varnames <- function() {
	if(is.null(input$datasets)) return()
	colnames(getdata())
}

#updating the data
changedata <- function(addCol = list(NULL), addColName = "") {
	# change data as specified
	if(addColName[1] == "") return()
  # isolate ensures no reactive dependencies are used
  # isolate({
  	if(length(addCol) == 1 && is.null(addCol[[1]])) {
  		return(values[[input$datasets]][,addColName] <- addCol)
  	} else if(nrow(getdata()) == nrow(addCol)) {
	  	return(values[[input$datasets]][,addColName] <- addCol)
  	} 
  	# else {
	  # 	return(values[[input$datasets]][,addColName] <- addCol)
	  # }
  # })
}

#################################################
# objects for 'Translations'
#################################################
translation.options<-function(){
	values$CTS.options
}

output$data_translation_options<-renderUI({
	wellPanel(	
	checkboxInput(inputId = "CTS_translate", label = "",value=FALSE),
	 h4('Translate'),
	 conditionalPanel(condition = "input.CTS_translate",
		selectInput(inputId = "CTS_translate_id", label = "Translate:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		selectInput(inputId = "CTS_translate_from", label = "From:", choices = translation.options(), selected = translation.options()[1], multiple = FALSE),
		selectInput(inputId = "CTS_translate_to", label = "To:", choices = translation.options(), selected = translation.options()[2], multiple = TRUE),
		br(),
		actionButton("CTS_calculate", "Calculate"),
		HTML('<script type="text/javascript">
        $(document).ready(function() {
          $("#CTS_calculate").click(function() { 
            $("#view_data").text("Translating...please wait.");
          });
        });
      </script>
		')
		)
	)	
})

#CTS translation function
CTS_calculate_translations<-reactive({
	if(is.null(input$CTS_calculate)||input$CTS_translate==FALSE) return()
	if(input$CTS_calculate == 0) return()
	#this gets evaluated after a translation is completed and can erro thus condition on this working
	id<-tryCatch(as.character(unlist(getdata()[,input$CTS_translate_id])),error=function(e){"ERRROR"})	# need to replace empty else bad output from CTS
	if(all(id=="ERROR")){return()}
	id[id==""]<-"NA"
	multi.CTSgetR(id, input$CTS_translate_from, input$CTS_translate_to)
})

#observer to carry out translations
observe({
	# if (input$CTS_calculate == 0) return()
	# if(input$CTS_translate==FALSE) return()
	# isolate({
	# values$BB<-CTS_calculate_translations()#as.list(CTS_id(), CTS_from(),CTS_to())
		values$CTS_translated_values<-CTS_calculate_translations()
		isolate({
		if(!is.null(values$CTS_translated_values)){
		#trying to avoid a double calculation triggered by data update from first completed calculation

			data.name<-paste0("translated_",input$datasets)
			tmp<-values$CTS_translated_values[,-1,drop=FALSE]
			colnames(tmp)<-paste0(input$CTS_translate_from,"_to_",input$CTS_translate_to)
			# if(length(agrep(paste0("translated_",data.name),r.datasets())==0)){ return()}# no clue, but get double binding of newly translated data
				# values[[data.name]]<-cbind(tmp,getdata()) # ****
				#remove old version
				# values$datasets<-values$datasets[-agrep(paste0("translated_",data.name),values$datasets,max = list(sub = 0))]
				# data.name<-paste0("translated_",input$datasets)
				# values$datasets <-unique(c(r.datasets(),data.name)) # ****
				# datasets <-unique(c(datasets,data.name))
			#try adding to existing data
			# values[[input$datasets]]<-cbind(tmp,getdata())
			
			#add to existing data (still recalculates)
			for(i in 1:ncol(tmp)){	
				changedata(tmp[,i,drop=FALSE], colnames(tmp)[i])
			}
			# values[["X"]]<-cbind(tmp,getdata())		
			}
		# }
	})
})

#################################################
# objects for 'Network'
#################################################
#possible names for nodes
Nodenames <- function() {
	if(is.null(input$datasets)) x<-NULL else x<-colnames(getdata())
	if(is.null(values$node.attributes)) y<-NULL else y<-colnames(values$node.attributes)
	res<-c(x,y)
	if(is.null(res)) return (NULL) else return(res)
}

#possible names for nodes
Nodeobjects <- function() {
	if(is.null(input$datasets)) x<-NULL else x<-getdata()
	if(is.null(values$node.attributes)) y<-NULL else y<-values$node.attributes
	res<-cbind(x,y)
	if(is.null(res)) return (NULL) else return(res)
}

#get network node names
get_node_names<-reactive({
	if(is.null(input$node_names)) return("")
	data<-getdata()
	names<-tryCatch(data[,colnames(data)%in%input$node_names],error=function(e){1:nrow(data)})
	return(fixlc(names))
})

#names of databas identifiers
DB.names <- function() {
	if(is.null(input$datasets)) return()
	list("Chemical Name" = "name", "KEGG" = "kegg", "PubChem CID" = "pubchemCID", "BioCyc" =  "biocyc" ,"InChiKey" = "inchikey") #hmdb = "HMDB"
}

#mass spect encoding types
MZ.encode<-function(){
	list("m/z : intensity" = "mz_int")
}


#------------------------------------------
# MAIN functions for edge list calculations
# translate index and calculate edges # remove translation option
# splitting into separate modules below into seperate
#------------------------------------------

#BIOCHEMICAL CONNETIONS
calculate_kegg_edgelist<-reactive({

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
			
		if(is.null(values$reaction.DB)){
			values$reaction.DB<-get.KEGG.pairs(type="main")
		}
			
		#carry out translations
		kegg.id<-index
		kegg.edges<-tryCatch(get.Reaction.pairs(kegg.id,values$reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE),error=function(e){NULL})
		
		#create shared index between different edge ids
		index<-kegg.id
		if(!is.null(kegg.edges)){
			edge.names<-data.frame(index, network.id = c(1:length(index)))
			kegg.edges<-make.edge.list.index(edge.names,kegg.edges)
			kegg.edges<-data.frame(as.matrix(kegg.edges),type = "KEGG", weight = 2)
			values$edgelist.error.message$kegg<-""
		} else {
			values$edgelist.error.message$kegg<-"Could not find any matches to the supplied KEGG identifier(s).\n"
			kegg.edges<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL) # to help bind later
		}
		
		#store objects for later filter
		values$tmp.edge.list$kegg<-kegg.edges
		values$tmp.node.info$biochemical.edge.index<-index
	} else {
		#store objects for later filter
		values$tmp.edge.list$kegg<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL)
		values$tmp.node.info$biochemical.edge.index<-NULL
	}	
	
	values$bio_edge_state<-list(data=input$datasets,variable=input$network_index_chem)
})

#STRUCTURAL SIMILARITY
calculate_tanimoto_edgelist<-reactive({

	#calculate tanimoto similarity between molecular fingerprints
	if(input$chem_edges){
		index<-getdata()[,input$network_index_chem]
		index.type<-switch(input$network_index_type_chem,
					kegg 	=	"KEGG",
					pubchemCID = "PubChem CID",
					name = "Chemical Name",
					biocyc = "BioCyc",
					inchikey = "InChiKey"
				)
	
		# get tanimoto similarity
		CID.id<-index
		#load local DB for SDF files from cid
		DB<-tryCatch(get(load("data/CID.SDF.DB")[1]),error=function(e) {NULL})
		tani.edges<-tryCatch(CID.to.tanimoto(cids=fixlc(CID.id),DB=DB,save.as="data/CID.SDF.DB"),error=function(e){NULL}) # cut.off = input$tanimoto_cutoff
		
		
		#create shared index between different edge ids
		index<-CID.id
		if(!is.null(tani.edges)){
			edge.names<-data.frame(index, network.id = c(1:length(index)))
			tani.edges[,1:2]<-make.edge.list.index(edge.names,tani.edges)
			tani.edges<-data.frame(as.matrix(tani.edges[,1:2]),type = "Tanimoto", weight = tani.edges[,3,])
			values$edgelist.error.message$chem<-NULL
		}	else {
			values$edgelist.error.message$chem<-"Could not find any matches to the supplied PubChem identifier(s).\n"
			tani.edges<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL) # to help bind later
		}
		
		#store temporary full edge.list objects
		values$tmp.edge.list$tanimoto<-tani.edges
		values$tmp.node.info$chemical.edge.index<-index
	} else {
		#store objects for later filter
		values$tmp.edge.list$tanimoto<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL)
		values$tmp.node.info$chemical.edge.index<-NULL
	}
	
	values$chem_edge_state<-list(data=input$datasets,variable=input$network_index_chem)
	
})

#MASS SPECTRAL SIMILARITY
calculate_mz_edgelist<-reactive({
	
	#TODO decouple correlation calculation from
	# from filtering on retention time and strength
	if(input$spec_edges){ #use 1 or 0 encoding to limit connections from known=1 to unknown = 0 (no 0-0)
		index<-getdata()[,input$network_index_spec]
		known<-input$network_spec_primary_nodes
		if(!known == "0"){known<-getdata()[,known]} # long story bro
	
		if(input$network_spec_retention_index=="0"){retention.index<-NULL} else {retention.index<-getdata()[,input$network_spec_retention_index]}
		spec.edges<-tryCatch(get.spectral.edge.list(spectra = index, known = known, 
							cutoff = input$spec_cutoff, edge.limit = input$network_spec_nodes_max,
							retention.index=retention.index,retention.cutoff=input$network_spec_retention_index_cutoff), error=function(e){NULL})
		
		
		#create shared index between different edge ids
		if(!is.null(spec.edges)){
			# edge.names<-data.frame(index, network.id = c(1:length(index))) # done internally to the function which should be ok as long ass all objects are submitted 
			# spec.edges[,1:2]<-make.edge.list.index(edge.names,spec.edges)
			spec.edges<-data.frame(as.matrix(spec.edges[,1:2]),weight = spec.edges[,3,],type = "mass spectral" )
			values$edgelist.error.message$mz<-NULL
		}	else {
			values$edgelist.error.message$mz<-"Error calculating mass spectral similarities.\n"
			spec.edges<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL) # to help bind later
		}
		
		#store temporary full edge.list objects
		values$tmp.edge.list$mz<-spec.edges
		values$tmp.node.info$mass.spectral.edge.index<-index
				
	} else {
		#store objects for later filter
		values$tmp.edge.list$mz<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL)
		values$tmp.node.info$mass.spectral.edge.index<-NULL
	}
	
	values$mz_edge_state<-list(data=input$datasets,variable=input$network_index_spec,known = input$network_spec_primary_nodes, 
							cutoff = input$spec_cutoff, edge.limit = input$network_spec_nodes_max,
							retention.index=input$network_spec_retention_index,retention.cutoff=input$network_spec_retention_index_cutoff)
})

#CORRELATION
calculate_cor_edgelist<-reactive({

	#calculate correlation
	if(input$cor_edges){
		data<-getdata()[,input$network_index_cor]
		tmp.data<-t(data) # flip to calculate correlations between variables and not samples
		#could do samples and use MDS layout
		colnames(tmp.data)<-1:nrow(data)
		cor.edges<-tryCatch(devium.calculate.correlations(tmp.data,type=input$network_index_type_cor, results = "edge list"), error=function(e){NULL})            
		
		#create shared index between different edge ids
		index<-1:nrow(data)
		if(!is.null(cor.edges)){
			#format for output
			#FDR adjust
			adj.p<-p.adjust(fixln(cor.edges[,4]), method="BH")#FDR.adjust(obj = fixln(cor.edges[,4]),type="pvalue")
			adj.p[is.na(as.numeric(adj.p))]<- 0 # error vars, assume due cor =1
			cor.edges$fdr.p.value<-adj.p
			
			#create edge type
			type<-rep(paste("positive correlation"),nrow(cor.edges)) #,input$network_index_type_cor
			type[fixln(cor.edges[,3])<=0]<-paste("negative correlation")
			cor.edges<-data.frame(as.matrix(cor.edges[,1:2]),type = type, weight = abs(fixln(cor.edges[,3])),p.values=fixln(cor.edges[,4]),fdr.p.values=fixln(cor.edges$fdr.p.value))

			values$edgelist.error.message$cor<-NULL
		}	else {
			values$edgelist.error.message$cor<-"Error in correlation calculation.\n"
			cor.edges<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL) # to help bind later
		}
		
		#store temporary full edge.list objects
		values$tmp.edge.list$cor.edges<-cor.edges
		values$tmp.node.info$correlation.edge.index<-index
	} else {
		#store objects for later filter
		values$tmp.edge.list$cor.edges<-NULL#data.frame(source=NULL,target=NULL,type=NULL,weight=NULL)
		values$tmp.node.info$correlation.edge.index<-NULL
	}
	
	values$cor_edge_state<-list(data=input$datasets,variable=input$network_index_cor,method=input$network_index_type_cor)
	
})

#------------------------------------------
final_edge_list_calculate<-reactive({
	
	# print("FINAL EDGE LIST TRIGGERED")	
	# isolate({ # not sure if required
		# calculate_edgelist()
	# })
	
	if(is.null(values$tmp.edge.list)) return()
	tmp<-values$tmp.edge.list
	names(tmp)<-names(values$tmp.edge.list)
	
	
	#filter chemical similarity
	if(!is.null(tmp$tanimoto)){
		obj<-data.frame(tmp$tanimoto)
		tmp$tanimoto<-obj[fixln(obj$weight)>=input$tanimoto_cutoff,]
	}
	
	#correlations
	if(!is.null(tmp$cor.edges)){
		
		if(input$cor_edges_fdr) filter<-tmp$cor.edges$fdr.p.values else filter<-tmp$cor.edges$p.values
		tmp$cor.edges<-tmp$cor.edges[filter <= input$cor_cutoff,c("source","target","type","weight")]
		cor.levels<-fixlc(unique(tmp$cor.edges$type))
	}
	
	res<-data.frame(do.call("rbind",tmp))
	
	#remove duplicated edges based on
	if(input$unique_edges) {
		#hierarchy for removing duplicates (add ability to change later)
		ord<-NULL
		if(!is.null(tmp$kegg)) ord<-c(ord,"KEGG")
		if(!is.null(tmp$tanimoto)) ord<-c(ord,"Tanimoto")
		if(!is.null(tmp$cor.edges)) ord<-c(ord,cor.levels) 
		if(!is.null(tmp$mz)) ord<-c(ord,"mass spectral")
		
		type<-factor(res$type,levels=ord,ordered=TRUE)
		res$type<-type
		
		res<-clean.edgeList(data=res)
	}
	
	#use for ggplot to IDs
	res$type<-factor(res$type) # could maintain order based on unique edges
	values$edge.list_for.network<-res # need to fix but translations mess network plotter up (should be node names any way so fix by using two objects)
	
	#optionally translate edge ids to a supplied index
	if(!input$translate_edge_index=="none"){
		tmp.id<-getdata()[,input$translate_edge_index]
		trans.s<-translate.index(fixlc(res[,1]), lookup=cbind(1:nrow(getdata()),fixlc(tmp.id)))
		trans.t<-translate.index(fixlc(res[,2]), lookup=cbind(1:nrow(getdata()),fixlc(tmp.id)))
		res$source<-as.numeric(trans.s)
		res$target<-as.numeric(trans.t)
		node.attr$network.index<-tmp.id
	} 
	
	values$edge.list<-res	
	values$node.attributes<-data.frame(do.call("cbind",lapply(values$tmp.node.info,fixlc)))
	colnames(values$node.attributes)<-names(values$tmp.node.info)
})

#format inputs for d3Network
#------------------------------------------
get.d3.Network<- reactive ({
			if(is.null(values$edge.list)) return()
			
			edge.list<-values$edge.list
			
			#need to re-encode edge list starting with 0 for d3
			
			# edge.list<-clean.edgeList(data=edge.list)
			# #need to translate index to include 0 and go in a decreasing order
			edge.list[,1]<-fixln(edge.list[,1])-1
			edge.list[,2]<-fixln(edge.list[,2])-1
			edge.list[,"weight"]<-tryCatch(rescale(fixln(edge.list[,"weight"]),c(10,input$network_plot_edge_size*10)),error=function(e){rep(input$network_plot_edge_size*10,length(edge.list[,"weight"]))})
			# edge.list<-edge.list[order(edge.list[,1],edge.list[,2]),]
			# #node info
			node.info<-data.frame(name=get_node_names())
			
			#add node legend
			#safe ids to use for nodes
			.types<-unique(fixlc(edge.list[,"type"]))
			ntypes<-length(.types)
			l.source<-max(unlist(edge.list[,1:2]))+1
			legend<-data.frame(source=rep(l.source,ntypes),target=c((l.source+1):(l.source+ntypes)),type=.types,weight=rep(max(edge.list[,"weight"]),ntypes))
			# #need to insert legend entry into l.source position of node info
			# tmp1<-as.matrix(node.info)[1:(l.source-1),,drop=FALSE]
			# tmp2<-as.matrix(node.info)[(l.source+1):nrow(node.info),,drop=FALSE]
			
			#update degle list and node info to include legend
			# node.info2<-data.frame(rbind(as.matrix(node.info),as.matrix(data.frame(name=c("LEGEND",.types)))))#data.frame(rbind(tmp1,as.matrix(data.frame(name="LEGEND")),tmp2,as.matrix(data.frame(name=.types))))
			# edge.list2<-data.frame(rbind(as.matrix(edge.list),as.matrix(legend)))
			# rownames(edge.list2)<-1:nrow(edge.list2)
			
			#need to convert to a format d3 will accept
			# clean<-clean.edgeList(data=edge.list2,source="source",target="target",type=NULL)
			# trans.t<-translate.index(fixlc(res[,2]), lookup=cbind(1:nrow(getdata()),fixlc(tmp.id)))
			# values$new.items<-NULL
			# values$new.items<-list(clean,node.info2,edge.list2)
			
			# #translate source target ids to node names for simple d3network
			# tmp<-fixln(edge.list[,1:2])
			# lookup<-data.frame(names=get_node_names())
			# rownames(lookup)<-fixlc(0:(nrow(lookup)-1))
			
			# simple.edge.list<-edge.list

			# # values$Fuck.edges<-edge.list
			# source<-lookup[fixlc(edge.list[,1]),]
			# target<-lookup[fixlc(edge.list[,2]),]
			# simple.edge.list<-data.frame(source,target)	
			# # values$simple.edge.list<-simple.edge.list
			# # simple.edge.list<-simple.edge.list[order(simple.edge.list$source),]
			# # values$simple.edge.list<-simple.edge.list
			
			# #names are ordered according to the source column
			# #could show groups based on clustering or not
			node.info$group<-1#sample(1:10,nrow(node.info),replace = TRUE)

			return(list(edge.list=edge.list,node.info=node.info))
})

# #reset inputs if no options are selected
state.check<-reactive({
	#all empty
	if(sum(c(input$bio_edges, input$chem_edges, input$spec_edges, input$cor_edges))==0){
		values$edge.list<-NULL
		values$node.attributes<-NULL
		values$edge.list_for.network<-NULL  #used for ggplot2 # no translation}
		#temporary objects used for filtering
		values$tmp.edge.list<-NULL
		values$tmp.node.info<-NULL
		values$edgelist.error.message<-NULL
	} 
	
	
})

#check initialization state
is.initialized<-reactive({
	check<-list(input$bio_edges, input$chem_edges, input$spec_edges, input$cor_edges)
	res<-sapply(check,is.null)
	if(any(res)) return(FALSE)  else return(TRUE)
})
	
#trigger state reset if no options are set
observe({
	if(is.initialized()) state.check()
})

#control state for each module
observe({
	# if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	if(!is.initialized()) return()
	if(!input$bio_edges) {
		values$tmp.edge.list$kegg<-NULL
		values$bio_edge_state<-NULL	
	}
	if(!input$chem_edges) {
		values$tmp.edge.list$tanimoto<-NULL
		values$chem_edge_state<-NULL	
	}
	if(!input$spec_edges) {
		values$tmp.edge.list$mz<-NULL
		values$mz_edge_state<-NULL	
	}
	if(!input$cor_edges) {
		values$tmp.edge.list$cor<-NULL
		values$cor_edge_state<-NULL	
	}
})

#trigger calculation from action button
observe({

	if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	isolate({

		if(bio_edge_watcher()){
			calculate_kegg_edgelist()
		}
		if(chem_edge_watcher()){
			calculate_tanimoto_edgelist()
		}
		if(cor_edge_watcher()){
			calculate_cor_edgelist()
		}
		if(mz_edge_watcher()){
			calculate_mz_edgelist()
		}
		
		final_edge_list_calculate() # fast filter of stored results
	})
})

#individual actionbuttons not working to trigger partial calculations
#implement local state watchers
#tanimoto
chem_edge_watcher<-reactive({
	if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	cur.state<-list(data=input$datasets,variable=input$network_index_chem)
	if(identical(cur.state,values$chem_edge_state)) return(FALSE) else return(TRUE)
})

#KEGG
bio_edge_watcher<-reactive({
	if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	cur.state<-list(data=input$datasets,variable=input$network_index_bio)
	if(identical(cur.state,values$bio_edge_state)) return(FALSE) else return(TRUE)
})

#mz
mz_edge_watcher<-reactive({
	if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	cur.state<-list(data=input$datasets,variable=input$network_index_spec,known = input$network_spec_primary_nodes, 
							cutoff = input$spec_cutoff, edge.limit = input$network_spec_nodes_max,
							retention.index=input$network_spec_retention_index,retention.cutoff=input$network_spec_retention_index_cutoff)
	if(identical(cur.state,values$mz_edge_state)) return(FALSE) else return(TRUE)					
})

#correlation
cor_edge_watcher<-reactive({
	if(input$create_edgelist==0&input$create_edgelist_network == 0) return()
	cur.state<-list(data=input$datasets,variable=input$network_index_cor,method=input$network_index_type_cor)
	if(identical(cur.state,values$cor_edge_state)) return(FALSE) else return(TRUE)
})

#function to rencode index
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

#upload data
output$network_data_upload<-renderUI({
		
	wellPanel(
		 checkboxInput(inputId = "upload_data_object", label = "",value=FALSE),
		 h4('Upload'),
		 conditionalPanel(condition = "input.upload_data_object",
			 withTags(div(class='row-fluid',
							 div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
							 div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE)))
							 ),
			HTML("<label>Load data: (.csv)</label>"),
			uiOutput("upload_local_server"),
			HTML("<label>Paste data:</label>"),
			tags$textarea(id="copyAndPaste", rows=3, cols=10, ""),
			tags$style(type="text/css", "#copyAndPaste     { width:200px;}")
		)		
	)	
		
})

#manage data
output$network_data_manage<-renderUI({	
	wellPanel(
		 checkboxInput(inputId = "manage_data_object", label = "",value=FALSE),
		 h4('Manage'),
		 conditionalPanel(condition = "input.manage_data_object",
			selectInput(inputId = "manage_datasets", label = "Dataset:", choices = c("----"="none",r.datasets()), multiple = TRUE),
			br(),
			actionButton("remove_network_dataset", "Remove Data Set")
		)		
	)		
})


#watcher for data manage
# from Radiant
observe({
  if(is.null(input$remove_network_dataset) || input$remove_network_dataset == 0||input$manage_datasets=="none") return()
  isolate({
    for(i in 1:length(input$manage_datasets)) {
      values[[input$manage_datasets[i]]] <- NULL
    }
    # datasets <<- datasets[-which(datasets == input$datasets)]
    datasets <- values[['datasets']]
    if(length(datasets) == length(input$removeDataset)) {
      datasets <- ""
    } else {
      # datasets <- datasets[-which(datasets == input$removeDataset)]
      datasets <- datasets[-which(datasets %in% input$manage_datasets)]
    }

    values[['datasets']] <- datasets
  })
})

#biochemical connections args
output$network_index_info_bio<-renderUI({
		
	wellPanel(
	 # withTags(div(class='row',
	 # div(class='span', checkboxInput(inputId = "bio_edges", label = "",value=FALSE)),
	 # div(class='span', h4('Biochemical')))),
	 checkboxInput(inputId = "bio_edges", label = "",value=FALSE),
	 h4('Biochemical Reactions'),
	 # tags$style(type='text/css', "#bio_edges { font-weight: bold; font-size:16px;}"),
		conditionalPanel(condition = "input.bio_edges",
		selectInput(inputId = "network_index_type_bio", label = "Database:", choices = list("KEGG" = "kegg" ), selected = "KEGG", multiple = FALSE),
			selectInput(inputId = "network_index_bio", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE)
			# selectInput(inputId = "network_index_type_bio", label = "Index type:", choices = DB.names(), selected = DB.names()[2], multiple = FALSE)
		
		)	
	)
		
})
	
#chemical similarity args
output$network_index_info_chem<-renderUI({
	wellPanel(
	checkboxInput(inputId = "chem_edges", label = "",value=FALSE),
	h4('Structural Similarity'),
		conditionalPanel(condition = "input.chem_edges",
		selectInput(inputId = "network_index_type_chem", label = "Database:", choices = list("PubChem CID" = "pubchemCID" ), selected = "PubChem CID", multiple = FALSE),
		selectInput(inputId = "network_index_chem", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		# selectInput(inputId = "network_index_type_chem", label = "Index type:", choices = DB.names(), selected = DB.names()[3], multiple = FALSE),
		numericInput(inputId = "tanimoto_cutoff" , "Cutoff:", value = 0.7, min = 0, max = 1, step = .005)
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
		numericInput(inputId = "network_spec_nodes_max", "Maximum connections:", min = 1, max = 1000, value = 5, step = 1), # need to dynamixcally update max, or make it big for now?
		numericInput(inputId = "spec_cutoff" , "cutoff:", value = 0.7, min = 0, max = 1, step = .005),
		selectInput(inputId = "network_spec_retention_index", label = "Retention time filter:", choices = c("none" = 0,varnames()), selected = "none", multiple = FALSE),
		numericInput(inputId = "network_spec_retention_index_cutoff" , "Delta cutoff:", value = 10000, min = 0, step = 500)
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

#node names
output$node_names<-renderUI({
selectInput(inputId = "node_names", label ="Names", choices = Nodenames(), selected = varnames()[1], multiple = FALSE)
})

#translate edge ids to a supplied index
output$translate_edge_index<-renderUI({
	selectInput(inputId = "translate_edge_index", label = "Edge index:", choices = c("row number"="none",varnames()), selected = "row number", multiple = FALSE)
})

#plot dimensions
plot_height<-function(){
	height <- try(input$plot_output_height)
	if(is(height, 'try-error') || is.null(height)) {
		height<-400
	} 
	return(height)
}

plot_width<-function(){
	width <- try(input$plot_output_width)
	if(is(width, 'try-error') || is.null(width)) {
		width<-400
	} 
	return(width)
}


#Generate ggplot2 network
#-----------------------------
#as PNG
output$network <- renderPlot({
	
	if (input$create_edgelist_network == 0) return()#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))
	if(!any(input$network_plot_type%in%"PNG")) return()#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))
		
		#calculate edges and plot 		
	isolate({
		# calculate_edgelist()
		# final_edge_list_calculate()
		# if(!input$metabomapr == "Network") return()
		if(is.null(values$edge.list_for.network)) { 
			return()#plot(x = 1, type = 'n', main="Please calculate edge list first.", axes = FALSE, xlab = "", ylab = "")
		} else if(length(values$edge.list) == 0) { 
			return()#plot(x = 1, type = 'n', main="No connections fit set criteria.", axes = FALSE, xlab = "", ylab = "")
		} 
		
		if(any(input$network_plot_type %in% "PNG")){
		
			edge.list<-values$edge.list_for.network
			#trying to avoid strange errors with factors
			names<-colnames(edge.list)[1:2]
			edge.list[,1]<-fixln(edge.list[,1,drop=FALSE])
			edge.list[,2]<-fixln(edge.list[,2,drop=FALSE])
			colnames(edge.list)[1:2]<-names
			
			#use a constant edge coloring scheme
			color.pal<-rainbow(5)
			.type<-c('KEGG','Tanimoto','positive correlation','negative correlation','mass spectral')
			col.opts<-data.frame(color = color.pal)
			rownames(col.opts)<-.type
			cur.types<-fixlc(levels(edge.list$type)) # current edge types
			edge.color<-fixlc(col.opts[cur.types,,drop=FALSE][cur.types,])
			#need to reorder color to the actual order of the edge types
			
			
			#not sure why color order doesn't respect the order factor type?
			ggplot2.network(edge.list, edge.color.var = "type", edge.color = edge.color, directed = FALSE,
						node.color = NULL, show.names = input$network_plot_show_name, node.names=input$node_names,
						bezier = input$network_plot_bezier, node.size = input$network_plot_node_size, 
						node.label.size = input$network_plot_name_size, max.edge.thickness = input$network_plot_edge_size)				
						
		} 
		})
	},  height = plot_height,width =plot_width )	

#ggplot as SVG
output$SVGnetwork<-renderPrint({
	#create empty html placeholder
	emptyHTML<-function(){
		htmlhead <- 
			'<!DOCTYPE html>
			<head>
			  <meta charset = "utf-8">
			
			</head>

			<body>
			'
		# sink(paste0(file,".html"))
		cat(htmlhead)
		#close our file
		# sink(file=NULL)
	}
	
	if (input$create_edgelist_network == 0) return(emptyHTML())#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))
	if(!any(input$network_plot_type%in%"SVG")) return(emptyHTML())#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))
		
		#calculate edges and plot 		
	isolate({
		# calculate_edgelist()
		# final_edge_list_calculate()
		# if(!input$metabomapr == "Network") return()
		if(is.null(values$edge.list_for.network)) { 
			return(emptyHTML())
		} else if(length(values$edge.list) == 0) { 
			return(emptyHTML())
		} 
		
		if(any(input$network_plot_type %in% "SVG")){
		
			edge.list<-values$edge.list_for.network
			#trying to avoid strange errors with factors
			names<-colnames(edge.list)[1:2]
			edge.list[,1]<-fixln(edge.list[,1,drop=FALSE])
			edge.list[,2]<-fixln(edge.list[,2,drop=FALSE])
			colnames(edge.list)[1:2]<-names
			
			#use a constant edge coloring scheme
			color.pal<-rainbow(5)
			.type<-c('KEGG','Tanimoto','mz','positive correlation','negative correlation')
			col.opts<-data.frame(color = color.pal)
			rownames(col.opts)<-.type
			cur.types<-fixlc(levels(edge.list$type)) # current edge types
			edge.color<-fixlc(col.opts[cur.types,,drop=FALSE][cur.types,])
			#need to reorder color to the actual order of the edge types
			
			
			#create svg of network
			ggplot2.network(edge.list, edge.color.var = "type", edge.color = edge.color, directed = FALSE,
				node.color = NULL, show.names = input$network_plot_show_name, node.names=input$node_names,
				bezier = FALSE, node.size = input$network_plot_node_size, #bezier = input$network_plot_bezier
				node.label.size = input$network_plot_name_size, max.edge.thickness = input$network_plot_edge_size,file="SVGnetwork")	
		} 	
						
	})	
})


# #trying to get dynamic html
# getPage<-function() {
      # return(includeHTML("SVGnetwork.html"))
  # }
  
# output$inc<-renderUI({
# #trigger
# if (input$create_edgelist_network == 0) return(emptyHTML(file="SVGnetwork"))#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))
	# if(!any(input$network_plot_type%in%"static")) return(emptyHTML(file="SVGnetwork"))#return(plot(x = 1, type = 'n', main="", axes = FALSE, xlab = "", ylab = ""))

		# if(any(input$network_plot_type %in% "static")){
	# getPage()
	# }

# })

# PLOTTING FUNCTIONS
#-----------------------------
# see http://bl.ocks.org/nsonnad/5982652 for colored links
output$networkPlot<-renderPrint({
		if (input$create_edgelist_network == 0) 
		return(h4("Please select options and draw the network."))
	
		#calculate edges and plot 		
	isolate({
		
		# if(!input$metabomapr == "Network") return()
		if(length(values$edge.list) == 0) { 
			return(h4("No connections fit set criteria."))
		} 
		if(any(input$network_plot_type %in% "interactive")) {
			# h4("D3 network goes here!")
			# if(input$network_plot_show_name){
			
				# #need to translate numeric edge.list to node names
				
			
				# d3SimpleNetwork(get.d3.Network()$simple.edge.list,	height=plot_height(),width=plot_width(),
					   # opacity = 0.9,standAlone = FALSE,fontsize = 12,
								# parentElement = '#networkPlot')	
			# } else {	
				# ##test to print file
				# d3ForceNetwork2(Links = get.d3.Network()$edge.list, Nodes=get.d3.Network()$node.info, Source = "source", Target = "target", Value = "weight",Type="type",
				   # NodeID = "name", Group = "group",file="test.html",
				   # height=plot_height(),width=plot_width(),
				   # opacity = input$network_plot_edge_alpha,nodeColour = "#d3d3d3", 
				   # nodeClickColour = "#000000", textColour = "#000000",
				   # nodeopacity=input$network_plot_node_alpha,legend="d3label.html")   
				 
				d3ForceNetwork2(Links = get.d3.Network()$edge.list, Nodes=get.d3.Network()$node.info,
				   Source = "source", Target = "target", Value = "weight",Type="type",
				   fontsize = input$network_plot_name_size*2, linkDistance = input$network_plot_edge_length,
				   NodeID = "name", Group = "group",height=plot_height(),width=plot_width(),
				   opacity = input$network_plot_edge_alpha,standAlone = FALSE, nodeColour = "#d3d3d3", 
				   nodeClickColour = "#000000", textColour = "#000000",nodeopacity=input$network_plot_node_alpha,
				   parentElement = '#networkPlot',legend="d3label.html") # legend is actually in www folder
	
		}	else { return(h5(""))}
	})
})	

# output$networkPlot_legend<-renderUI({
	# if (input$create_edgelist_network == 0) return()
	# if(length(values$edge.list) == 0) return()
	# if(any(input$network_plot_type %in% "interactive")) {
		# includeHTML('www/d3label.html')
	# } else {return()}	
# })	


# output$networkPlot_legend<-renderPrint({ # can not get legend positioned or shown
	# if (input$create_edgelist_network == 0) return(h5(""))
	# if(length(values$edge.list) == 0) return(h5(""))
	# if(any(input$network_plot_type %in% "interactive")) {
		# # return(cat(readLines('www/d3label.html')))
		# h5("LEGEND hoes here")
		
	# } else {return(h5(""))}	
# })	
	
	
#network attributes table
output$node.attributes <- renderTable({
	
	res<-data.frame(values$node.attributes)
		rownames(res)<-NULL
		return(res)
		

	
})

# Generate output for the summary tab
# output$summary <- renderUI(function() {
output$edge_list <- renderTable({

	#calculate edges		
	# isolate({
		res<-data.frame(values$edge.list)
		rownames(res)<-NULL
		return(res)
	# })
	 
})


#error messages for edge list calculation
output$edgelist_error_message<-renderPrint({
	if(is.null(values$edgelist.error.message)) {message<-"" } else { message<-paste0(unique(values$edgelist.error.message),collapse="") }
	h5(message,style = "color:red")
})


#------------------------
#downloading objects
#------------------------
#edge list for download
currentEdgeList <- reactive({
	values$edge.list
})

# node attributes for download
currentNodeAttributes <- reactive({
	values$node.attributes
})

#data for download
currentDataObject <- reactive({
	values[[input$datasets]]
})

#name of data data
currentDataObjectName<-reactive({ # -function(){
	paste0(input$datasets,".csv")	
})

#download edgelist
output$downloadEdgeList <- downloadHandler(
    filename = function() { "edge list.csv" },
    content = function(file) {
      write.csv(currentEdgeList(), file,row.names=FALSE)
    } )
  
 #download node attributes
 output$downloadNodeAttributes<- downloadHandler(
    filename = function() { "node attributes.csv" },
    content = function(file) {
      write.csv(currentNodeAttributes(), file,row.names=FALSE)
    })
  
#download data
output$downloadDataObject<- downloadHandler(
filename = function(){paste0(input$datasets,".csv")},
content = function(file) {
  write.csv(currentDataObject(), file,row.names=FALSE)
})