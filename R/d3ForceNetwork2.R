test<-function(){
	setwd("C:\\Users\\D\\Dropbox\\Software\\MetaMapR")
	size<-100
	Links<-data.frame(source=0:(size-1),target=sample(1:size,size,replace=TRUE),type=sample(0:3, size, replace=T),weight=1:size)
	Nodes<-data.frame(ID=unique(unlist(Links[,1:2])))
	Nodes$group<-0
	d3ForceNetwork2(Links, Nodes, Source="source", Target="target", NodeID="ID", Group="group", 
                 Value = "weight", Type = "type",file="test.html",legend="www/d3label.html")  

}

#modification of d3ForceNetwork to show node ids and allow edge colors
d3ForceNetwork2 <- function(Links, Nodes, Source, Target, Value = NULL, Type = NULL, NodeID,
	Group, height = 600, width = 900, fontsize = 14, linkDistance = 50,
	linkWidth = "function(d) { return Math.sqrt(d.value); }", charge = -120,
	linkColour = "#666",opacity = 0.6, parentElement = "body",nodeopacity=1,
	standAlone = TRUE, file = NULL, iframe = FALSE,legend=NULL,
	d3Script = "http://d3js.org/d3.v3.min.js", nodeColour = "#3182bd",
	nodeClickColour = "#E34A33", textColour = "#3182bd")
{
	if (!isTRUE(standAlone) & isTRUE(iframe)){
		stop("If iframe = TRUE then standAlone must be TRUE.")
	}
	# If no file name is specified create random name to avoid conflicts
	if (is.null(file) & isTRUE(iframe)){
		Random <- paste0(sample(c(0:9, letters, LETTERS), 5, replace=TRUE),
						collapse = "")
		file <- paste0("NetworkGraph", Random, ".html")
	}

	# Create iframe dimensions larger than graph dimensions
	FrameHeight <- height + height * 0.07
	FrameWidth <- width + width * 0.03

	# Create click text size
	clickTextSize <- fontsize * 2.5

	# Subset data frames for network graph
	if (class(Links) != "data.frame"){
		stop("Links must be a data frame class object.")
	}
	if (class(Nodes) != "data.frame"){
		stop("Nodes must be a data frame class object.")
	}
  
	if (is.null(Value)){
		LinksDF <- data.frame(Links[, Source], Links[, Target])
		names(LinksDF) <- c("source", "target")
	}
	else if (!is.null(Value)){
		LinksDF <- data.frame(Links[, Source], Links[, Target], Links[, Value])
		names(LinksDF) <- c("source", "target", "value")
	}
  
  if(!is.null(Type)){
    LinksDF$type<-Links[,Type]    
  }
  
	NodesDF <- data.frame(Nodes[, NodeID], Nodes[, Group])
	names(NodesDF) <- c("name", "group")

	# Convert data frames to JSON format
	LinkData <- toJSONarray(LinksDF)
	LinkData <- paste("var links =", LinkData, "; \n")

	NodesData <- toJSONarray(NodesDF)
	NodesData <- paste("var nodes =", NodesData, "; \n")

	# Create webpage head
	PageHead <- BasicHead()

	# Create Style Sheet
	NetworkCSS <- whisker.render(ForceMainStyleSheet())

	# Main script for creating the graph
# 	#if (!isTRUE(zoom)){
# 		MainScript <- whisker.render(MainForceJS())
# 	}
# 	else if (isTRUE(zoom)){
		MainScript <- whisker.render(ForceZoomJS2())
# 	}

	if (is.null(file) & !isTRUE(standAlone)){
		if(!is.null(legend)){
			cat(NetworkCSS, LinkData, NodesData, MainScript, 
				paste0("<br><iframe src=\'",legend,"\' frameborder=\'0\' width=\'400\' height=\'400\'></iframe>"))
			} else {
				cat(NetworkCSS, LinkData, NodesData, MainScript)
			}
	}
	else if (is.null(file) & isTRUE(standAlone)){
		if(!is.null(legend)){
			cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript, 
				paste0("<br><iframe src=\'",legend,"\' frameborder=\'0\' width=\'400\' height=\'400\'></iframe>"),
				"</body>")
			} else {
				cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript,"</body>")
		}		
	}
	else if (!is.null(file) & !isTRUE(standAlone)){
		cat(NetworkCSS, LinkData, NodesData, MainScript, file = file)
	}

	else if (!is.null(file) & !isTRUE(iframe)){
    
	#show network legend in iframe
	if(!is.null(legend)){
		cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript, 
			paste0("<br><iframe src=\'",legend,"\' frameborder=\'0\' width=\'400\' height=\'400\'></iframe>"),
			"</body>", file = file)
		} else {
			cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript, "</body>", file = file)
		}	
		
	}

	else if (!is.null(file) & isTRUE(iframe)){
		cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript,
		    "</body>", file = file)
		cat("<iframe src=\'", file, "\'", " height=", FrameHeight, " width=",
			FrameWidth, "></iframe>", sep="")
	}


}

#for zooming add to below fxn
# var svg = d3.select(\"{{parentElement}}\").append(\"svg\")
# .attr(\"width\", width)
# .attr(\"height\", height)
# .attr(\"pointer-events\", \"all\")
# .call(d3.behavior.zoom().on(\"zoom\", redraw));


#javascript allowing node labels and colored edges
ForceZoomJS2 <- function(){
"var width = {{width}}
height = {{height}};

var color = d3.scale.category10();

var force = d3.layout.force()
.nodes(d3.values(nodes))
.links(links)
.size([width, height])
.linkDistance({{linkDistance}})
.charge({{charge}})
.on(\"tick\", tick)
.start();


var svg = d3.select(\"{{parentElement}}\").append(\"svg\")
.attr(\"width\", width)
.attr(\"height\", height);

var vis = svg
.append(\"svg:g\");

vis.append(\"svg:rect\")
.attr(\"width\", width)
.attr(\"height\", height)
.attr(\"fill\", 'white');

function redraw() {
vis.attr(\"transform\",
\"translate(\" + d3.event.translate + \")\"
+ \" scale(\" + d3.event.scale + \")\");
}

var link = vis.selectAll(\".link\")
.data(force.links())
.enter().append(\"line\")
.attr(\"class\", \"link\")
.style(\"stroke-width\", {{linkWidth}})
.style(\"stroke\", function(d) { 
  //set default edge color
  var edcol = \"#3182bd\" 
  
  //set edge color by type
  switch (d.type) {
      case \"KEGG\":
          edcol = \"#FF0000\";
          break;
      case \"Tanimoto\":
          edcol = \"#CCFF00\";
          break;
      case \"mass spectral\":
          edcol = \"#CC00FF\";
          break;
      case \"positive correlation\":
          edcol = \"#00FF66\";
          break;
	  case \"negative correlation\":
          edcol = \"#0066FF\";
          break;	  
  }
  return edcol;
})
;

var node = vis.selectAll(\".node\")
.data(force.nodes())
.enter().append(\"g\")
.attr(\"class\", \"node\")
.style(\"fill\", function(d) { return color(d.group); })
.style(\"opacity\", {{nodeopacity}})
.on(\"mouseover\", mouseover)
.on(\"mouseout\", mouseout)
.on(\"click\", click)
.on(\"dblclick\", dblclick)
.call(force.drag);

node.append(\"circle\")
.attr(\"r\", 8)
.style(\"fill\", \"{{nodeColour}}\"); //delete this line to color nodes by group

node.append(\"text\")
.attr(\"x\", 12)
.attr(\"dy\", \".35em\")
.style(\"fill\", \"{{textColour}}\")
.text(function(d) { return d.name });

function tick() {
link
.attr(\"x1\", function(d) { return d.source.x; })
.attr(\"y1\", function(d) { return d.source.y; })
.attr(\"x2\", function(d) { return d.target.x; })
.attr(\"y2\", function(d) { return d.target.y; });

node.attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"; });
}

function mouseover() {
d3.select(this).select(\"circle\").transition()
.duration(750)
.attr(\"r\", 16);

d3.select(this).select(\"text\").transition()
.duration(750)
.attr(\"x\", 22)
.style(\"stroke-width\", \".5px\")
.style(\"opacity\", 1)
.style(\"fill\", \"{{textColour}}\")
.style(\"font\", \"{{clickTextSize}}px serif\");
}

function mouseout() {
d3.select(this).select(\"circle\").transition()
.duration(750)
.attr(\"r\", 8);

d3.select(this).select(\"text\").transition()
.duration(750)
.attr(\"x\", 12)
.style(\"stroke\", \"none\")
.style(\"fill\", \"{{textColour}}\")
.style(\"stroke\", \"none\")
.style(\"opacity\", {{nodeopacity}})
.style(\"font\", \"{{fontsize}}px serif\");
}

// action to take on mouse click
function click() {
d3.select(this).select(\"text\").transition()
.duration(750)
.attr(\"x\", 22)
.style(\"stroke-width\", \".5px\")
.style(\"opacity\", 1)
.style(\"fill\", \"{{nodeClickColour}}\")
.style(\"font\", \"{{clickTextSize}}px serif\");

d3.select(this).select(\"circle\").transition()
.duration(750)
.style(\"fill\", \"{{nodeClickColour}}\")
.attr(\"r\", 16);
}

// action to take on mouse double click
function dblclick() {
d3.select(this).select(\"circle\").transition()
.duration(750)
.attr(\"r\", 8)
.style(\"fill\", \"{{nodeColour}}\")
;

d3.select(this).select(\"text\").transition()
.duration(750)
.attr(\"x\", 12)
.style(\"stroke\", \"none\")
.style(\"fill\", \"{{textColour}}\")
.style(\"stroke\", \"none\")
.style(\"opacity\", {{nodeopacity}})
.style(\"font\", \"{{fontsize}}px serif\");
}

</script>\n
"
}

# from d3Network
#----------------------
toJSONarray <- function(dtf){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric' && class(dtf[, i])!='integer'){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, 
                                                          collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}

BasicHead <- function(){
"<!DOCTYPE html>
<meta charset=\"utf-8\">
<body> \n"
}

#' Mustache basic CSS template for d3Network
#'
#' @keywords internals
#' @noRd

BasicStyleSheet <- function(){
"<style>
.link {
stroke: {{linkColour}};
opacity: {{opacity}};
stroke-width: 1.5px;
}
.node circle {
stroke: #fff;
opacity: {{nodeopacity};
stroke-width: 1.5px;
}
text {
font: {{fontsize}}px serif;
opacity: {{nodeopacity}};
pointer-events: none;
}
</style>

<script src={{d3Script}}></script>

<script> \n"
}

#' Mustache CSS template for d3ForceNetwork
#'
#' @keywords internals
#' @noRd

ForceMainStyleSheet <- function(){
"<style>
.link {
stroke: {{linkColour}};
opacity: {{opacity}};
stroke-width: 1.5px;
}
.edgelabel{
font: {{fontsize}}px serif;
opacity: {{opacity}};
pointer-events: none;
}
.node circle {
stroke: #fff;
opacity: {{nodeopacity}};
stroke-width: 1.5px;
}
.node:not(:hover) .nodetext {
display: none;
}
text {
font: {{fontsize}}px serif;
opacity: {{nodeopacity}};
pointer-events: none;
}
</style>

<script src={{d3Script}}></script>

<script> \n"
}

