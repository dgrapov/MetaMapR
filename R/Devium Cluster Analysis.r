
#create heatmap optionally using HCA
devium.heatmap<-function(data, class.factor=NULL, class.color=NULL, heatmap.color = NULL, border.color=NULL, match.dim=2, type=c("none","z.scale", "spearman", "pearson","biweight"),
                         cluster.method = c("none","ward", "single", "complete", "average", "mcquitty", "median" , "centroid"),
                         distance.method = c("none","euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"),
                         alpha = NULL,font.size = 12,show.names=F, ncolors = 100, level.limit=10){
  check.get.packages("pheatmap")
  type<-match.arg(type)
  cluster.method<-match.arg(cluster.method)
  distance.method<-match.arg(distance.method)
  
  #prepare data object
  if(match.dim==1){tmp.data<-data.frame(t(data.frame(data)))} else { tmp.data<-data.frame(data)}
	
  if(type == "z.scale") {tmp.data<-scale(tmp.data, center=TRUE, scale = TRUE)}		
  
  # calculate correlations
  if(!type=="none"& !type == "z.scale" ){
    tmp<-devium.calculate.correlations(tmp.data,type=type)
    tmp.data<-tmp$cor
    tmp.data.pvalue<-tmp$p.value
    
    if(is.numeric(alpha)){ # make discrete
      tmp.data[tmp.data>0]<-1
      tmp.data[tmp.data<0]<--1
      tmp.data[tmp.data.pvalue>alpha]<-0
      ncolors<-3 # limit heat map to 3 colors
    }
  }
  
  if(!cluster.method=="none"){
    cluster_rows<-cluster_cols<-T
    # calculate distances
    if(!distance.method=="none"){
      if(type=="none"|type == "z.scale" ){
        # if(match.dim==2){
        # clustering_distance_cols<-dist(tmp.data, method= distance.method)
        # clustering_distance_rows<-dist(data.frame(t(tmp.data)), method= distance.method)
        # } else {
        clustering_distance_rows<-dist(tmp.data, method= distance.method)
        clustering_distance_cols<-dist(data.frame(t(tmp.data)), method= distance.method)
        # }
      } else {                         				
        tmp.data.dist<-dist(tmp.data, method= distance.method) # for correlations ignore sign focus on magnitude
        clustering_distance_cols<-clustering_distance_rows<-tmp.data.dist
      }
    } 
  } else {
    cluster_rows<-cluster_cols<-F
    cluster.method<-NULL			
  }
  
  # set colors for top of the heatmap annotation
  if(!is.null(class.factor)){
    annotation<- data.frame(sapply(class.factor,as.factor))
    dimnames(annotation)<-dimnames(class.factor)
	#rownames need to match data colnames (which depend on match dim)
	#tryCatch(row.names(annotation)<-colnames(tmp.data), error=function(e){row.names(annotation)<-rownames(tmp.data)}) # hack
    #tryCatch(rownames(annotation)<-colnames(tmp.data), error=function(e){cat("The supplied class.factor doesn't match the number of columns","\n")})
  } else {
    annotation<-NA
  }
  
  if(is.na(annotation)){
    annotation.color<-NA
  } else {
    if(is.null(class.color)){
      choices<-colors()[-agrep(c("gray","grey"),colors())]
      #test if factor is continuous or discrete
      fct.type<-sapply(1:ncol(annotation),function(i){
        obj<-as.factor(annotation[,i])
        nlevels(obj)>level.limit
      })
      
      annotation.color<-lapply(1:ncol(class.factor), function(i){
        if(fct.type[i]){
          tmp<-choices[sample(1:length(choices),1)]
          tmp<-colorRampPalette(c("white",tmp))(nlevels(annotation[,i]))
          names(tmp)<-levels(annotation[,i])
        } else {
          tmp<-choices[sample(1:length(choices),nlevels(annotation[,i]))]
          names(tmp)<-levels(annotation[,i])
        }
        tmp
      })
      names(annotation.color)<-colnames(annotation)
    } else {
      annotation.color<- lapply(1:ncol(class.color), function(i){
        fct<-as.factor(annotation[,i])
        sapply(1:nlevels(fct), function(j){
          name<-levels(fct)[j]
          tmp<-fixlc(class.color[which(fct==name)[1], i])
          names(tmp)<-name
          tmp
        })
      })
      names(annotation.color)<-colnames(annotation)
    }
  }
  
  
  if(show.names==FALSE){
    show_rownames<-show_colnames <-F
  } else { 
    show_rownames<-show_colnames <-T
  }
  
  #set colors
  heat.col<-function(col=1,n){ # color for heat map
    # cat("Choices  \n","1 = orange-white-blue",
    # "\n", "2 = white-red \n","3 = white-black \n",
    # "4 = green-black-red \n",
    # "5 = yellow-red \n",
    # "6 = white-yellow-red \n",
    # "7 = white-yellow-orange-red \n")
    col= switch(col,
                "1" = colorRampPalette(c("navy", "white", "orange"))(n),
                "2" = colorRampPalette(c( "white", "red"))(n),
                "3" = colorRampPalette(c( "white", "black"))(n),
                "4" = colorRampPalette(c( "green","black", "red"))(n),
                "5" = colorRampPalette(c( "yellow", "red"))(n),
                "6" = colorRampPalette(c( "white","yellow", "red"))(n),
                "7" = colorRampPalette(c( "white","yellow","orange", "red"))(n),
                "8" = colorRampPalette(c("navy", "white", "firebrick3"))(n))
    return(col)
  }
  
  if(is.null(heatmap.color)){ heat.color<-heat.col(8,ncolors) } else { heat.color<-colorRampPalette(heatmap.color)(ncolors)}
  
  #plot heat map
  pheatmap(tmp.data,col= heat.color,
           show_rownames = show_rownames,show_colnames = show_colnames, # show labels
           cluster_rows = cluster_rows, cluster_cols=cluster_cols, # cluster
           clustering_method=cluster.method,
           clustering_distance_rows = clustering_distance_rows,
           clustering_distance_cols = clustering_distance_cols,
           border_color = border.color,
           annotation = annotation, # annotation factor
           annotation_colors = annotation.color, # colors for each column of annotation factor
           annotation_legend= TRUE,
           fontsize = font.size)
}

# slight modification of  on "http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R"
# fix for the case where labels are numeric and add color, digit length and size control
devium.dendrogram<-function( 
  x ,             # an hclust object to draw
  k        = 2,   # the number of groups
  col.up   = "black",
  col.down = rainbow(k),
  lty.up   = 2,
  lty.down = 1,
  lwd.up   = 1,
  lwd.down = 2,
  type     = c("rectangle","triangle"),
  knot.pos = c("mean","bary","left","right","random"),
  criteria,
  fact.sup,
  show.labels=TRUE,
  only.tree=FALSE,
  main     = paste("Colored Dendrogram (",k," groups)"),
  boxes    = TRUE,
  members,
  color.text = FALSE,
  text.length = 10,
  text.cex=.5,
  ...
){

  if(missing(members)) members <- NULL
  opar <- par(no.readonly=TRUE)
  knot.pos <- match.arg(knot.pos)
  type     <- match.arg(type)
  # tests
  if(k<2) 
    stop("k must be at least 2")  
    
  ._a2r_counter    <<- 0
  ._a2r_hclu       <<- x

  ._a2r_envir      <<- environment()
  nn <- length(x$order) - 1

  ._a2r_height_cut <<- mean(x$height[nn-k+1:2])
  ._a2r_group      <<- 0
  
  n.indiv   <- length(x$order)
  groups.o  <- cutree.order(x, k=k)[x$order]
  
  bottom <- if(is.null(members)) 0 else x$height[nn] * -.2 
  
  if(only.tree){
    if(is.null(members)) plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
    else                 plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="")
    #call to the ** recursive function ** .rec.hclust
    .rec.hclust(nn, col=col.up, lty=lty.up, lwd=lwd.up)
    
    if(boxes){
      axis(2)
      box()
    }
    return(NULL)
  }
  
  # prepare the layout
  matlayout <- matrix(c(2,4,6,1,3,5), nc=2, nr=3)
  widths    <- c(1,9)
  heights   <- c(8,1,1)
  if(!show.labels){
      matlayout <- matrix(c(2,4,1,3), nc=2, nr=2)
      widths    <- c(1,9)
      heights   <- c(9,1)
  }
  if(!missing(fact.sup) ) {
    heights   <- c(8,1,1)
  }
  if(missing(criteria) & missing(fact.sup)){
    matlayout <- matrix(c(2,4,1,3), nc=2, nr=2)
      widths    <- c(1,9)
      heights   <- c(9,1)
    
  }
  layout(matlayout, width=widths, height=heights)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ The tree (1)
  par(mar=c(0,0,3,4))
  if(is.null(members)) plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
  else plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
  #call to the ** recursive function ** .rec.hclust
  .rec.hclust(nn, col=col.up, lty=lty.up, lwd=lwd.up)
  title(main)
  if(boxes){
    box()
    axis(4)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Criteria (2)
  if(!missing(criteria)){
    par(mar=c(0,0,3,0))
    plot(0,
         type="n",
         xlim=range(criteria), 
         ylim=c(0,x$height[nn]), 
         axes=FALSE, 
         xlab="",
         ylab="")
    par(las=2)
    n.crit <- length(criteria)
    heights.cut <- ( tail(x$height,n.crit) + 
                     tail(x$height,n.crit+1)[-(n.crit+1)] ) / 2
    heights.cut <- rev(heights.cut)
                   
    points(criteria   , heights.cut   , pch=21, bg="red", type="o")
    points(criteria[k-1], heights.cut[k-1], pch=21, cex=2, bg="blue", xpd=NA)
    if(boxes){
      axis(3)
      box()
    }
  }
  else{
    par(mar=c(0,0,3,0))
    plot(0,
         type="n",
         xlim=c(0,1), 
         ylim=c(0,1), 
         axes=FALSE, 
         xlab="",
         ylab="")
  }

  if(show.labels){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Name of the observations (3)
    par(mar=c(0,0,0,4))
    par(srt=90)
    obs.labels <- toupper(substr(x$labels[x$order],1,text.length))
    if(is.null(members)) {
      plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
      if(color.text){
		 text(1:n.indiv    , 0, obs.labels, pos=4, col=col.down[groups.o],cex=text.cex)
		} else {
		 text(1:n.indiv    , 0, obs.labels, pos=4,cex=text.cex)
	  }
    }
    else{
      plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
      xo <-   members[x$order]
	  if(color.text){
		text(cumsum(xo)-xo/2, 0, obs.labels, pos=4, col=col.down[groups.o],cex=text.cex) 
		} else {
		text(cumsum(xo)-xo/2, 0, obs.labels, pos=4,cex=text.cex)
	  }
    }
    par(srt=0)
    if(boxes){
      box()
    }
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Labels (4)
    # par(mar=c(0,0,0,0))
    # plot(0,type="n",xlim=c(0,1), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
    # text(.5,.5,"Labels")
    # if(boxes){
      # box()
    # }
      
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Quali (5,6)
  if(!missing(fact.sup)){
    quali  <- as.factor(fact.sup)[x$order]
    quanti <- as.numeric(quali)

    par(mar=c(1,0,0,4))
    n.levels <- length(levels(quali))
    plot(0,type="n",
         xlim=c(0.5,n.indiv+.5), 
         ylim=c(0,n.levels), 
         xaxs="i", yaxs="i",axes=FALSE, xlab="",ylab="") 
        
    rect(xleft    = (1:n.indiv)-.5,
         xright   = (1:n.indiv)+.5,
         ybottom  = quanti-1, 
         ytop     = quanti,
         col      = col.down[groups.o])
    par(las=1)
    axis(4, (1:n.levels)-.5,levels(quali), tick=FALSE)
      
    if(boxes){
      box()
    }
    
    
    par(mar=c(1,0,0,0))
    plot(0,type="n",xlim=c(0,1), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
    text(.5,.5,deparse(substitute(fact.sup)))
    if(boxes){
      box()
    }
  }
  
  
  par(opar) # reset parameter
}



#exactly from "http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R"
"._a2r_hclu"       <- NULL # to receive an hclust object when 
                           # A2Rplot.hclust is called

"._a2r_counter"       <- NA # a counter used in A2Rplot.hclust
"._a2r_height_cut"    <- NA

"._a2r_envir"         <- NA
"._a2r_group"         <- NA


#===============================================================================
"A2Rplot" <- function(x,...){
  UseMethod("A2Rplot")
}
#===============================================================================
"A2Rplot.default" <- function(x,...){
  plot(x,...)
}
#===============================================================================
"A2Rplot.hclust" <- function(
  x ,             # an hclust object to draw
  k        = 2,   # the number of groups
  col.up   = "black",
  col.down = rainbow(k),
  lty.up   = 2,
  lty.down = 1,
  lwd.up   = 1,
  lwd.down = 2,
  type     = c("rectangle","triangle"),
  knot.pos = c("mean","bary","left","right","random"),
  criteria,
  fact.sup,
  show.labels=TRUE,
  only.tree=FALSE,
  main     = paste("Colored Dendrogram (",k," groups)"),
  boxes    = TRUE,
  members,
  ...
){

  if(missing(members)) members <- NULL
  opar <- par(no.readonly=TRUE)
  knot.pos <- match.arg(knot.pos)
  type     <- match.arg(type)
  # tests
  if(k<2) 
    stop("k must be at least 2")  
    
  ._a2r_counter    <<- 0
  ._a2r_hclu       <<- x

  ._a2r_envir      <<- environment()
  nn <- length(x$order) - 1

  ._a2r_height_cut <<- mean(x$height[nn-k+1:2])
  ._a2r_group      <<- 0
  
  n.indiv   <- length(x$order)
  groups.o  <- cutree.order(x, k=k)[x$order]
  
  bottom <- if(is.null(members)) 0 else x$height[nn] * -.2 
  
  if(only.tree){
    if(is.null(members)) plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
    else                 plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="")
    #call to the ** recursive function ** .rec.hclust
    .rec.hclust(nn, col=col.up, lty=lty.up, lwd=lwd.up)
    
    if(boxes){
      axis(2)
      box()
    }
    return(NULL)
  }
  
  # prepare the layout
  matlayout <- matrix(c(2,4,6,1,3,5), nc=2, nr=3)
  widths    <- c(1,9)
  heights   <- c(8,1,1)
  if(!show.labels){
      matlayout <- matrix(c(2,4,1,3), nc=2, nr=2)
      widths    <- c(1,9)
      heights   <- c(9,1)
  }
  if(!missing(fact.sup) ) {
    heights   <- c(8,1,1)
  }
  if(missing(criteria) & missing(fact.sup)){
    matlayout <- matrix(c(2,4,1,3), nc=2, nr=2)
      widths    <- c(1,9)
      heights   <- c(9,1)
    
  }
  layout(matlayout, width=widths, height=heights)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ The tree (1)
  par(mar=c(0,0,3,4))
  if(is.null(members)) plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
  else plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(bottom,x$height[nn]), xaxs="i", axes=FALSE, xlab="",ylab="") 
  #call to the ** recursive function ** .rec.hclust
  .rec.hclust(nn, col=col.up, lty=lty.up, lwd=lwd.up)
  title(main)
  if(boxes){
    box()
    axis(4)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Criteria (2)
  if(!missing(criteria)){
    par(mar=c(0,0,3,0))
    plot(0,
         type="n",
         xlim=range(criteria), 
         ylim=c(0,x$height[nn]), 
         axes=FALSE, 
         xlab="",
         ylab="")
    par(las=2)
    n.crit <- length(criteria)
    heights.cut <- ( tail(x$height,n.crit) + 
                     tail(x$height,n.crit+1)[-(n.crit+1)] ) / 2
    heights.cut <- rev(heights.cut)
                   
    points(criteria   , heights.cut   , pch=21, bg="red", type="o")
    points(criteria[k-1], heights.cut[k-1], pch=21, cex=2, bg="blue", xpd=NA)
    if(boxes){
      axis(3)
      box()
    }
  }
  else{
    par(mar=c(0,0,3,0))
    plot(0,
         type="n",
         xlim=c(0,1), 
         ylim=c(0,1), 
         axes=FALSE, 
         xlab="",
         ylab="")
  }

  if(show.labels){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Name of the observations (3)
    par(mar=c(0,0,0,4))
    par(srt=90)
    obs.labels <- toupper(substr(x$labels[x$order],1,6))
    if(is.null(members)) {
      plot(0,type="n",xlim=c(0.5,n.indiv+.5), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
      text(1:n.indiv            , 0, obs.labels, pos=4, col=col.down[groups.o])
    }
    else{
      plot(0,type="n",xlim=c(0.5,sum(members)+.5), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
      xo <-   members[x$order]
      text(cumsum(xo)-xo/2, 0, obs.labels, pos=4, col=col.down[groups.o])
    }
    par(srt=0)
    if(boxes){
      box()
    }
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Labels (4)
    par(mar=c(0,0,0,0))
    plot(0,type="n",xlim=c(0,1), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
    text(.5,.5,"Labels")
    if(boxes){
      box()
    }
      
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Quali (5,6)
  if(!missing(fact.sup)){
    quali  <- as.factor(fact.sup)[x$order]
    quanti <- as.numeric(quali)

    par(mar=c(1,0,0,4))
    n.levels <- length(levels(quali))
    plot(0,type="n",
         xlim=c(0.5,n.indiv+.5), 
         ylim=c(0,n.levels), 
         xaxs="i", yaxs="i",axes=FALSE, xlab="",ylab="") 
        
    rect(xleft    = (1:n.indiv)-.5,
         xright   = (1:n.indiv)+.5,
         ybottom  = quanti-1, 
         ytop     = quanti,
         col      = col.down[groups.o])
    par(las=1)
    axis(4, (1:n.levels)-.5,levels(quali), tick=FALSE)
      
    if(boxes){
      box()
    }
    
    
    par(mar=c(1,0,0,0))
    plot(0,type="n",xlim=c(0,1), ylim=c(0,1), xaxs="i", axes=FALSE, xlab="",ylab="") 
    text(.5,.5,deparse(substitute(fact.sup)))
    if(boxes){
      box()
    }
  }
  
  
  par(opar) # reset parameter
}

#===============================================================================

".rec.hclust" <- function(
  index, # index of the current tree to draw
  lwd = 1,
  lty = 1,
  col = "black"){

  members <- get('members', envir= ._a2r_envir) 
  bottom  <- get('bottom',  envir= ._a2r_envir) 
  if(index<0){ # it is a leaf
    if(is.null(members)){
       ._a2r_counter <<- ._a2r_counter + 1
       return(list( x = ._a2r_counter,
                    n = 1))       
    }
    else{
      cc <- ._a2r_counter
      mm <- members[-index]
      polygon(x  = c(cc, cc+mm/2, cc+mm),
              y  = c(bottom, 0, bottom),
              col= col, 
              border = col, 
              lwd=lwd)
      ._a2r_counter <<- ._a2r_counter + mm
      return(list(x = cc+mm/2,
                  n = mm))
    }
  }
  
  h.m   <- ._a2r_hclu$height[index]

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~ do left
  index.l  <- ._a2r_hclu$merge[index,1]
  
  h.l <- if(index.l<0) 0 else ._a2r_hclu$height[index.l]
  if(h.l<._a2r_height_cut & h.m > ._a2r_height_cut){
      ._a2r_group <<- ._a2r_group + 1
      col.l <- get("col.down",envir=._a2r_envir)[._a2r_group]
      lwd.l <- get("lwd.down",envir=._a2r_envir)
      lty.l <- get("lty.down",envir=._a2r_envir)
  }
  else{
      col.l <- col
      lwd.l <- lwd
      lty.l <- lty
  }
  out.l   <- .rec.hclust(index.l, col=col.l, lty=lty.l, lwd=lwd.l)
  x.l     <- out.l$x
  n.l     <- out.l$n
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~ do right
  index.r  <- ._a2r_hclu$merge[index,2]
  h.r <- if(index.r<0) 0 else ._a2r_hclu$height[index.r]
  if(h.r<._a2r_height_cut & h.m > ._a2r_height_cut){
      ._a2r_group <<- ._a2r_group + 1
      col.r <- get("col.down",envir=._a2r_envir)[._a2r_group]
      lwd.r <- get("lwd.down",envir=._a2r_envir)
      lty.r <- get("lty.down",envir=._a2r_envir)
  }
  else{
      col.r <- col
      lwd.r <- lwd
      lty.r <- lty
  }
  out.r   <- .rec.hclust(index.r, col=col.r, lty=lty.r, lwd=lwd.r)
  x.r     <- out.r$x
  n.r     <- out.r$n
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~ draw what you have to draw
  
  type <- get("type",envir=._a2r_envir)
  x.m  <- (x.r + x.l) / 2  
  n    <- n.r + n.l
  x.b  <- (n.r * x.r + n.l * x.l) / n

  
  knot.pos <- get("knot.pos",envir=._a2r_envir) 
  
  x <- switch(knot.pos,
          mean = x.m,
          left = x.l,
          right= x.r,
          random = x.l + runif(1)*(x.r-x.l),
          bary   = x.b)

          
          
  if(type=="rectangle"){
    segments(x0  = c(x.l, x.l, x.r),
             x1  = c(x.l, x.r, x.r),
             y0  = c(h.l, h.m, h.r),
             y1  = c(h.m, h.m, h.m),
             col = col,
             lty = lty,
             lwd = lwd)
  }
  if(type =="triangle"){
    segments(x0  = c(x.l, x.r),
             x1  = c(x  , x),
             y0  = c(h.l, h.r),
             y1  = c(h.m, h.m),
             col = col,
             lty = lty,
             lwd = lwd)
  }
          
          
  list(x=x,n=n)
}
#===============================================================================
"cutree.order" <- function(hclu, k=NULL, h=NULL){
  
  coupe <- cutree(hclu,k=k, h=h)

  coupe.or <- coupe[hclu$order]
  coupe.out<- rep(NA,length(coupe))
  j <-  1 #
  k <-  coupe.or[1]
  for(i in 1:length(coupe)){
    if(coupe.or[i]==k) next
    else{
      coupe.out[which(coupe==k)] <- j
      j <- j + 1
      k <- coupe.or[i]
    }
  }
  coupe.out[is.na(coupe.out)] <- j
  names(coupe.out) <- names(coupe)
  coupe.out
}
#===============================================================================

#various additions/tests in progress
test<-function(){

#colored label dendrograms
## function to set label color
# see added color.key which you can define for your data
labelCol <- function(x,color.key) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color 
	color<-unname(color.key[label,])
    attr(x, "nodePar") <- list(lab.col=color)
  }
  return(x)
}

## apply labelCol on all nodes of the dendrogram
color.key<-matrix(mycolors);rownames(color.key)<-rownames(mydata) # extraction is done on rownames,use make.unique() if not unique
d <- dendrapply(as.dendrogram(mydata.bc.clust), labelCol, color.key)



}