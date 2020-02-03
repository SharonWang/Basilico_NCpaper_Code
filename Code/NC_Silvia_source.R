#######################################################
##' This contains the source codes required for plotting
##' Most of the codes relies on the SCD object created from bglab R package
## developed by wajid Jawaid and can be installed from gitbub @
## https://github.com/wjawaid/bglab
##' Author: Xiaonan Wang
##' Email: xw251@cam.ac.uk
##' Date: 02Feb2020
#######################################################

#######################################################
##' General scatter plot function for descrete features
##' 
##' @param scd SCD object from bglab R package
##' @param Co Coordinates, only if reduceMethod == 'none'
##' @param dim Components from dimensionality method (x,y)
##' @param reduceMethod Default method: PCA; Alternatively: tsne
##' @param lv Reorder levels of the Pheno input from colorby option
##' @param lv_rename Rename the levels of the Pheno input from colorby option
##' @param colorby Pheno input from SCD object (pData(scd))
##' @param Cat Split the plot by a Pheno input from SCD object
##' @param width Width of PDF output, only if pdf != NULL
##' @param height Height of FPD output, only if pdf != NULL
##' @param theme General theme of ggplot2
##' @param colors Manually input colour of categories of Pheno input
##' @param pchfac Shape of point
##' @param pdf Name of PDF output; Default: NULL
##' @param point.size Size of points
##' @param label Label certain cells; R data.fram with two columns, 1st column: Cell Names, 2nd column: Names for labelling
##' @param LegendOnPlot Label legends on plot; Default: FALSE
##' @param fontsize Fontsize of legend if LegendOnPlot == TRUE
##' @return Scatter plot
##' @author Xiaonan Wang
##' @export

plotPCA1 <- function(scd, Co=NULL,dim=c(1,2), reduceMethod="pca", lv=NULL,lv_rename=NULL, colorby, Cat=FALSE,width=NA, height=NA, theme=NULL, colors=NULL, pchfac=NULL, pdf=NULL, point.size=2, label=NULL, LegendOnPlot=FALSE, fontsize=5){
  require(ggplot2)
  Type <- pData(scd)[,match(colorby, colnames(pData(scd)))]
  # uniq_Type <- unique(Type)
  # if (sum(grepl("\\d", uniq_Type))>0){
  #   require(tidyr)
  #   T1 <- extract_numeric(uniq_Type)
  #   o1 <- order(T1)
  #   T1 <- T1[o1]
  #   Type <- as.factor(Type)
  #   levels(Type) <- T1
  # }
  
  
  if (length(colorby) > 1){
    Type <- apply(Type, 1, function(x) paste(x, collapse="_"))
  } else if (length(colorby) == 0) {
    stop("provide correct colorby")
  }
  
  Type <- as.factor(as.character(Type))
  nType <- length(unique(Type))
  
  if (!is.null(lv)){
    Type <- factor(Type, levels=lv)
  }
  #print(levels(as.factor(as.character(Type))))
  
  if (!is.null(lv_rename)){
    levels(Type) <- lv_rename
  }
  
  
  #print(dim(data))
  if (reduceMethod=="pca"){
    x <- scd@pca@eigenvectors[,dim[1]]
    y <- scd@pca@eigenvectors[,dim[2]]
  } else if (reduceMethod=="tsne"){
    x <- scd@tsne@eigenvectors[,1]
    y <- scd@tsne@eigenvectors[,2]
  } 
  
  if (!is.null(Co)){
    x <- Co[,dim[1]]
    y <- Co[,dim[2]]
  }
  
  if (Cat==TRUE){
    nData <- nrow(pData(scd))
    newT <- unlist(lapply(unique(Type), function(xsub){
      newT <- Type %in% xsub
      return(newT)
    }))
    newT<- factor(newT, levels=c("TRUE", NA))
    T1 <- rep(unique(Type), each=nData)
    if (!is.null(lv_rename)){
      T1 <- as.factor(as.character(T1))
      levels(T1) <- lv_rename
    }
    data <- data.frame(CN=rep(pData(scd)$ID, nType), TypeSub=newT, component1=rep(x, nType), component2=rep(y, nType), Type=T1)
    p <- ggplot(data, aes(x=component1, y=component2, color=TypeSub)) +geom_point(size=point.size) + facet_wrap(~Type)
  } else {
    data <- data.frame(CN = pData(scd)$ID, Type=Type, component1=x, component2=y)
    p <- ggplot(data, aes(x=component1, y=component2))+geom_point(aes(color=Type), size=point.size)
  }
  #print(table(data$TypeSub))
  #print(dim(data))
  #print(head(data))
  
  if (!is.null(pchfac)){
    data$Condition <- pchfac
    p <- ggplot(data, aes(x=component1, y=component2))+geom_point(aes(color=Type, shape=Condition), size=point.size)
  } 
  
  p <- p + xlab(paste("Component", dim[1], sep=""))+ylab(paste("Component", dim[2], sep=""))
  
  if (!is.null(colors)){
    if (length(colors) != nType){
      stop("Color numbers not the same")
    } else {
      p <- p + scale_color_manual(values=colors)
    }
  }
  
  if (!is.null(theme)){
    if (theme == "bw"){
      p <- p + theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),
                                  panel.border = element_rect(colour = "black"))
    }
  }
  
  
  if (!is.null(label)){
    colnames(label) <- c("CN", "Name")
    #print(head(label))
    subData <- data[data$CN %in% label$CN,]
    #print(head(subData))
    subData <- merge(subData, label, by="CN", all.x=TRUE)
    
    p <- p + geom_text_repel(
      data = subData,
      aes(label = Name),
      size = 2,
      segment.color = 'black',
      # Width of the line segments.
      segment.size = 2,
      # Draw an arrow from the label to the data point.
      #arrow = arrow(length = unit(0.01, 'npc')),
      # Strength of the repulsion force.
      force = 1#,
      #box.padding = unit(0.35, "lines"),
      #point.padding = unit(0.3, "lines")
    )
  }
  
  if (LegendOnPlot){
    p <- addLegend(p, co=cbind(x,y),colorby=Type, fontsize = fontsize)
  }
  
  if(!is.null(pdf)){
    p
    ggsave(paste(pdf, ".pdf", sep=""), device="pdf", width=width, height=height, useDingbats=FALSE)
  } else {
    p
  }
}


#######################################################
##' General scatter plot function for continues features
##' 
##' @param scd SCD object from bglab R package
##' @param Co Coordinates, only if reduceMethod == 'none'
##' @param dim Components from dimensionality method (x,y)
##' @param reduceMethod Default method: PCA; Alternatively: tsne
##' @param gene Gene names from Features of SCD object (fData(scd)), alternatively, dataframe with ncol == ncells
##' @param width Width of PDF output, only if pdf != NULL
##' @param height Height of FPD output, only if pdf != NULL
##' @param theme General theme of ggplot2
##' @param pchfac Shape of point
##' @param pdf Name of PDF output; Default: NULL
##' @param ncol Number of columns if number of features provided > 1
##' @param point.size Size of points
##' @param label Label certain cells; R data.fram with two columns, 1st column: Cell Names, 2nd column: Names for labelling
##' @param Scales parameter in ggplot2; Default: fixed, alternatively: free
##' @return Scatter plot
##' @author Xiaonan Wang
##' @export
plotGene1 <- function(scd, gene, dim=c(1,2),Co=NULL, reduceMethod="pca", pchfac=NULL, pdf=NULL, ncol=2, point.size=2, label=NULL, width=NA, height=NA, scales="fixed", theme=NULL){ # label should be dataframe, first column: ID, second: display name
  require(ggplot2); require(reshape2);require(ggrepel)
  if (is.data.frame(gene)){
    data <- t(gene)
    #data <- as.matrix(data)
    #colnames(data) <- pData(scd)$ID
    ngene <- nrow(data)
  } else {
    data <- getExp(scd, id = gene)
    ngene <- length(gene)
  }
  
  #print(ngene)
  #print(dim(data))
  if (reduceMethod=="pca"){
    x <- scd@pca@eigenvectors[,dim[1]]
    y <- scd@pca@eigenvectors[,dim[2]]
  } else if (reduceMethod=="tsne"){
    x <- scd@tsne@eigenvectors[,dim[1]]
    y <- scd@tsne@eigenvectors[,dim[2]]
  }
  
  if (!is.null(Co)){
    x <- Co[,dim[1]]
    y <- Co[,dim[2]]
  }
  
  data <- melt(data)
  #print(dim(data))
  #print(head(data))
  data <- data.frame(CN = rep(pData(scd)$ID, each=ngene), gene=data$Var1, log10_Exp=data$value, component1=rep(x, each=ngene), component2=rep(y, each=ngene))
  
  #print(head(data))
  
  if (!is.null(pchfac)){
    data$Condition <- rep(pchfac, each=ngene)
    p <- ggplot(data, aes(x=component1, y=component2))+geom_point(aes(color=log10_Exp, shape=Condition), size=point.size)+facet_wrap(~gene, ncol=ncol, scales = scales)+scale_colour_gradient(low = "grey", high = "red")
  } else {
    p <- ggplot(data, aes(x=component1, y=component2))+geom_point(aes(color=log10_Exp), size=point.size)+facet_wrap(~gene, ncol=ncol, scales = scales)+scale_colour_gradient(low = "grey", high = "red")
  }
  p <- p + geom_point(data = subset(data, log10_Exp>0),aes(x=component1, y=component2,color=log10_Exp), size=point.size)
  
  if (!is.null(label)){
    colnames(label) <- c("CN", "Name")
    #print(head(label))
    subData <- data[data$CN %in% label$CN,]
    #print(head(subData))
    subData <- merge(subData, label, by="CN", all.x=TRUE)
    
    p <- p + geom_text_repel(
      data = subData,
      aes(label = Name),
      size = 2,
      segment.color = 'black',
      # Width of the line segments.
      segment.size = 1,
      # Draw an arrow from the label to the data point.
      #arrow = arrow(length = unit(0.01, 'npc')),
      # Strength of the repulsion force.
      force = 1#,
      #box.padding = unit(0.35, "lines"),
      #point.padding = unit(0.3, "lines")
    )
  }
  
  if (!is.null(theme)){
    if (theme == "bw"){
      p <- p + theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  strip.background = element_rect(colour = "black", fill = "white"),
                                  panel.border = element_rect(colour = "black"))
    }
  }
  
  if(!is.null(pdf)){
    p
    ggsave(paste(pdf, ".pdf", sep=""), device="pdf", width=width, height=height, useDingbats=FALSE)
  } else {
    p
  }
}


#######################################################
##' Get gene expression/counts from SCD object
##' 
##' @param scd SCD object from bglab R package
##' @param id Vector of feature/gene Names; Alternative: Ensembl IDs if convert = FALSE
##' @parma cell Cells to be extracted
##' @param exp Expression/Raw count matrix; Default: Expression
##' @param convert If converting gene names to Ensembl; Default: TRUE
##' @return Exp/cnt matrix
##' @author Xiaonan Wang
##' @export
getExp <- function(scd, id, cell=NULL, exp=TRUE, convert=TRUE){
  id <- unique(id)
  filterGene(scd) <- FALSE
  if (exp==TRUE){
    exp <- exprs(scd)
  } else {
    exp <- counts(scd)
  }
  if (convert==TRUE){
    ID <- geneName2id(scd, id)
  } else {
    ID <- id
  }
  if (!is.null(cell)){
    exp <- exp[, match(cell, colnames(exp)), drop=FALSE]
  }
  exp <- exp[match(ID, rownames(exp)),, drop=FALSE]
  rownames(exp) <- id
  return(exp)
}


#######################################################
##' General violin plot function
##' 
##' @param data Expression matrix; Column: Cells; Rows: Genes
##' @param ncol Number of columns
##' @parma scale Scale parameter in ggplot2 for normalisation; Default: "area"
##' @param scales Scales parameter in ggplot2; Default: fixed, alternatively: free
##' @param ylab y-axis label
##' @param lv Reorder levels of input features
##' @param textsize Size for strip title
##' @param strip.position Location of strip; Default: top
##' @return Exp/cnt matrix
##' @author Xiaonan Wang
##' @export
ggviolinplot <- function(data, ncol=3, scale="area", scales="fixed", ylab="log10 Normalised Counts", lv = NULL, textsize=7, strip.position="top"){
  require(reshape2)
  gExp_melt <- melt(data)
  if (!is.null(lv)){
    gExp_melt$Var2 <- factor(gExp_melt$Var2, levels=lv)
  }
  c <- ggplot(gExp_melt, aes(Var2, value))+geom_violin(aes(fill=Var2), scale=scale)+geom_jitter(size=0.1)+facet_wrap(~Var1, ncol=ncol, scales=scales, strip.position=strip.position)
  c <- c + xlab("Type")+ylab(ylab)+guides(fill=guide_legend(title="Type"))
  c <- c + theme(axis.text.x=element_blank(), axis.text.y=element_text(size=textsize), strip.text=element_text(size=textsize))
  return(c)
}


#######################################################
##' Provide colours to categories of a vector input
##' 
##' @param x Vector 
##' @param col Colours for each category of input x
##' @return List with Legend, Colours and Colour_level
##' @author Xiaonan Wang
##' @export
generate_colour <- function(x, col=NULL){
  x <- as.factor(as.character(x))
  x_lv <- levels(x)
  print(x_lv)
  print(length(x_lv))
  if (!is.null(col)){
    stopifnot(length(col)==length(x_lv))
    levels(x) <- col
  } else {
    levels(x) <- rainbow(length(x_lv))
  }
  return(list(Legend=x_lv, Colours=as.character(x), Colour_level=levels(x)))
}


#######################################################
##' Row normalisation
##' 
##' @param x Matrix
##' @param na.rm Remove NAs; Default: TRUE
##' @return Scaled matrix 
##' @author Xiaonan Wang
##' @export
ScalebyRow <- function(x, na.rm=TRUE){
  rm <- rowMeans(x, na.rm = na.rm)
  x <- sweep(x, 1, rm)
  sx <- apply(x, 1, sd, na.rm = na.rm)
  x <- sweep(x, 1, sx, "/")
  return(x)
}


#######################################################
##' Split strings
##' 
##' @param x Matrix
##' @param split Split by; Default: _
##' @param nstr Number of split characters in each row
##' @return Split strings
##' @author Xiaonan Wang
##' @export
sepWord <- function(x, split="_", nstr=2){
  ws <- unlist(strsplit(x, split=split))
  #print(ws)
  nws <- length(ws)
  width <- ceiling(nws/nstr)
  ws1 <- unlist(sapply(1:width, function(y){
    s_idx <- 1+(y-1)*nstr
    e_idx <- (s_idx+nstr-1)
    if (e_idx > nws){
      e_idx <- nws
    }
    paste(as.character(ws[s_idx:e_idx]), collapse=split)
  }))
  #print(ws1)
  ws2 <- paste(ws1, collapse="_\n")
  return(ws2)
}


#######################################################
##' get legend from ggplot2 object
##' This is contained online from http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
##' 
##' @param myggplot ggplot2 object
##' @return Legend object from ggplot2
##' @author OnlineWiki
##' @export
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

