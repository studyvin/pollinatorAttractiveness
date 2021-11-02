#' @export
#'
#' @title Create a bar chart of means with 95\% confidence intervals bounded
#'   below by zero.
#'
#' @description Create a bar chart of means with 95\% confidence intervals
#'   bounded below by zero.
#'
#' @param df A \code{data.frame} containing columns for \code{FlowerType},
#'   \code{Variety}, and \code{b0}.  Typically the output from function
#'   \code{getPoissonMeans}.
#'
#' @param fileSave A character string used to name the plot file.
#'
#' @param title A character string used to entitle the plot.
#'
#' @param xlab A character string used to label the \eqn{x}-axis.
#'
#' @param ylab A character string used to label the \eqn{y}-axis.
#'
#' @param dir A character string used to identify the directory into which
#'   output is to be saved.  The directory should have a trailing slash
#'   "\code{/}."
#'
#' @param height A number indicating the height of the resulting \code{png}, in
#'   inches.
#'
#' @param width A number indicating the width of the resulting \code{png}, in
#'   inches.
#'
#' @param hline1 A logical indicating if a vertical line should be drawn where
#'   the \eqn{x}-axis equals one.  Default is \code{FALSE}.
#'
#' @param hcol A character string used to identify the uniform color of all
#'   bars, when only one color is needed, typically because there is one
#'   \code{FlowerType}.  Default is \code{"red"}.
#'
#' @param text The size of the output font.  Larger means bigger.  Default is
#'   \code{11}.
#'
#' @details This function creates a \code{png} via \code{ggplot2}.
#'
#' @return A \code{ggplot2}-styled bar chart, with bars oriented horizontally.
#'
#' @author Jason Mitchell, based on original work by Zoe Gustafson.
#'
#' @seealso \code{ggplot2::ggplot} and friends, \code{forcats}.
#'
makeTallBars <- function(df,fileSave,title,xlab,ylab,dir,height,width,hline1=FALSE,hcol="red",text=11){

  # df <- totalperennial %>%
  # dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>%
  #   dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>%
  #   dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>%
  #   getPoissonMeans() %>%
  #   dplyr::arrange(dplyr::desc(b0)) %>%
  #   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  #   dplyr::filter(FlowerType == ft)
  # fileSave <- paste0("2018 Perennials -- ",insectClean," -- ",ft)
  # title <- ""
  # xlab <- "Ratios with 95% Confidence Interval"
  # ylab <- "Cultivar Comparisons"
  # dir <- paste0(out,"2018 Perennials/")
  # height <- 12
  # width <- 12
  # hline1 <- TRUE
  # hcol <- "green"
  # text <- 20

  #   ---- Count number of flower types so we know how to color.
  nFlowerType <- length(unique(df$FlowerType))

  #   ---- Make bar plot.
  if(nFlowerType == 1){
    p <- df %>%
      dplyr::arrange(dplyr::desc(.data$b0)) %>%
      dplyr::mutate(Cultivar=factor(.data$Cultivar,levels=.data$Cultivar)) %>%
      ggplot2::ggplot(ggplot2::aes(x=.data$Cultivar,y=.data$b0)) +
      {if(hline1) ggplot2::geom_hline(ggplot2::aes(yintercept=1),color="darkgray",show.legend=FALSE,size=1.0)} +
      ggplot2::geom_bar(stat="identity",fill=hcol,color="black",position=ggplot2::position_dodge(),width=0.825) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$lo2.5,ymax=.data$hi97.5),width=0.2,position=ggplot2::position_dodge(0.9)) +
      # ggplot2::coord_flip() +
      ggplot2::labs(title=title,x=ylab,y=xlab) +
      ggplot2::guides(lines=FALSE) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,hjust=1,vjust=0.5),text=ggplot2::element_text(size=text))
  } else {
    ggdf <- df %>%
      # dplyr::arrange(dplyr::desc(b0)) %>%
      dplyr::mutate(Cultivar=factor(.data$Cultivar,levels=.data$Cultivar)) #%>%
    p <- ggplot2::ggplot(ggdf, ggplot2::aes(x=.data$Cultivar,y=.data$b0,fill=.data$FlowerType)) +
      {if(hline1) ggplot2::geom_hline(ggplot2::aes(yintercept=1),color="darkgray",show.legend=FALSE,size=1.0)} +
      ggplot2::geom_bar(stat="identity",color="black",position=ggplot2::position_dodge(),width=0.825) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$lo2.5,ymax=.data$hi97.5),width=0.2,position=ggplot2::position_dodge(0.9)) +
      # ggplot2::coord_flip() +
      ggplot2::labs(title=title,x=ylab,y=xlab,fill="Flower Type") +
      ggplot2::guides(lines=FALSE) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,hjust=1,vjust=0.5),text=ggplot2::element_text(size=text))
  }
  grDevices::png(paste0(dir,fileSave,".png"),res=400,height=height,width=width,units="in")
  print(p)
    # theme_classic() +
    # ggplot2::scale_fill_manual(values=c('#999999','#E69F00'))
  grDevices::dev.off()
}
