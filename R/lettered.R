#' @export
#'
#' @title Create a Tukey-type bar chart with Tukey letters.
#'
#' @description Create a Tukey-type bar chart with Tukey letters.
#'
#' @param df A \code{data.frame} containing columns for \code{FlowerType},
#'   \code{Variety}, \code{b0}, \code{lo2.5}, and \code{hi97.5},
#'   \code{Captured}, and \code{Observed}.  Typically the output from function
#'   \code{getPoissonMeans}.
#'
#' @param here A text string indicating the subfolder into which results should
#'   be saved.  Also sometimes used to help entitle output.
#'
#' @param fileSave A character string used to name the plot file.
#'
#' @param title A character string used to entitle the plot.
#'
#' @param xlab A character string used to label the \eqn{x}-axis.
#'
#' @param ylab A character string used to label the \eqn{y}-axis.
#'
#' @param ft A text string indicating the \code{FlowerType} of focus.
#'
#' @param out A character string used to identify the directory into which
#'   output is to be saved.  The directory should have a trailing slash
#'   "\code{/}."
#'
#' @param year The year of focus.  Typically one of \code{2017} or \code{2018}.
#'
#' @param height A number indicating the height of the resulting \code{png}, in
#'   inches.
#'
#' @param width A number indicating the width of the resulting \code{png}, in
#'   inches.
#'
#' @param letters Default is \code{TRUE}.
#'
#' @param bounds Default is \code{FALSE}.
#'
#' @param text The size of the output font.  Larger means bigger.  Default is
#'   \code{11}.
#'
#' @details Function \code{lettered} ensures that "the letters" appear above
#'   bars representing means.  The letters indicate the groupings that are
#'   statistically similar. This plot emulates the example plot from Garbuzov &
#'   Ratnieks (2014).
#'
#' @return A plot with Tukey lettering, assuming \code{letters} is set to
#'   \code{TRUE}.
#'
#' @references Garbuzov and Ratnieks.  2014.


lettered <- function(df,here,fileSave,title,xlab,ylab,ft,out,year,height,width,letters=TRUE,bounds=FALSE,text=11){

  ## df <- totalstandarddates
  ## fileSave <- "2017 Standard Annual Dates -- Pollinator Means & Confidence Bounds"
  ## here <- "Standards"
  ## title <- ""
  ## xlab <- "Cultivar Comparisons"
  ## ylab <- "Visits / Sampling Period"
  ## ft <- c("Marigolds","Zinnia","Echinacea","Nepeta")
  ## out <- out
  ## year <- 2017
  ## height <- 12
  ## width <- 12
  ## letters <- FALSE
  ## bounds <- TRUE
  ## text <- 11



  ## Define directory where Tukey letters are.
  dir <- paste0(out,year," ",here,"/")

  ## Construct means per pollinator.
  if(letters){

    ## Get Tukey letters.  If Annuals, fool into thinking FlowerType is Cultivar, since we have that coded already.
    if(here == "Annuals"){
      tlets <- read.csv(paste0(dir,"tukeyLettersAnnuals",year,".csv")) %>%
          ##dplyr::mutate(FlowerType = as.character(droplevels(.data$FlowerType))) %>%
        dplyr::mutate(FlowerType = as.character((.data$FlowerType))) %>%
        dplyr::rename(Cultivar = .data$FlowerType)

      df <- df %>%
        dplyr::select(-.data$Cultivar) %>%
        dplyr::rename(Cultivar = .data$FlowerType) %>%
        dplyr::mutate(Cultivar = as.character(droplevels(.data$Cultivar))) %>%
        dplyr::mutate(FlowerType = .data$Cultivar)
    } else {
      tlets <- utils::read.csv(paste0(dir,"tukeyLettersPerennials",year,".csv")) %>%
        mutate(Cultivar = as.character(droplevels(.data$Cultivar)))
    }

    means <- df %>%
      dplyr::filter(.data$FlowerType %in% ft) %>%
      dplyr::group_by(.data$Cultivar) %>%
      dplyr::summarize(`Pollinator Visits` = mean(.data$TotalPollinatorVisits/.data$SamplingPeriods),
                       Syrphidae = mean(.data$TotalSyrphidae/.data$SamplingPeriods),
                       `Apis Mellifera` = mean(.data$TotalApisMellifera/.data$SamplingPeriods),
                       `Bombus Impatiens` = mean(.data$TotalBombusImpatiens/.data$SamplingPeriods),
                       `Other Diptera`	= mean(.data$TotalOtherDiptera/.data$SamplingPeriods),
                       `Other Hymenoptera` = mean(.data$TotalOtherHymenoptera/.data$SamplingPeriods)) %>%
                       ## `Other Bees` = mean(.data$TotalOtherBees/.data$SamplingPeriods),
                       ## Wasps = mean(.data$TotalWasps/.data$SamplingPeriods)) %>%
      dplyr::arrange(dplyr::desc(.data$`Pollinator Visits`)) %>%
      dplyr::left_join(tlets,by=c("Cultivar")) %>%
      dplyr::filter(.data$`Pollinator Visits` > 0) %>%
      dplyr::select(-.data$`Pollinator Visits`) %>%
      tidyr::gather(.data$Syrphidae,.data$`Apis Mellifera`,.data$`Bombus Impatiens`,.data$`Other Diptera`,
                    .data$`Other Hymenoptera`,
                    ##.data$`Other Bees`,.data$Wasps,
                    key="Pollinators",
                    value="Mean")
  } else {
    means <- df %>%
      dplyr::filter(.data$FlowerType %in% ft) %>%
      dplyr::group_by(.data$Cultivar) %>%
      dplyr::summarize(`Pollinator Visits` = mean(.data$TotalPollinatorVisits/.data$SamplingPeriods),
                       Syrphidae = mean(.data$TotalSyrphidae/.data$SamplingPeriods),
                       `Apis Mellifera` = mean(.data$TotalApisMellifera/.data$SamplingPeriods),
                       `Bombus Impatiens` = mean(.data$TotalBombusImpatiens/.data$SamplingPeriods),
                       `Other Diptera`	= mean(.data$TotalOtherDiptera/.data$SamplingPeriods),
                         `Other Hymenoptera` = mean(.data$TotalOtherHymenoptera/.data$SamplingPeriods)) %>%
                       ## `Other Bees` = mean(.data$TotalOtherBees/.data$SamplingPeriods),
                       ## Wasps = mean(.data$TotalWasps/.data$SamplingPeriods)) %>%
      dplyr::arrange(desc(.data$`Pollinator Visits`)) %>%
      ## dplyr::left_join(tlets,by=c("Cultivar")) %>%
      dplyr::filter(.data$`Pollinator Visits` > 0) %>%
      dplyr::select(-.data$`Pollinator Visits`) %>%
      tidyr::gather(.data$Syrphidae,.data$`Apis Mellifera`,.data$`Bombus Impatiens`,.data$`Other Diptera`,
                    .data$`Other Hymenoptera`,
                    ##.data$`Other Bees`,.data$Wasps,
                    key="Pollinators",
                    value="Mean")
  }

  ## Sum up Mean by Cultivar so we can sort by descending total correctly.
  meanSums <- means %>%
    dplyr::group_by(.data$Cultivar) %>%
    dplyr::summarize(SumMean = sum(.data$Mean)) %>%
    dplyr::arrange(dplyr::desc(.data$SumMean)) %>%
    dplyr::mutate(SortNew = seq(1,length(unique(.data$Cultivar))))

  ## Bring in meanSums so we can sort by it.
  means <- means %>%
    dplyr::left_join(meanSums,by="Cultivar") %>%
    dplyr::arrange(dplyr::desc(.data$SumMean),.data$Pollinators)

  ## Get vector of values so we can find max on y-axis.
  yM <- means %>%
    dplyr::group_by(.data$Cultivar) %>%
    dplyr::summarize(SumMean = sum(.data$Mean))

  ## Read in confidence bounds, if desired.  If we do this, recalculate yM.
  if(here %in% c("Standards","Perennials","StandardsMums")){
    bnds <- utils::read.csv(paste0(dir,tolower(here),"Means",year,".csv")) %>%
      dplyr::mutate(Cultivar = as.character(droplevels(.data$Cultivar))) %>%
      dplyr::filter(.data$FlowerType %in% ft)
  } else {
    bnds <- utils::read.csv(paste0(dir,tolower(here),"MeansAll",year,".csv")) %>%
        ##dplyr::mutate(Cultivar = as.character(droplevels(.data$Cultivar))) %>%
      dplyr::select(-.data$FlowerType)
  }

  ugh <- means %>%
    dplyr::left_join(bnds,by=c("Cultivar"))

  ## Get vector of values so we can find bnds max on y-axis.  Call it SumMean
  ## so it matches the previous yM.  Easier in ggplot below.
  if(bounds == TRUE){
    yM <- bnds %>%
      dplyr::summarize(SumMean = max(.data$hi97.5))
  }



    if(bounds){
        ## letters above bounds
        ugh$letterY <- ugh$hi97.5
    }else{
        ## letters above box
        ugh$letterY <- ugh$SumMean
    }
  ## Plot stacked bars with Tukey letters.
  p4 <- suppressWarnings(ggplot2::ggplot(data=ugh,ggplot2::aes(x=stats::reorder(Cultivar,SortNew),y=Mean,fill=Pollinators)) +
    ggplot2::geom_bar(stat="identity",color="black") +
    ggplot2::labs(title=title,x=xlab,y=ylab) +
    ggplot2::ylim(0,1.1*max(yM$SumMean)) +
    ##ggplot2::scale_fill_discrete(name='')+
    {if(letters) ggplot2::annotate(geom="text",
                      x=unique(ugh$Sort),
                      y=(ugh[!duplicated(ugh$Cultivar),]$letterY)+.01,
                      label=paste0(ugh[!duplicated(ugh$Cultivar),]$Letter),
                      color="black",
                      fontface=2,
                      size=12)} +
    {if(bounds) ggplot2::geom_errorbar(ggplot2::aes(ymin=ugh$lo2.5,ymax=ugh$hi97.5),width=0.2,position="identity",width=1)} + ##ggplot2::position_dodge(0.9))} +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,hjust=1,vjust=0.5),text=ggplot2::element_text(size=text)))

    grDevices::png(paste0(dir,fileSave,".png"),res=400,height=height,width=width,units="in")
    graphics::plot(p4)
    grDevices::dev.off()


}## end lettered function
