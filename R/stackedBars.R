#' @export
#'
#' @title Create a stacked bar chart of means bounded below by zero.
#'
#' @description Create a stacked bar chart of means bounded below by zero.
#'
#' @param df A \code{data.frame} containing columns for \code{FlowerType},
#'   \code{Variety}, \code{b0}, \code{lo2.5}, and \code{hi97.5},
#'   \code{Captured}, and \code{Observed}.  Typically the output from function
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
#' @param text The size of the output font.  Larger means bigger.  Default is
#'   \code{11}.
#'
#' @details This function creates a \code{png} via \code{ggplot2}.
#'
#' @return A \code{ggplot2}-styled stacked bar chart, with bars oriented horizontally.
#'
#' @author Jason Mitchell
#'
#' @seealso \code{ggplot2::ggplot} and friends, \code{forcats}.
#'
stackedBars <- function(df,fileSave,title,xlab,ylab,dir,height,width,text=11){

  # df <- perennialMeans2
  # fileSave <- "2017 Perennials -- Visits per Sampling Period by Insect Status"
  # title <- ""
  # xlab <- "Visits / Sampling Period"
  # ylab <- "Flower Type - Cultivar"
  # dir <- "/"
  # height <- 6
  # width <- 8
  # text <- 11

  #   ---- Get data ready to go for stacked bars.
  df1 <- df %>%
    tidyr::gather(variable, value, .data$Captured:.data$Observed) %>%
    dplyr::mutate(`Flower Type - Cultivar` = paste0(.data$FlowerType," - ",.data$Cultivar))

  #   ---- Get data ready to go ... kinda ... for confidence bounds.  Not
  #   ---- currently implemented / used.
  # df2 <- df %>%
  #   tidyr::gather(variable, value, Captured:Observed) %>%
  #   dplyr::filter(.data$variable == "Captured")

  p <- df1 %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$`Flower Type - Cultivar`, y = .data$value, fill = .data$variable)) +
    ggplot2::geom_bar(stat = "identity",color="black") +
    # ggplot2::geom_errorbar(ggplot2::aes(ymin=lo2.5,ymax=hi97.5),width=0.2,position=ggplot2::position_dodge(0.9)) +
    # ggplot2::coord_flip() +
    ggplot2::labs(title=title,x=ylab,y=xlab,fill="Insect Status") +
    ggplot2::guides(lines=FALSE) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,hjust=1,vjust=0.5),text=ggplot2::element_text(size=text))

  grDevices::png(paste0(dir,fileSave,".png"),res=400,height=height,width=width,units="in")
  print(p)
  # theme_classic() +
  # ggplot2::scale_fill_manual(values=c('#999999','#E69F00'))
  grDevices::dev.off()

}
