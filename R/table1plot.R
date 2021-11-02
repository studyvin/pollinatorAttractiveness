#' @export
#'
#' @title Create a Table-1-like statistical summary of experimental results.
#'
#' @description Create a Table-1-like statistical summary of experimental
#'   results.
#'
#' @param df A \code{data.frame} containing columns for \code{PollinatorVisits},
#'   \code{Cultivar}, \code{SamplingPeriods}, and \code{FlowerType}.  Typically,
#'   \code{Cultivar} is nested within \code{FlowerType}.
#'
#' @param factor The variable of factor-type for whose levels form the
#'   subsequent rows in the final table. This is typically \code{"Cultivar"}.
#'
#' @param anc The grouping variable into which the \code{factor} variable
#'   partitions.  This is typically \code{"FlowerType"}.
#'
#' @details Function \code{table1plot} helps to calculate a Table 1-like set of
#'   statistical results; i.e., simple metrics including mean, variance, and so
#'   on.
#'
#'   Contrary to its name, \code{table1plot} doesn't actually plot anything.
#'
#' @return A data frame, with each row containing summary statistics for each
#'   \code{factor}.

table1plot <- function(df,factor,anc){

  # df <- totalannual
  # factor <- "Cultivar"
  # anc <- "FlowerType"

  if(is.factor(dplyr::pull(df[,factor]))){
    ff <- as.character(droplevels(unique(dplyr::pull(df[,factor]))))
  }

  rows <- NULL
  for(f in ff){
    if(!is.null(anc)){
      anc2 <- unique(df[df[,factor] == f,anc])
    }

    #   ---- Make sure we have variability in the response.
    if(all(df[df[,factor] == f,]$TotalPollinatorVisits == df[df[,factor] == f,]$TotalPollinatorVisits[1])){

      if(!is.null(anc)){
        row <- data.frame(thing=f,anc=anc2,
                          b0=df[df[,factor] == f,]$TotalPollinatorVisits[1],
                          lo2.5=df[df[,factor] == f,]$TotalPollinatorVisits[1],
                          hi97.5=df[df[,factor] == f,]$TotalPollinatorVisits[1],
                          stringsAsFactors=FALSE)
      } else {
        row <- data.frame(thing=f,b0=df$TotalPollinatorVisits[1],
                          lo2.5=df$TotalPollinatorVisits[1],
                          hi97.5=df$TotalPollinatorVisits[1],
                          stringsAsFactors=FALSE)
      }
    } else {
      m <- stats::glm(TotalPollinatorVisits ~ 1 + offset(log(SamplingPeriods)),data=df[df[,factor] == f,],family=stats::quasipoisson)
      mean <- exp(coef(m))
      bnds <- exp(confint(m))
      if(!is.null(anc)){
        row <- data.frame(thing=f,anc=anc2,b0=mean,lo2.5=bnds[1],hi97.5=bnds[2],stringsAsFactors=FALSE)
      } else {
        row <- data.frame(thing=f,b0=mean,lo2.5=bnds[1],hi97.5=bnds[2],stringsAsFactors=FALSE)
      }
      rm(m,mean,bnds)
    }
    rows <- rbind(rows,row)
    rm(row)
  }

  if(!is.null(anc)){
    names(rows)[names(rows) == "thing"] <- factor
    rows <- rows[order(rows[,"b0"],decreasing=TRUE),]
    rows <- rows[order(rows[,anc]),]
  } else {
    rows <- rows[order(rev(rows[,"b0"])),]
  }
  row.names(rows) <- NULL
  return(rows)
}
