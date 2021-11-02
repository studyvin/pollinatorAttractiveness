#' @export
#'
#' @title Estimate \code{Cultivar} means via use of Poisson regression.
#'
#' @description Estimate \code{Cultimar} means via use of Poisson regression,
#'   with any overdispersion handled via quasilikelihood.
#'
#' @param df A \code{data.frame} containing columns for \code{PollinatorVisits},
#'   \code{Cultivar}, \code{SamplingPeriods}, and \code{FlowerType}.
#'
#' @details Function \code{getPoissonMeans} runs a simple intercept-only model
#'   for each \code{Cultivar} found within \code{df}.  It also incorporates
#'   offsets via \code{SamplingPeriods}.  This allows application of function
#'   \code{stats::confint} so that resulting 95\% confidence intervals are fit
#'   on the log-scale.  Practically, this ensures they do not cross zero.
#'
#'   Because function \code{getPoissonMeans} is geared towards estimate Poisson
#'   means, no consideration of overdispersion is made.
#'
#' @return A \code{data.frame} containing estimates of means and 95\% confidence
#'   intervals.  In the case a \code{Cultivar} is all-zero, the confidence
#'   interval is returned as \code{NA}.
#'
#' @author Jason Mitchell, based on original work by Zoe Gustafson.
#'
#' @seealso \code{stats::glm}, \code{stats::coef}, \code{stats::confint}
#'
getPoissonMeans <- function(df){

  # df <- totalperennial

  V <- unique(as.character(df$Cultivar))
  allRows <- NULL
  for(v in V){

    #   ---- Pluck out the lil dataframe for this Variety and estimate mean.
    lildf <- df[df$Cultivar == v,]
    # wgts <- lildf %>%
    #   group_by(SamplingPeriod) %>%
    #   dplyr::count(SamplingPeriod)

    #   ---- Make sure we have variability in the response.
    if(all(lildf$TotalPollinatorVisits == lildf$TotalPollinatorVisits[1])){

      thisRow <- data.frame(FlowerType=lildf$FlowerType[1],
                            Cultivar=v,
                            b0=lildf$TotalPollinatorVisits[1],
                            lo2.5=NA,
                            hi97.5=NA,
                            ##Captured=0,
                            ##Observed=0,
                            stringsAsFactors=FALSE)
    } else {

      fit <- stats::glm(TotalPollinatorVisits ~ 1 + offset(log(SamplingPeriods)),data=lildf,family=stats::poisson)

      #   ---- Check out if we have varying exposures.
      # if(!(all(lildf$SamplingPeriod != lildf$SamplingPeriod[1]))){
      #   message("I see non-constant Sampling Period for Variety, ",v,".  That right?\n")
      # }

      #   ---- Get estimates and confidence intervals on original scale.
      b0 <- exp(stats::coef(fit))
      b0conf <- exp(stats::confint(fit))

      #   ---- Calculate raw estimates of visits per sampling period for each captured and observed.
      ## captured <- lildf %>%
      ##   dplyr::mutate(CapturedAll=purrr::reduce(dplyr::select(.,dplyr::contains("Collected")), `+`)) %>%
      ##   dplyr::select(.data$FlowerType,.data$CapturedAll,.data$SamplingPeriods) %>%
      ##   dplyr::summarize(CapturedSum=sum(.data$CapturedAll)/sum(.data$SamplingPeriods))
      ##   # as.data.frame(stringsAsFactors=FALSE)

      ## observed <- lildf %>%
      ##   dplyr::mutate(ObservedAll=purrr::reduce(dplyr::select(.,dplyr::contains("Observed")), `+`)) %>%
      ##   dplyr::select(.data$FlowerType,.data$ObservedAll,.data$SamplingPeriods) %>%
      ##   dplyr::summarize(ObservedSum=sum(.data$ObservedAll)/sum(.data$SamplingPeriods))
      ##   # as.data.frame(stringsAsFactors=FALSE)

      thisRow <- data.frame(FlowerType=lildf$FlowerType[1],
                            Cultivar=v,
                            b0=b0,
                            lo2.5=b0conf[1],
                            hi97.5=b0conf[2],
                            ##Captured=captured$CapturedSum,
                            ##Observed=observed$ObservedSum,
                            stringsAsFactors=FALSE)
    }
    allRows <- rbind(allRows,thisRow)
  }
  rownames(allRows) <- NULL
  allRows <- allRows[!is.na(allRows$b0),]
  allRows[is.na(allRows$lo2.5),]$lo2.5 <- allRows[is.na(allRows$lo2.5),]$b0
  allRows[is.na(allRows$hi97.5),]$hi97.5 <- allRows[is.na(allRows$hi97.5),]$b0
  allRows <- allRows %>% dplyr::arrange(.data$FlowerType,dplyr::desc(.data$b0))
  return(allRows)
}
