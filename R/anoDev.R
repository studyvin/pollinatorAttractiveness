#' @export
#'
#' @title Conduct an analysis-of-deviance for the MSU pollinator analysis.
#'
#' @description Conduct an analysis-of-deviance for the MSU pollinator analysis
#'   assuming a Poisson-distributed outcome and varying exposure.
#'
#' @param df A \code{data.frame} containing columns for \code{PollinatorVisits},
#'   \code{Cultivar}, \code{SamplingPeriods}, and \code{FlowerType}.  Typically,
#'   \code{Cultivar} is nested within \code{FlowerType}.
#'
#' @param ft A \code{FlowerType} within the Perrenial dataset.
#'
#' @param annuals A \code{logical} indicating if annuals or perennials are the
#'   analysis type of focus.  Default is \code{TRUE}.
#'
#' @param blocks A \code{logical} indicating if a random block effect shoud be
#'   included.  Requires the presence of a \code{Block} variable.  Default is
#'   \code{TRUE}.
#'
#' @details Function \code{stats::glm} performs the analysis-of-deviance via use
#'   of the \code{quasipoisson} family, and thus \eqn{log} link. Following
#'   initial fit, a chi-squared test is used to test via deviance.  This
#'   generalized-linear-model set-up corresponds to the typical partial
#'   \eqn{F}-test conducted via residual error in the general-linear-model
#'   normal-outcome case.
#'
#'   Note that the use of the \code{quasipoisson} family provides a conservative
#'   estimate of underlying test statistics.  This is due to the
#'   \code{quasipoisson} correcting for observed overdispersion in
#'   \code{poisson}-model fits.
#'
#' @return A \code{list} containing results of the original fit, adjusted
#'   Tukey-like pairwise multiple comparisons, and the analysis-of-deviance.
#'   The four constituent objects are \enumerate{
#'
#' \item \code{a} The resulting \code{data.frame} housing the statistical
#' results of the analysis of deviance.  Recall that the degrees of freedom
#' reports numbers one less than the distinct groups tested.  The statistic
#' entitled \code{phi} is the (Pearson's chi-squared) overdispersion.  These
#' statistics are corrected for quasilikelihoo.
#'
#' \item \code{c} The Tukey-lettering system identifying how individual levels
#' compare.  Utilized \code{multcomp:cld}.
#'
#' \item \code{i} The inverted confidence intervals on the original scales of
#' the pairwise estimates.
#'
#' \item \code{m} The original \code{glm} fit, assuming a Poisson distrubtion
#' and possible offset.
#'
#' \item \code{t} The original mutliple-comparison \code{glht} fit, assuming
#' Tukey-like pairwise comparisons.
#'
#' }
#'
#' @author Jason Mitchell, based on original work by Zoe Gustafson.
#'
#' @seealso \code{stats::glm}, \code{stats::anova}, \code{stats::confint},
#'   \code{multcomp::glht}
#'
anoDev <- function(df,ft,annuals=TRUE,blocks=TRUE){

  # df <- totalannual
  # ft <- "Begonia" #c("Begonia","Geranium","Impatiens","NGImpatiens","Pansy","Petunia")
  # annuals <- TRUE
  # blocks <- FALSE

  #   ---- Fit the analysis of deviance.
  if(annuals == TRUE){
    if(blocks == TRUE){
      m <- lme4::glmer(TotalPollinatorVisits ~ 1 + Cultivar + (1 | Block),data=df[df$FlowerType == ft,],family=stats::poisson)  # Usually unsuccessful.
    } else if(length(ft) == 1){   # Annuals, one flower type at a time.  Investigating differences in cultivar within flower type.
      m <- stats::glm(TotalPollinatorVisits ~ 1 + Cultivar,data=df[df$FlowerType %in% ft,],family=stats::quasipoisson)
    } else {                      # Annuals, all flower types, ignoring cultivar.  Investigating difference in flower type.
      m <- stats::glm(TotalPollinatorVisits ~ 1 + FlowerType,data=df[df$FlowerType %in% ft,],family=stats::quasipoisson)
    }
  } else {
    if(blocks == TRUE){
      m <- lme4::glmer(TotalPollinatorVisits ~ 1 + Cultivar + (1 | Block),data=df[df$FlowerType == ft,],family=stats::poisson)  # Usually unsuccessful.
    } else {
      m <- stats::glm(TotalPollinatorVisits ~ 1 + Cultivar,data=df[df$FlowerType %in% ft,],family=stats::quasipoisson)
    }
  }

  #   ---- Fit the Tukey multicomparison.
  if(annuals == TRUE){
    if(length(ft) == 1){
      t <- multcomp::glht(m, multcomp::mcp(Cultivar="Tukey"))
    } else {
      t <- multcomp::glht(m, multcomp::mcp(FlowerType="Tukey"))
    }
  } else {
    t <- multcomp::glht(m, multcomp::mcp(Cultivar="Tukey"))
  }

  # ttt <- emmeans(m, pairwise~Cultivar)
  # confint(ttt)
  # plot(ttt$contrasts)
  # CLD(ttt)
  # cld(ttt[[1]])
  #
  # grDevices::png("L:/Jason/pollinatorAttractiveness/inst/Output/June2019/2018 Perennials/chry2018.png",res=400,height=18,width=18,units="in")
  # par(mar=c(5, 16, 4, 2) + 0.1)
  # plot(t)
  # plot(ttt$contrasts)
  # grDevices::dev.off()

  #   ---- Get pairwise p-values.
  s <- data.frame(Comparison=names(summary(t)$test$coefficients),pvalue=summary(t)$test$pvalues,stringsAsFactors=FALSE)

  #   ---- Get estimates of differences and confidence intervals on original scale.
  if(annuals == TRUE){
    if(length(ft) == 1){
      i <- exp(stats::confint(t)$confint) %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "Comparison") %>%
        dplyr::mutate(ugh = as.character(1:n())) %>%
        data.frame() %>%
        dplyr::mutate(FlowerType = ft) %>%
        dplyr::mutate(Estimate2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$Estimate)),.data$Estimate)) %>%
        dplyr::mutate(lwr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$upr)),.data$lwr)) %>%
        dplyr::mutate(upr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$lwr)),.data$upr)) %>%
        dplyr::select(-.data$Estimate,-.data$lwr,-.data$upr) %>%
        dplyr::rename(Estimate = .data$Estimate2,lwr = .data$lwr2,upr = .data$upr2) %>%
        dplyr::mutate(calpha = attr(stats::confint(t)$confint,"calpha")) %>%
        dplyr::left_join(s,by="Comparison")
    } else {
      i <- exp(stats::confint(t)$confint) %>%
        as.data.frame() %>%
          tibble::rownames_to_column(var = "Comparison") %>%
        dplyr::mutate(ugh = as.character(1:n())) %>%
        data.frame() %>%
        dplyr::mutate(FlowerType = "Annuals") %>%
        dplyr::mutate(Estimate2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$Estimate)),.data$Estimate)) %>%
        dplyr::mutate(lwr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$upr)),.data$lwr)) %>%
        dplyr::mutate(upr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$lwr)),.data$upr)) %>%
        dplyr::select(-.data$Estimate,-.data$lwr,-.data$upr) %>%
        dplyr::rename(Estimate = .data$Estimate2,lwr = .data$lwr2,upr = .data$upr2) %>%
        dplyr::mutate(calpha = attr(stats::confint(t)$confint,"calpha")) %>%
        dplyr::left_join(s,by="Comparison")
    }
  } else {
    i <- exp(stats::confint(t)$confint) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Comparison") %>%
      dplyr::mutate(ugh = as.character(1:n())) %>%
      data.frame() %>%
      dplyr::mutate(FlowerType = ft) %>%
      dplyr::mutate(Estimate2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$Estimate)),.data$Estimate)) %>%
      dplyr::mutate(lwr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$upr)),.data$lwr)) %>%
      dplyr::mutate(upr2 = ifelse(.data$Estimate < 1,exp((-1)*log(.data$lwr)),.data$upr)) %>%
      dplyr::select(-.data$Estimate,-.data$lwr,-.data$upr) %>%
      dplyr::rename(Estimate = .data$Estimate2,lwr = .data$lwr2,upr = .data$upr2) %>%
      dplyr::mutate(calpha = attr(stats::confint(t)$confint,"calpha")) %>%
      dplyr::left_join(s,by="Comparison")
  }

  #   ---- Conduct overall test.
  a <- data.frame(stats::anova(m,test="Chisq"))
  if(annuals == TRUE){
    if(length(ft) == 1){
      a$FlowerType <- ft
    } else {
      a$FlowerType <- "Annuals"
    }
  } else {
    a$FlowerType <- ft
  }
  if("glm" %in% class(m)){
    a$phi <- summary(m)$dispersion
    a$functionUsed <- "stats::glm"
  } else {
    a$phi <- stats::deviance(m)/stats::df.residual(m)
    a$functionUsed <- "stats::glm"
  }

  #   ---- Apply Tukey lettering system.
  if(annuals == TRUE){
    if(length(ft) == 1){
      return(list(a=a,m=m,t=t,i=i))
    } else {
      meanSort <- df %>%
        # dplyr::filter(FlowerType == ft) %>%
        dplyr::group_by(.data$FlowerType) %>%
        dplyr::summarize(`Pollinator Visits` = mean(.data$TotalPollinatorVisits/.data$SamplingPeriods),
                         Syrphidae = mean(.data$TotalSyrphidae/.data$SamplingPeriods),
                         `Apis Mellifera` = mean(.data$TotalApisMellifera/.data$SamplingPeriods),
                         `Bombus Impatiens` = mean(.data$TotalBombusImpatiens/.data$SamplingPeriods),
                         `Other Diptera`	= mean(.data$TotalOtherDiptera/.data$SamplingPeriods),
                         `Other Hymenoptera` = mean(.data$TotalOtherHymenoptera/.data$SamplingPeriods)) %>%
                         ##`Other Bees` = mean(.data$TotalOtherBees/.data$SamplingPeriods),
                         ##Wasps = mean(.data$TotalWasps/.data$SamplingPeriods)) %>%
        dplyr::arrange(dplyr::desc(.data$`Pollinator Visits`)) %>%
        dplyr::mutate(MeanSort = seq(1,length(unique(df$FlowerType)))) %>%
        dplyr::filter(.data$`Pollinator Visits` > 0) %>%
        dplyr::select(.data$FlowerType,.data$MeanSort)

      #   ---- Now get letters, and sort, by meanSort if necessary.
      # meanSort$FlowerType <- as.character(droplevels(meanSort$FlowerType))
      c <- sort(multcomp::cld(t,decreasing=TRUE)$mcletters$Letters)
      c <- data.frame(FlowerType=names(c),Letter=c) %>%
        dplyr::left_join(meanSort,by=c("FlowerType")) %>%
        dplyr::arrange(.data$Letter,.data$MeanSort) %>%
        dplyr::filter(!is.na(.data$MeanSort)) %>%
        dplyr::mutate(Sort = seq(1,length(.data$MeanSort)))
      rownames(c) <- NULL
      return(list(a=a,m=m,t=t,i=i,c=c))
    }
  } else {

    meanSort <- df %>%
      dplyr::filter(.data$FlowerType == ft) %>%
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
      dplyr::mutate(MeanSort = seq(1,length(unique(.data$Cultivar)))) %>%
      dplyr::filter(.data$`Pollinator Visits` > 0) %>%
      dplyr::select(.data$Cultivar,.data$MeanSort)

    #   ---- Now get letters, and sort, by meanSort if necessary.
    c <- sort(multcomp::cld(t,decreasing=TRUE)$mcletters$Letters)
    c <- data.frame(FlowerType=ft,Cultivar=names(c),Letter=c) %>%
      dplyr::left_join(meanSort,by=c("Cultivar")) %>%
      dplyr::arrange(.data$Letter,.data$MeanSort) %>%
      dplyr::filter(!is.na(.data$MeanSort)) %>%
      dplyr::mutate(Sort = seq(1,length(.data$MeanSort)))
    rownames(c) <- NULL
    return(list(a=a,m=m,t=t,i=i,c=c))
  }
}
