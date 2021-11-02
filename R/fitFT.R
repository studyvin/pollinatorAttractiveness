#' @export
#'
#' @title Conduct an analysis-of-deviance for the MSU pollinator analysis.
#'
#' @description Conduct an analysis-of-deviance for the MSU pollinator analysis
#'   assuming a Poisson-distributed outcome and varying exposure.
#'
#' @param df A \code{data.frame} containing columns for \code{TotalPollinatorVisits},
#'   \code{Cultivar}, \code{SamplingPeriods}, and \code{FlowerType}.  Typically,
#'   \code{Cultivar} is nested within \code{FlowerType}.
#'
#' @param year The numeric four-digit year.  Used to handle slight differences
#'   in recorded data layout.
#'
#' @param offsetVar The variable to be utilized as an offset, submitted as a
#'   character string.  Typically \code{SamplingPeriods}.  Default is
#'   \code{NULL}, or no offset.
#'
#' @param CNMax The maximum condition number to serve as a cutoff, beyond which
#'   models are not considered.
#'
#' @details Function \code{fitFT} serves as a preparation and wrapper function
#'   for function \code{fits} which conducts the statistical fitting via either
#'   of \code{glm} or \code{MASS::glm.nb}.
#'
#'   Processing includes the creation of a binary explanatory variables for
#'   consideration of different cutpoints.  Cutpoints are binary, in that
#'   cultivar records greater than or equal to a predefined number of
#'   \code{TotalPollinatorVisits} are grouped as \code{1}, while all others (so
#'   those less than \code{TotalPollinatorVisits}) are \code{zero}.  All
#'   possible binary outcomes from \code{1} through \code{10} are considered.
#'
#'   Following the creation of appropriate data sets, R-type formula strings are
#'   then created, possibly including \code{Cultivar} as an additional
#'   explanatory covariate via function \code{modelStrings}.  Up to 5
#'   explanatory covariates are allowed, with possibly two interactions.
#'
#'   Next, given the model strings, design matrices are constructed so that
#'   condition numbers can be calculated for each.  The condition number of the
#'   underlying variance-covariance matrix of the centered design matrix allows
#'   for interpretation of the multicollinearity of the variables whose columns
#'   form the original design matrix.  Condition numbers are ratios of the
#'   largest eigenvalue to the smallest eigenvalue, which themselves are
#'   estimates of variance in rotated dimensions in which they matter the most.
#'   More practically, the presence of a large condition number implies a more
#'   ellipsoidal data cloud when compared to clouds tied to smaller condition
#'   numbers.  The ellipsoidal nature of the cloud is very similar to
#'   ellipsoidal clouds observed in two-dimensional data plots used to assess
#'   two-dimensional correlation.  The condition-number approach is the
#'   generalization of the typical approach used for two dimensions to assess
#'   correlation.
#'
#'   Those model strings with a sufficiently low condition number are then
#'   passed to the \code{fits} function, which then conducts the actual
#'   fitting of count statistical models.
#'
#' @return A \code{list} containing results of the original fit, adjusted
#'   Tukey-like pairwise multiple comparisons, and the analysis-of-deviance.
#'   The four constituent objects are \enumerate{
#' }
#'
#' @author Jason Mitchell, based on original work by Zoe Gustafson.
#'
#' @seealso \code{stats::glm}, \code{stats::anova}, \code{stats::confint},
#'   \code{multcomp::glht}
#'
fitFT <- function(df,year,offsetVar=NULL,CNMax=10){

  # df <- readr::read_csv(paste0(path2017,"/2017ChrysanthemumR.csv"))
  # year <- 2017
  # offsetVar <- "SamplingPeriods"
  # CNMax <- 10

  # Modeling.
  if(year == 2017){
    df <- df %>% #readr::read_csv(paste0(path2018,"/",csv)) %>%
      dplyr::mutate(Cultivar=as.factor(.data$Cultivar)) %>%
      dplyr::select(.data$FlowerType,.data$Cultivar,.data$TotalPollinatorVisits,.data$SamplingPeriods) %>%
      dplyr::select_if(function(x){!all(is.na(x))}) %>%
      dplyr::mutate(z0_1z = ifelse(.data$TotalPollinatorVisits >= 1,1,0)) %>%
      dplyr::mutate(z1_2z = ifelse(.data$TotalPollinatorVisits >= 2,1,0)) %>%
      dplyr::mutate(z2_3z = ifelse(.data$TotalPollinatorVisits >= 3,1,0)) %>%
      dplyr::mutate(z3_4z = ifelse(.data$TotalPollinatorVisits >= 4,1,0)) %>%
      dplyr::mutate(z4_5z = ifelse(.data$TotalPollinatorVisits >= 5,1,0)) %>%
      dplyr::mutate(z5_6z = ifelse(.data$TotalPollinatorVisits >= 6,1,0)) %>%
      dplyr::mutate(z6_7z = ifelse(.data$TotalPollinatorVisits >= 7,1,0)) %>%
      dplyr::mutate(z7_8z = ifelse(.data$TotalPollinatorVisits >= 8,1,0)) %>%
      dplyr::mutate(z8_9z = ifelse(.data$TotalPollinatorVisits >= 9,1,0)) %>%
      dplyr::mutate(z9_10z = ifelse(.data$TotalPollinatorVisits >= 10,1,0)) # %>%
      # group_by(z0_1z) %>%
      # summarize(mean(TotalPollinatorVisits))
    # summary(glm(TotalPollinatorVisits ~ z0_1z,data=df,family=poisson))
  } else {
    df <- df %>% #readr::read_csv(paste0(path2018,"/",csv)) %>%
      dplyr::mutate(Cultivar=as.factor(.data$Cultivar)) %>%
      dplyr::select(.data$FlowerType,.data$Cultivar,.data$TotalPollinatorVisits,.data$SamplingPeriods,.data$SpidersPerSP,.data$FlowerArea,.data$Color) %>%
      dplyr::select_if(function(x){!all(is.na(x))}) %>%
      dplyr::mutate(FlowerArea = .data$FlowerArea/100) %>%
      dplyr::mutate(z0_1z = ifelse(.data$TotalPollinatorVisits >= 1,1,0)) %>%
      dplyr::mutate(z1_2z = ifelse(.data$TotalPollinatorVisits >= 2,1,0)) %>%
      dplyr::mutate(z2_3z = ifelse(.data$TotalPollinatorVisits >= 3,1,0)) %>%
      dplyr::mutate(z3_4z = ifelse(.data$TotalPollinatorVisits >= 4,1,0)) %>%
      dplyr::mutate(z4_5z = ifelse(.data$TotalPollinatorVisits >= 5,1,0)) %>%
      dplyr::mutate(z5_6z = ifelse(.data$TotalPollinatorVisits >= 6,1,0)) %>%
      dplyr::mutate(z6_7z = ifelse(.data$TotalPollinatorVisits >= 7,1,0)) %>%
      dplyr::mutate(z7_8z = ifelse(.data$TotalPollinatorVisits >= 8,1,0)) %>%
      dplyr::mutate(z8_9z = ifelse(.data$TotalPollinatorVisits >= 9,1,0)) %>%
      dplyr::mutate(z9_10z = ifelse(.data$TotalPollinatorVisits >= 10,1,0))

    if(df$FlowerType[1] == "Impatiens"){
      df <- df %>% dplyr::select(-.data$SpidersPerSP,-.data$Color)
    }
  }

  #   ---- Build data frame of results for Poisson fits.
  rowP <- data.frame(possibleCovars=paste0(colnames(df)[!colnames(df) %in% c("FlowerType","TotalPollinatorVisits","SamplingPeriods")],collapse=","),
                    outcomeFormula="TotalPollinatorVisits",
                    baseFormula="",
                    ziFormula="",
                    fitFunction="stats::glm",
                    family="quasipoisson",
                    flowerType=df$FlowerType[1],
                    stringsAsFactors=FALSE)

  #   ---- Build data frame of results for negative binomial fits.
  rowNB <- data.frame(possibleCovars=paste0(colnames(df)[!colnames(df) %in% c("FlowerType","TotalPollinatorVisits","SamplingPeriods")],collapse=","),
                     outcomeFormula="TotalPollinatorVisits",
                     baseFormula="",
                     ziFormula="",
                     fitFunction="MASS::glm.nb",
                     family="negbin",
                     flowerType=df$FlowerType[1],
                     stringsAsFactors=FALSE)

  #   ---- Build up model strings of interest.
  mStrings <- makeModelStrings(rowP,K=5)

  #   ---- Calculate the condition number of the data.
  CN <- sapply(mStrings$modelRaw,function(x) getKappa(x,df))

  #   ---- Conduct the fits with an appropriate CN.
  stuffP <- fits(mStrings,CN,CNMax,rowP,df,offsetVar,saveModels=FALSE)
  stuffNB <- fits(mStrings,CN,CNMax,rowNB,df,offsetVar,saveModels=FALSE)
  return(list(poisson=stuffP,negbin=stuffNB))
}
