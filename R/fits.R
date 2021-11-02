#' @export
#'
#' @title Fit all options for a given outcome model scenario, allowing
#'   fixed-effect environmental covariates to vary.
#'
#' @description Fit all options for a given outcome model scenario, allowing
#'   fixed-effect environmental covariates to vary.
#'
#' @param mStrings A list housing various model \code{formula} strings from
#'   function \code{makeModelStrings}.
#'
#' @param CN A numeric vector of results, as output via \code{sapply} and
#'   \code{getKappa} from the \code{buildModel} wrapper function.
#'
#' @param CNMax A number specifiying the maximum condition number for which
#'   models are to be evaluated.  Default is \code{30}.
#'
#' @param offsetVar The variable to be utilized as an offset, submitted as a
#'   character string.  Typically \code{SamplingPeriods}.  Default is
#'   \code{NULL}, or no offset.
#'
#' @param row A one-row \code{data.frame}, typically originating from a
#'   \code{csv} housing desired model characteristics.  Should include at least
#'   \code{outcome}, \code{fitFunction}, \code{outcomeFormula},
#'   \code{possibleCovars}, \code{baseFormula}, \code{ziFormula}, and
#'   \code{family}.
#'
#' @param df A dataframe containing the variables necessary to fit a Piepho &
#'   Ogutu linear mixed model.
#'
#' @param saveModels A logical indicating if each individual model should be
#'   pushed as functional output.  Default is \code{FALSE}.
#'
#' @details Function \code{fits} fits the model functional form specified via
#'   \code{row}.  Included fit models are dictated via \code{mStrings} with
#'   calculated condition numbers less than \code{CNMax}, whose default is set
#'   at \code{30}.
#'
#'   Although function \code{buildModel} constructs model strings both with and
#'   without an intercept, formula manipulation to create the \code{top}
#'   dataframe currently assumes the presence of one.  Thus, manually removing
#'   the intercept via the typical \code{-1} will probably lead to an error.
#'
#'   Currently, count models always assume an offset of \eqn{log(30)} (for 30
#'   m\eqn{^2} searched per site visit).
#'
#' @return A list with several elements.
#'
#' \describe{
#' \item{good}{A list of length equal to the number of models whose condition
#' number was less than the \code{CNMax}.  Entries are direct model objects from
#' fitting, and thus contain all information possibly of value.}
#' \item{bad}{A list of length equal to the number of models whose condition
#' number was greater than the \code{CNMax}.  Entries simply report the
#' offending condition-number values.}
#' \item{mess}{A list of length equal to the length in list \code{good}.
#' Entries correspond to messages recorded during model fits.  Values of
#' \code{NULL} mean no message was recorded.  Typically, this implies model
#' convergence.}
#' \item{fitsL}{A list of length equal to the number of distinct models included
#' via \code{mStrings$modelRaw} housing results from the model fit.  In the case
#' the condition number was greater than \code{CNMax}, the number is the
#' offending condition number. Otherwise, an object of the class peculiar to the
#' model-type fit is returned.  In the case that \code{saveModels} is
#' \code{FALSE}, a simple character string is returned instead.}
#' \item{aic}{A list of length equal to the length in list \code{good}. Reported
#' numbers are the AIC, as pulled from the \code{summary} of the original model
#' fits.  Entries with \code{9999999999} generally represent models for which
#' convergence was achieved, but for which the AIC was reported to be \code{NA}.
#' This means these models could be included within the \code{good} list. }
#' \item{top}{A \code{data.frame} encapsulating all other results in a simple
#' tabulated format.  Conducive for output to \code{csv} format.}
#' }
#'
#' @seealso \code{makeModelStrings}, \code{getKappa}
#'
#' @author Jason Mitchell
#'
fits <- function(mStrings,CN,CNMax=30,row,df,offsetVar,saveModels=FALSE){

  # mStrings <- mStrings
  # CN <- CN
  # CNMax <- 10
  # row <- rowNB
  # df <- df
  # offsetVar <- "SamplingPeriods"
  # saveModels <- FALSE

  # Pluck off some stuff from row from underlying model csv.
  fitFunction <- row$fitFunction
  family <- row$family

  # Pluck off offset.
  if(!is.null(offsetVar)){
    offset <- df %>% dplyr::select(!!offsetVar) %>% as.matrix()
  }

  # Build some formulae and collect CN.
  fitsL <- vector("list",length(CN))

  # Collect AIC in the case of using a quasi family.
  if(family %in% c("quasipoisson","quasibinomial")){
    familyP <- substr(family,6,nchar(family))
    fitsP <- vector("list",length(CN))
  }

  for(j in 1:length(CN)){

    # Build necessary functional inputs.
    modelF <- stats::as.formula(mStrings$modelIntY[j])
    if(mStrings$modelZI[j] != ""){
      ziF <- stats::as.formula(mStrings$modelZI[j])
    } else {
      ziF <- ""
    }
    thisCN <- CN[j]

    # If we meet the condition-number paradigm, fit the model.
    if(thisCN < CNMax){
      if(fitFunction == "lme4::glmer"){
        fitsL[[j]] <- tryCatch(suppressWarnings(lme4::glmer(modelF,data=df,family=get(family))),error=function(cond) cond,warning=function(cond) cond)
      } else if(fitFunction == "stats::glm"){
        fitsL[[j]] <- tryCatch(suppressWarnings(stats::glm(modelF,family=get(family),offset=log(offset),data=df)),error=function(cond) cond,warning=function(cond) cond)

        # Capture AIC without overdispersion adjustment.  We'll adjust later.
        if(family %in% c("quasipoisson","quasibinomial")){
          fitsP[[j]] <- tryCatch(suppressWarnings(stats::glm(modelF,family=get(familyP),offset=log(offset),data=df)),error=function(cond) cond,warning=function(cond) cond)
        }
      } else if(family %in% c("negbin")){
        modelF2 <- as.formula(paste0(as.character(modelF)[2],as.character(modelF)[1],as.character(modelF)[3]," + offset(log(",offsetVar,"))"))
        fitsL[[j]] <- tryCatch(suppressWarnings(MASS::glm.nb(modelF2,data=df)),error=function(cond) cond,warning=function(cond) cond)
      }
    } else {
      fitsL[[j]] <- thisCN
    }
  }
  names(fitsL) <- mStrings$modelIntY
  classID <- sapply(sapply(fitsL,class),function(x) utils::head(x,1))

  #   ---- Build up objects.
  if(any(classID %in% c("glm","glmmTMB","negbin"))){
    goodL <- fitsL[classID %in% c("glm","glmmTMB","negbin")]
    convLAIC <- as.numeric(sapply(goodL,function(x) x$converged[1]))

    if(family %in% c("quasipoisson","quasibinomial")){
      goodP <- fitsP[classID %in% c("glm","glmmTMB")]
      aic <- sapply(goodP,function(x) summary(x)$aic[1])
      convPAIC <- as.numeric(sapply(goodP,function(x) x$converged[1]))
      phi <- sapply(goodL,function(x) summary(x)$dispersion[1])

      ll <- sapply(goodP,function(x) stats::logLik(x)[1])
      k <- sapply(goodP,function(x) stats::extractAIC(x)[1])
      n <- sapply(goodP,function(x) nrow(x$data))
      qaicc <- (-2*ll / phi) + (2*k*(n/(n - k - 1)))
    } else {
      goodP <- goodL
      aic <- sapply(goodL,function(x) summary(x)$aic[1])
      convPAIC <- convLAIC
      phi <- sapply(goodL,function(x) sum(stats::residuals(x, type="pearson")^2) / x$df.residual)#rep(1,length(goodL))

      ll <- sapply(goodP,function(x) stats::logLik(x)[1])
      k <- sapply(goodP,function(x) stats::extractAIC(x)[1])
      n <- sapply(goodP,function(x) length(x$residuals))
      qaicc <- (-2*ll / phi) + (2*k*(n/(n - k - 1)))
    }

    if(family == "negbin"){
      theta <- sapply(goodL,function(x) x$theta[1])
    } else {
      theta <- rep(NA,length(goodL))
    }

    bad <- unlist(fitsL[classID == "numeric"])
    names(aic) <- names(goodL)
    names(phi) <- names(goodL)
    names(qaicc) <- names(goodL)
    names(theta) <- names(goodL)

    #   ---- Put it all together for further processing.
    convdf <- data.frame(aic=aic,phi=phi,k=k,n=n,qaicc=qaicc,theta=theta,formula=names(goodL),convLAIC=convLAIC,convPAIC=convPAIC,stringsAsFactors = FALSE)
    convdf <- convdf[order(convdf$qaicc),]
    rownames(convdf) <- NULL
  }

  #   ---- Scrounge together top 20 aic.
  topaicP <- convdf$qaicc

  #   ---- Save the formula used since we use it twice in constructing top.
  theFormula <- sapply(strsplit(sapply(strsplit(convdf$formula," ~ 1 + ",fixed=TRUE),"[[",2),paste0(" + ",row$baseFormula),fixed=TRUE),"[[",1)

  #   ---- Compile a dataframe of useful results.
  top <- data.frame(flowerType=row$flowerType,
                    outcome=row$outcome,
                    family=row$family,
                    goodN=match(convdf$formula,names(goodL)),
                    CN=CN[match(theFormula,names(CN))],
                    fitFunction=row$fitFunction,
                    rank=seq(1,length(topaicP)),
                    aic=convdf$aic,
                    phi=convdf$phi,
                    n=convdf$n,
                    k=convdf$k,
                    qaicc=topaicP,
                    theta=convdf$theta,
                    baseFormula=row$baseFormula,
                    formula=theFormula,
                    ziFormula=row$ziFormula,
                    convLAIC=convdf$convLAIC,
                    convPAIC=convdf$convPAIC,
                    stringsAsFactors=FALSE)
  rownames(top) <- NULL

  if(saveModels == TRUE){
    ans <- list(goodL=goodL,goodP=goodP,bad=bad,fitsL=fitsL,aic=aic,phi=phi,theta=theta,qaicc=qaicc,top=top)
  } else {
    ans <- list(goodL=goodL,goodP=goodP,bad=bad,fitsL="Not saved.",aic=aic,phi=phi,theta=theta,qaicc=qaicc,top=top)
  }
  return(ans)
}
