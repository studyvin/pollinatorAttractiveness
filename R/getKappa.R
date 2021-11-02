#' @export
#'
#' @description Calculate the condition number of a set of covariates.
#'
#' @title  Calculate the condition number of a set of covariates.
#'
#' @param x A character string containing covariates present in \code{df},
#'   separated by \code{" + "}.
#'
#' @param df A dataframe containing the data to be used for subsequent modeling.
#'
#' @details Included covariates stored as class \code{factor} within \code{df}
#'   are expanded via function \code{model.matrix} via the use of reference-cell
#'   coding. Thus, a \eqn{k}-level factor contributes \eqn{k - 1} variables in
#'   the design matrix ultimately used in modeling.
#'
#'   Currently, the function does not check if each variable within \code{x} is
#'   actually contained within \code{df}.
#'
#' @return The condition number, a number within the range \eqn{1,\infty)}.
#'
#' @references Belsley, D.A., Kuh, E., and Welsch, R. E. (1980).  Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity.  New
#' York, NY: John Wiley \& Sons, Inc.
#'
#' @author WEST, Inc.
getKappa <- function(x,df){

  # x <- mStrings$modelRaw[3]
  # df <- df

  #   ---- Convert the df to a one-row vector.  Needed when testing and
  #   ---- outside of the apply framework.
  if(class(x) == "data.frame"){
    n <- names(x)
    x <- as.character(x)
    names(x) <- n
  }

  f <- stats::as.formula(paste0("~",x))

  #   ---- Get the variables of interest by themselves, and force true df.
  if("sf" %in% class(df)){
    df$geometry <- NULL
  }
  datX <- df[,colnames(df) %in% unlist(strsplit(x,"+",fixed=TRUE))]

  if(class(datX)[1] %in% c("integer","numeric","character")){
    datX <- data.frame(as.matrix(datX,ncol=1))
    names(datX) <- x
  }

  #   ---- Calculate the design matrix;  necessary for factors.
  X <- stats::model.matrix(f,data=datX)
  if(colnames(X)[1] == "(Intercept)" & ncol(X) == 2){
    X <- X[,-1]
    # message("I removed the intercept from the calculation of the condition number.\n")
  }

  #   ---- Get the condition number.  Exclude the intercept.
  CN <- tryCatch(kappa(X),
                 error=function(cond) cond,
                 warning=function(cond) cond)
  return(CN)
}
