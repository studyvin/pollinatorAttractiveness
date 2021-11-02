#' @export
#'
#' @title Massage a provided vector of intended model character strings into the
#'   format necessary for a \code{formula}-class object.
#'
#' @description Massage a provided vector of intended model character strings
#'   into the format necessary for a \code{formula}-class object.
#'
#' @param var The character vector of data intended to be used and formatted as
#'   "clean" formula strings (as least in terms of the R-object
#'   \code{formula}-class.)
#'
#' @details As coded here, the \code{out} object is assumed to possible contain
#'   a "hyphen," serving as a minus sign, as well as commas, due to typical
#'   syntax involving fits against the \code{binomial} family and associated
#'   \code{cbind} usage.  It is possible that the syntax of replacement steps
#'   performed here is not ideal for your situation.
#'
#' @return The original character vector \code{var} with slashes "\code{/}"
#'   replaced with a single underscore "\code{_}," hyphens "\code{-}" replaced
#'   with a double underscore "\code{__}," and commas "{,}" replaced with a
#'   triple underscore "\code{___}."
#'
#' @references Wilkinson G.N., and Rogers C.E. (1973). "Symbolic Description of
#'   Factorial Models for Analysis of Variance." Applied Statistics, 22, 392-399
#'
#' @author WEST, Inc.
#'
cleanUpFormula <- function(var){

  # var <- allJ$modelPretty
  # out <- outcomeFormula

  var <- gsub("/","_",var,fixed=TRUE)         # Replace slashes with single underscore _.
  var <- gsub("-","__",var,fixed=TRUE)        # Replace hyphens with double underscore __.
  var <- gsub(",","___",var,fixed=TRUE)       # Replace commas with triple underscore ___.
  var <- gsub(" ","",var,fixed=TRUE)          # Remove spaces.
  var <- var                                  # Replace leading numbers with.  (Doesn't apply in current application.)
  return(var)
}
