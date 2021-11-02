#' @export
#'
#' @description Given a character vector of length \eqn{J} of variable names,
#'   construct all \eqn{K <= J} combinations of possible model covariate
#'   strings.
#'
#' @title Given a character vector of length \eqn{J} of variable names,
#'   construct all \eqn{K <= J} combinations of possible model covariate
#'   strings.
#'
#' @param row A \code{data.frame} of one row, containing several columns used to
#'   build a \code{formula} object.  See Details.
#'
#' @param K An integer less than or equal to the length of variables provided
#'   via \code{possibleCovars}.  Currently, only values of \code{1}, \code{2},
#'   \code{3}, \code{4}, or \code{5} are allowed.
#'
#' @param offset A character vector of variables to include as an offset.
#'   Default is \code{NULL}.  Not currently coded to do anything in the case of
#'   a non-\code{NULL}.
#'
#' @details Function \code{makeModelStrings} simply uses the \code{combn}
#'   function to select all combinations of required model covariate strings.
#'
#'   Required object \code{row} should include columns for each of
#'
#'   \describe{
#'   \item{\code{possibleCovars}}{A character vector housing the names of the
#'   different variables for which univariate models have previously been fit.}
#'   \item{\code{outcomeFormula}}{A character vector housing the name(s) of the
#'   variable used as an outcome, and intended for use within a resulting
#'   \code{formula}-type object. Can be fancier "\code{cbind}-type" character
#'   strings in the case of binomial outcomes.}
#'   \item{\code{baseFormula}}{A text string identifying any random-effect
#'   summands.  Possibly blank in the case none are desired.}
#'   \item{\code{ziFormula}}{A text string identifying any zero-inflation
#'   formulae.  Possibly blank in the case none are desired.}
#'   }
#'
#' @return A list with three objects:
#'
#'   \describe{
#'   \item{\code{modelIntY}}{A character vector housing \code{formula} objects for
#'   use in modeling with intercepts "{ + 1}" explicitly included.}
#'   \item{\code{modelIntN}}{A character vector housing \code{formula} objects for
#'   use in modeling without intercepts "{ - 1}" explicitly included.}
#'   \item{\code{modelZI}}{A character vector housing \code{formula} objects for
#'   use in modeling.  Presence or absence of intercepts dictated via underlying
#'   stored \code{csv} from which \code{row} originates.}
#'   \item{\code{modelRaw}}{A character vector housing \code{modelIntY} (or
#'   \code{modelIntN}) without an intecept summand.}
#'   }
#'
#' @author WEST, Inc.
#'
makeModelStrings <- function(row,K,offset=NULL){

  # row <- row
  # K <- 5
  # offset <- NULL

  if(!is.null(offset)){
    stop("The offset argument is currently not coded to do anything in the case of non-NULL.  Sorry.\n")
  }

  #   ---- Limit K to be <= 5.
  if(!any(K %in% seq(1,5))){
    stop("The K argument must be one of 1, 2, 3, 4, or 5.  Try again.\n")
  }

  #   ---- Pluck out goods from row.
  outcomeFormula <- row$outcomeFormula
  possibleCovars <- strsplit(row$possibleCovars,",")[[1]]
  baseFormula <- row$baseFormula
  ziFormula <- row$ziFormula

  #   ---- Two-way interactions.  Could be generalized.
  if(length(possibleCovars) > 1){
    possibleCovars2 <- apply(t(utils::combn(possibleCovars,2)),1,function(x) paste0(x[1],":",x[2]))
    possibleCovars <- c(possibleCovars,possibleCovars2)
  }

  #   ---- Define J.
  J <- length(possibleCovars)

  #   ---- Check for plausible K.
  if(K > J){
    warning("The provided value of K is greater than the number of variable J.  Redefining K to equal J.\n")
    K <- J
  }

  allJ <- NULL#data.frame(nVar=0,model="1")
  for(j in 1:K){

    if(j %in% c(1,2)){

      #   ---- We cannot have an interaction with only 1 or 2 variables, so exclude the interaction ":".
      temp.j <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j))),stringsAsFactors=FALSE)

    } else if(j == 3){

      #   ---- THREE POSSIBLE MODEL TERMS (j <- 3)
      #   ------- Get the (k choose 3) models with 0 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= j){
        temp.j3 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j))),stringsAsFactors=FALSE)
      } else {
        temp.j3 <- NULL
      }

      #   ------- Get the (k choose 2) models with 1 interaction.
      tmp2 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 1))),stringsAsFactors=FALSE)
      tmp2$X3 <- paste0(tmp2$X1,":",tmp2$X2)
      temp.j2 <- tmp2
      temp.j <- rbind(temp.j3,tmp2)
      # 1330
      suppressWarnings(rm(temp.j3,tmp2))

    } else if(j == 4){

      #   ---- FOUR POSSIBLE MODEL TERMS (j <- 4)
      #   ------- Get the (k choose 4) models with 0 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= j){
        temp.j4 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j))),stringsAsFactors=FALSE)
      } else {
        temp.j4 <- NULL
      }

      #   ------- Get the (k choose 3)*(3 choose 2) models with 1 interaction.
      tmp3 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 1))),stringsAsFactors=FALSE)
      tmp3a <- tmp3b <- tmp3c <- tmp3
      tmp3a$X4 <- paste0(tmp3$X1,":",tmp3$X2)
      tmp3b$X4 <- paste0(tmp3$X1,":",tmp3$X3)
      tmp3c$X4 <- paste0(tmp3$X2,":",tmp3$X3)
      temp.j3 <- unique(rbind(tmp3a,tmp3b,tmp3c))
      temp.j <- rbind(temp.j4,temp.j3)
      # 8265
      suppressWarnings(rm(temp.j4,tmp3,tmp3a,tmp3b,tmp3c,temp.j3))

    } else if(j == 5){

      #   ---- FIVE POSSIBLE MODEL TERMS (j <- 5)
      #   ------- Get the (k choose 5) models with 0 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= j){
        temp.j5 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j))),stringsAsFactors=FALSE)
      } else {
        temp.j5 <- NULL
      }

      #   ------- Get the (k choose 4)*(4 choose 2) models with 1 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= (j - 1)){
        tmp4 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 1))),stringsAsFactors=FALSE)
        tmp4a <- tmp4b <- tmp4c <- tmp4d <- tmp4e <- tmp4f <- tmp4
        tmp4a$X5 <- paste0(tmp4$X1,":",tmp4$X2)
        tmp4b$X5 <- paste0(tmp4$X1,":",tmp4$X3)
        tmp4c$X5 <- paste0(tmp4$X1,":",tmp4$X4)
        tmp4d$X5 <- paste0(tmp4$X2,":",tmp4$X3)
        tmp4e$X5 <- paste0(tmp4$X2,":",tmp4$X4)
        tmp4f$X5 <- paste0(tmp4$X3,":",tmp4$X4)
        temp.j4 <- unique(rbind(tmp4a,tmp4b,tmp4c,tmp4d,tmp4e,tmp4f))
      } else {
        temp.j4 <- NULL
      }

      #   -------- Get the (k choose 3)*(3 choose 2) models with 2 interaction.
      tmp3 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 2))),stringsAsFactors=FALSE)
      tmp3a <- tmp3b <- tmp3c <- tmp3

      tmp3a$X4 <- paste0(tmp3$X1,":",tmp3$X2)
      tmp3a$X5 <- paste0(tmp3$X1,":",tmp3$X3)

      tmp3b$X4 <- paste0(tmp3$X1,":",tmp3$X2)
      tmp3b$X5 <- paste0(tmp3$X2,":",tmp3$X3)

      tmp3c$X4 <- paste0(tmp3$X1,":",tmp3$X3)
      tmp3c$X5 <- paste0(tmp3$X2,":",tmp3$X3)

      temp.j3 <- unique(rbind(tmp3a,tmp3b,tmp3c))

      temp.j <- rbind(temp.j5,temp.j4,temp.j3)
      # 47994
      suppressWarnings(rm(temp.j5,tmp4,tmp4a,tmp4b,tmp4c,tmp4d,tmp4e,tmp4f,temp.j4,tmp3,tmp3a,tmp3b,tmp3c,temp.j3))

    } else if(j == 6){

      #   ---- SIX POSSIBLE MODEL TERMS (j <- 6)
      #   ------- Get the (k choose 6) models with 0 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= j){
        temp.j6 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j))),stringsAsFactors=FALSE)
      } else {
        temp.j6 <- NULL
      }

      #   ------- Get the (k choose 5)*(5 choose 2) models with 1 interaction.
      if(length(possibleCovars[!(grepl(":",possibleCovars))]) >= (j - 1)){
        tmp5 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 1))),stringsAsFactors=FALSE)
        tmp5a <- tmp5b <- tmp5c <- tmp5d <- tmp5e <- tmp5f <- tmp5g <- tmp5h <- tmp5i <- tmp5j <- tmp5
        tmp5a$X6 <- paste0(tmp5$X1,":",tmp5$X2)
        tmp5b$X6 <- paste0(tmp5$X1,":",tmp5$X3)
        tmp5c$X6 <- paste0(tmp5$X1,":",tmp5$X4)
        tmp5d$X6 <- paste0(tmp5$X1,":",tmp5$X5)
        tmp5e$X6 <- paste0(tmp5$X2,":",tmp5$X3)
        tmp5f$X6 <- paste0(tmp5$X2,":",tmp5$X4)
        tmp5g$X6 <- paste0(tmp5$X2,":",tmp5$X5)
        tmp5h$X6 <- paste0(tmp5$X3,":",tmp5$X4)
        tmp5i$X6 <- paste0(tmp5$X3,":",tmp5$X5)
        tmp5j$X6 <- paste0(tmp5$X4,":",tmp5$X5)
        temp.j5 <- unique(rbind(tmp5a,tmp5b,tmp5c,tmp5d,tmp5e,tmp5f,tmp5g,tmp5h,tmp5i,tmp5j))
      } else {
        temp.j5 <- NULL
      }

      #   -------- Get the (k choose 4)*[(4 choose 2) + (4 choose 3)*(2 choose 1)*(2)] models with 2 interaction.
      tmp4 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 2))),stringsAsFactors=FALSE)
      tmp4a <- tmp4b <- tmp4c <- tmp4d <- tmp4e <- tmp4f <- tmp4g <- tmp4h <- tmp4i <- tmp4j <- tmp4k <- tmp4l <- tmp4m <- tmp4n <- tmp4o <- tmp4

      tmp4a$X5 <- paste0(tmp4$X1,":",tmp4$X2)
      tmp4a$X6 <- paste0(tmp4$X3,":",tmp4$X4)

      tmp4b$X5 <- paste0(tmp4$X1,":",tmp4$X3)
      tmp4b$X6 <- paste0(tmp4$X2,":",tmp4$X4)

      tmp4c$X5 <- paste0(tmp4$X1,":",tmp4$X4)
      tmp4c$X6 <- paste0(tmp4$X2,":",tmp4$X3)

      #   ------- Now code interactions of the form 12:34.
      tmp4d$X5 <- paste0(tmp4$X1,":",tmp4$X2)
      tmp4d$X6 <- paste0(tmp4$X1,":",tmp4$X3)
      tmp4e$X5 <- paste0(tmp4$X1,":",tmp4$X2)
      tmp4e$X6 <- paste0(tmp4$X1,":",tmp4$X4)
      tmp4f$X5 <- paste0(tmp4$X1,":",tmp4$X3)
      tmp4f$X6 <- paste0(tmp4$X1,":",tmp4$X4)

      #   ------- Now code interactions of the form 23:24.  Note how X2 repeats.
      tmp4g$X5 <- paste0(tmp4$X2,":",tmp4$X3)
      tmp4g$X6 <- paste0(tmp4$X2,":",tmp4$X4)
      tmp4h$X5 <- paste0(tmp4$X2,":",tmp4$X1)
      tmp4h$X6 <- paste0(tmp4$X2,":",tmp4$X3)
      tmp4i$X5 <- paste0(tmp4$X2,":",tmp4$X1)
      tmp4i$X6 <- paste0(tmp4$X2,":",tmp4$X4)

      tmp4j$X5 <- paste0(tmp4$X3,":",tmp4$X1)
      tmp4j$X6 <- paste0(tmp4$X3,":",tmp4$X2)
      tmp4k$X5 <- paste0(tmp4$X3,":",tmp4$X2)
      tmp4k$X6 <- paste0(tmp4$X3,":",tmp4$X4)
      tmp4l$X5 <- paste0(tmp4$X3,":",tmp4$X1)
      tmp4l$X6 <- paste0(tmp4$X3,":",tmp4$X4)

      tmp4m$X5 <- paste0(tmp4$X4,":",tmp4$X1)
      tmp4m$X6 <- paste0(tmp4$X4,":",tmp4$X2)
      tmp4n$X5 <- paste0(tmp4$X4,":",tmp4$X1)
      tmp4n$X6 <- paste0(tmp4$X4,":",tmp4$X3)
      tmp4o$X5 <- paste0(tmp4$X4,":",tmp4$X2)
      tmp4o$X6 <- paste0(tmp4$X4,":",tmp4$X3)

      temp.j4 <- unique(rbind(tmp4a,tmp4b,tmp4c,tmp4d,tmp4e,tmp4f,tmp4g,tmp4h,tmp4i,tmp4j,tmp4k,tmp4l,tmp4m,tmp4n,tmp4o))

      #   -------- Get the (k choose 3)*(3 choose 2) models with 3 interaction.
      tmp3 <- data.frame(t(as.matrix(utils::combn(possibleCovars[!(grepl(":",possibleCovars))],j - 3))),stringsAsFactors=FALSE)
      tmp3a <- tmp3

      tmp3a$X4 <- paste0(tmp3$X1,":",tmp3$X2)
      tmp3a$X5 <- paste0(tmp3$X1,":",tmp3$X3)
      tmp3a$X6 <- paste0(tmp3$X2,":",tmp3$X3)

      temp.j3 <- tmp3a

      temp.j <- rbind(temp.j6,temp.j5,temp.j4,temp.j3)
      # 267615
      suppressWarnings(rm(temp.j6,tmp5a,tmp5b,tmp5c,tmp5d,tmp5e,tmp5f,tmp5g,tmp5h,tmp5i,tmp5j,tmp5,temp.j5,tmp4a,tmp4b,tmp4c,tmp4d,tmp4e,tmp4f,tmp4g,tmp4h,tmp4i,tmp4j,tmp4k,tmp4l,tmp4m,tmp4n,tmp4o,tmp4,temp.j4,tmp3a,tmp3,temp.j3))

    }

    #   ---- Now build the data.frame.
    this.j <- data.frame(nVar=j,model=as.character(apply(temp.j,1,function(x) paste0(x,collapse=" + "))),stringsAsFactors=FALSE)
    allJ <- rbind(allJ,this.j)
  }

  # #   ---- If offset is required, add it in.
  # if(!is.null(offset)){
  #   allJ$modelOffsetPretty <- paste0(allJ$modelPretty," + offset(log(",paste0(offset,collapse="*"),"))")
  # }

  #   ---- Remove model groups we don't want to double up -- GLG_SYM vs GLG_SYM2.
  # allJ[allJ$nVar == 2,]
  if("GLG_SYM" %in% allJ[allJ$nVar == 1,]$model & "GLG_SYM2" %in% allJ[allJ$nVar == 1,]$model){
    allJ$bad1 <- as.numeric(grepl("\\bGLG_SYM\\b",allJ$model))
    allJ$bad2 <- as.numeric(grepl("GLG_SYM2",allJ$model))
    allJ$bad3 <- allJ$bad1*allJ$bad2
    allJ <- allJ[allJ$bad3 == 0,]
    allJ$bad1 <- allJ$bad2 <- allJ$bad3 <- NULL
  }

  #   ---- Need to often clean up covariate names.
  modelFormat <- cleanUpFormula(allJ$model)
  baseFormula <- cleanUpFormula(baseFormula)

  #   ---- Build model strings allowing / not allowing intercept.
  if(baseFormula != ""){
    modelIntY <- paste0(outcomeFormula," ~ 1 + ",modelFormat," + ",baseFormula)
    modelIntN <- paste0(outcomeFormula," ~ -1 + ",modelFormat," + ",baseFormula)
    # modelIntY2 <- paste0(outcomeFormula," ~ 1 + (",modelFormat,")^2 + ",baseFormula)
    # modelIntN2 <- paste0(outcomeFormula," ~ -1 + (",modelFormat,")^2 + ",baseFormula)
  } else {
    modelIntY <- paste0(outcomeFormula," ~ 1 + ",modelFormat)
    modelIntN <- paste0(outcomeFormula," ~ -1 + ",modelFormat)
  }

  #   ---- Only prepare this for a formula if necessary.
  if(ziFormula != ""){
    modelZI <- rep(paste0(" ~ ",ziFormula),length(modelFormat))
  } else {
    modelZI <- rep(ziFormula,length(modelFormat))
  }

  return(list(modelIntY=modelIntY,modelIntN=modelIntN,modelZI=modelZI,modelRaw=modelFormat))
}
