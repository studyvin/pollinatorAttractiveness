
#' @importFrom dplyr arrange contains desc filter group_by left_join mutate pull rename select select_if summarize
#' @importFrom ggplot2 aes annotate element_text geom_bar geom_errorbar geom_hline ggplot guides labs position_dodge theme
#' @importFrom graphics plot
#' @importFrom grDevices dev.off png
#' @importFrom lme4 glmer
#' @importFrom magrittr %>%
#' @importFrom MASS glm.nb
#' @importFrom multcomp cld glht
#' @importFrom purrr reduce
#' @importFrom rlang .data
#' @importFrom stats anova as.formula coef confint deviance df.residual extractAIC glm logLik model.matrix poisson quasipoisson reorder residuals
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom utils combn head read.csv
"_PACKAGE"

#   ---- Quiet concerns of R CMD check re: the .'s that appear in pipelines.
#   ---- Ref: Jenny Bryan's GitHub googlesheet package.
#   ---- Reference the "evaluator" package on GitHub for guidance/example.
utils::globalVariables(c("."))

#' @export
magrittr::`%>%`






