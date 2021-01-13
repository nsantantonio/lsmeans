#' fitlsmeans function
#'
#' function to fit a linear mixed model and calculate BLUPs and heritability.
#'
#' @param form object of class formula, compliant with lmer().
#' @param data data.frame.  Name of effect for which lsmeans should be calculated
#' @param addMu logical. Should the mean be added back into the BLUP? Default is FALSE
#' @return list. Returns list of BLUP, mean, and h2, i.e. heritability
#' @details extracts BlUPs and calculates heritability 
#' This function is a wrapper for getBlupH2, and was made to calculate iid BLUPs and heritability 
#' @examples
#' fitBlupH2(mpg ~ 1|cyl, effect = "cyl", data = mtcars)
#' fitBlupH2(Sepal.Length ~ 1|Species, "Species", data = iris)
#' @export
fitBlupH2 <- function(form, effect, addMu = FALSE, ...){
	require(lme4)
	usrContr <- options("contrasts") # save user contrast options
	options(contrasts = c('contr.sum', 'contr.sum'))
	fit <- lmer(form, ...)
	lsm <- getBlupH2(fit, effect = effect, addMu = addMu)
	options(contrasts = usrContr$contrasts)
	return(lsm)
}
