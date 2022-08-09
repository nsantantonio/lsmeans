#' lsmeans function
#'
#' function to calculate lsmeans, CV and LSD from a linear model fit.
#'
#' @param lmfit object of class lm. 
#' @param effect character. Name of effect for which lsmeans should be calculated
#' @param meanType mean to use for calculating CV. Options include 'raw' = mean of y, 'effect' = mean of estimated effects, and 'intercept', the estimated intercept of th emodel fit. Default is "intercept". To reproduce CVs calculated by proc glm in SAS, use "intercept", or set contrasts to use sum contrsaints with 'options(contrasts = c("contr.sum", "contr.sum")'
#' @param alpha numeric. Significance threshold. Default is 0.05
#' @return list. Returns a list of BLUEs, mean used for CV, CV, and LSD. 
#' @details 
#' This function was made to calculate effects, CV and LSD as proc glm in SAS does. CVs are not reliable statistics for determining field trial quality.  
#' @examples
#' # contrasts need to be set to sum contraints to obtain same stats produced by proc glm in SAS 
#' usrContr <- options("contrasts") # save user contrast options
#' options(contrasts = c('contr.sum', 'contr.sum'))
#' fit <- lm(mpg ~ factor(cyl), data = mtcars)
#' lsmeans(fit, effect = "factor(cyl)")
#' options(contrasts = usrContr)
#' @export
lsmeans <- function(lmfit, effect, meanType = "intercept", alpha = 0.05, sortHiLo = FALSE, sortLoHi = FALSE){
	if(alpha >= 1 | alpha <= 0) stop("alpha must be between 0 and 1, i.e. 0 < alpha < 1")
	if(!meanType %in% c("raw", "effect", "weighted", "intercept")) stop("meanType agrgument must one of the following:\n 'raw', 'effect', 'intercept'")

	if(grepl("\\(|\\)", effect)){
		effectGrep <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", effect))
	} else {
		effectGrep <- effect
	}
	N <- length(lmfit$residuals)
	p <- length(lmfit$coefficients)
	residDF <- N - p

	if (all(options("contrasts")$contrasts == "contr.sum")){
		effL1 <- lmfit$coefficients[grep(effectGrep, names(lmfit$coefficients))]
		eff <- c(effL1, 0 - sum(effL1)) + lmfit$coefficients["(Intercept)"] 
		names(eff) <- lmfit$xlevels[[effect]]
	} else if (options("contrasts")$contrasts[1] == "contr.treatment"){
		eff <- c(lmfit$coefficients["(Intercept)"], lmfit$coefficients[grep(effectGrep, names(lmfit$coefficients))] + lmfit$coefficients["(Intercept)"])
		names(eff) <- lmfit$xlevels[[effect]]
		if(meanType == "intercept") warning("meanType is 'intercept', but contrasts set to treatment. The first factor level will be used for the mean (i.e. probably not what you want!).  To set contrasts to sum constraints, use: \n options(contrasts = c('contr.sum', 'contr.sum'))")
	} else {
		stop("Was expecting contrasts to be either 'contr.sum' or 'contr.treatment'. For example, set contrast options with: \n options(contrasts = c('contr.sum', 'contr.sum'))")
	}

	sumlmfit <- summary(lmfit)

	if(meanType == "raw"){	
		y <- lmfit$fitted + lmfit$residuals
		mu <- mean(y)
	} else if(meanType == "effect") {
		mu <- mean(eff)
	} else {
		mu <- lmfit$coefficients["(Intercept)"]
	}

	sigma <- sumlmfit$sigma
	n <- length(lmfit$residuals) / length(lmfit$xlevels[[effect]])

	tStat <- qt(1 - alpha / 2, residDF)
	lsd <- tStat * sigma * sqrt(2/n)

	cv <- sigma / mu * 100
	
	names(cv) <- NULL
	names(mu) <- NULL

	if(sortHiLo) {
		eff <- sort(eff, decreasing = TRUE)
	} else if (sortLoHi) {
		eff <- sort(eff)
	} 

	rL <- list(BLUE = eff, mean = mu, CV = cv, LSD = lsd)
	return(rL)
}
