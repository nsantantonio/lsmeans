#' lsmeans function
#'
#' function to (do something)
#'
#' @param lmfit [value]
#' @param effect [value]
#' @param meanType [value]. Default is "intercept"
#' @param alpha [value]. Default is 0.05
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
lsmeans <- function(lmfit, effect, meanType = "intercept", alpha = 0.05){
	if(!meanType %in% c("raw", "effect", "weighted", "intercept")) stop("meanType agrgument must one of the following:\n 'raw', 'effect', 'intercept'")

	N <- length(lmfit$residuals)
	p <- length(lmfit$coefficients)
	residDF <- N - p

	if (all(options("contrasts")$contrasts == "contr.sum")){
		effL1 <- lmfit$coefficients[grep(effect, names(lmfit$coefficients))]
		eff <- c(effL1, 0 - sum(effL1)) + lmfit$coefficients["(Intercept)"]
		names(eff) <- lmfit$xlevels[[effect]]
	} else if (all(options("contrasts")$contrasts == "contr.treatment")){
		eff <- c(lmfit$coefficients["(Intercept)"], lmfit$coefficients[grep(effect, names(lmfit$coefficients))] + lmfit$coefficients["(Intercept)"])
		names(eff) <- lmfit$xlevels[[effect]]

	} else {
		stop("Was expecting contrasts to be either 'contr.sum' or 'contr.treatment'. For example, set contrast options with: \n options(contrasts = c('contr.sum', 'contr.sum'))")
	}

	sumlmfit <- summary(lmfit)

	if(meanType == "raw"){	
		y <- lmfit$fitted + lmfit$residuals
		mu <- mean(y)
	} else if(meanType == "effect") {
		mu <- mean(eff)
	} else if(meanType == "weighted"){
		XtX <- solve(sumlmfit$cov.unscaled)
		nLev <- diag(XtX)
	 	nLevEff <- nLev[grep(effect, names(nLev))]
	 	eff1 <- NULL
	 	eff1[[paste0(effect, lmfit$xlevels[[effect]][1])]] <- nLev["(Intercept)"] - sum(nLevEff)
	 	nLevEffFull <- c(eff1, nLevEff)
	 	mu <- sum(eff * nLevEffFull) / nLev["(Intercept)"]
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

	rL <- list(BLUE = eff, mean = mu, CV = cv, LSD = lsd)
	return(rL)
}
