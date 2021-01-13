#' getBlupH2 function
#'
#' function to (do something)
#'
#' @param lmerFit [value]
#' @param effect [value]
#' @param addMu [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
getBlupH2 <- function(lmerFit, effect, addMu = FALSE){
	vc <- as.data.frame(VarCorr(lmerFit))
	rownames(vc) <- vc$grp
	GE <- vc[c(effect, "Residual"), "vcov"]
	h2 <- GE[1] / sum(GE)
	effTab <- ranef(lmerFit)[[effect]]
	eff <- effTab[,1]
	names(eff) <- rownames(effTab)
	mu <- fixef(lmerFit)[["(Intercept)"]]
	if(addMu) eff <- eff + mu
	rL <- list(BLUP = eff, mean = mu, h2 = h2)
	return(rL)
}
