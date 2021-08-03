#' getBlupH2 function
#'
#' function to extract BLUPs and calculate heritability.
#'
#' @param lmerFit lmer model object.
#' @param effect factor to extract BLUPs and calculate heritability
#' @param addMu logical. Should the mean be added back into the BLUP? Default is FALSE
#' @return list. Returns list of BLUP, mean, and h2, i.e. heritability
#' @details extracts BlUPs and calculates heritability 
#' @examples none
#' @export
getBlupH2 <- function(lmerFit, effect, addMu = FALSE){
	vc <- as.data.frame(VarCorr(lmerFit))
	rownames(vc) <- vc$grp
	# GE <- vc[c(effect, "Residual"), "vcov"]
	# h2 <- GE[1] / sum(GE)
	Vg <- vc[c(effect), "vcov"]
	Verr <- vc[c("Residual"), "vcov"]
	h2 <- Vg / (Vg + Verr)
	effTab <- ranef(lmerFit)[[effect]]
	eff <- effTab[,1]
	names(eff) <- rownames(effTab)
	mu <- fixef(lmerFit)[["(Intercept)"]]
	if(addMu) eff <- eff + mu
	rL <- list(BLUP = eff, mean = mu, h2 = h2, sigma = sqrt(Verr))
	return(rL)
}
