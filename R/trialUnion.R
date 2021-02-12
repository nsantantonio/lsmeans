#' trialUnion function
#'
#' function to (do something)
#'
#' @param trialList [value]
#' @param resetRowNames [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
trialUnion <- function(trialList, resetRowNames = TRUE){
	allTraits <- Reduce(union, lapply(trialList, names))
	for (i in names(trialList)){
		missTrt <- allTraits[!allTraits %in% names(trialList[[i]])]
		for(j in missTrt){
			trialList[[i]][[j]] <- NA
		}
		trialList[[i]] <- trialList[[i]][allTraits]
	}
	trialDf <- do.call(rbind, trialList)
	if(resetRowNames) rownames(trialDf) <- NULL
	trialDf
}
