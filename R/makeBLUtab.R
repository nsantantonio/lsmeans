#' makeBLUtab function
#'
#' function to (do something)
#'
#' @param BLU [value]
#' @param sortby [value]. Default is NULL
#' @param addInfo [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
makeBLUtab <- function(BLU, sortHiLo = NULL, sortLoHi = NULL, addInfo = NULL){
	bluTab <- mergeBLUs(BLU, sortHiLo = sortHiLo, sortLoHi = sortLoHi, addInfo = addInfo)
	bluTabwStats <- do.call(rbind, bluTab)
	bluTabwStats[is.na(bluTabwStats$effect), "effect"] <- rownames(bluTabwStats)[is.na(bluTabwStats$effect)]
	return(bluTabwStats)
}
