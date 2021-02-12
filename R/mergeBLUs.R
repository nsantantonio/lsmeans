#' mergeBLUs function
#'
#' function to (do something)
#'
#' @param BLUlist [value]
#' @param sortHiLo [value]. Default is NULL
#' @param addInfo [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
mergeBLUs <- function(BLUlist, sortHiLo = NULL, addInfo){
	if(!is.null(addInfo)){
		addToBLU <- list() 
		for(i in names(addInfo)){
			xtra <- 2:length(BLUlist[[1]])
			addToBLU[[i]] <- addInfo[i]
			addToBLU[[i]][names(BLUlist[[1]][xtra])] <- NA
		}
		BLUlist <- c(addToBLU, BLUlist)
	}
	x1 <- BLUlist[[1]][[1]]
	bluDF <- data.frame(randEff = names(x1))
	bluDF[[names(BLUlist)[1]]] <- x1

	for(i in 2:length(BLUlist)){
		x2 <- BLUlist[[i]][[1]]
		bluDFi <- data.frame(randEff = names(x2))
		bluDFi[[names(BLUlist)[i]]] <- x2
		bluDF <- merge(bluDF, bluDFi, by = "randEff", all = TRUE)
	}
	if(!is.null(sortHiLo)){
		bluDF <- bluDF[order(-bluDF[[sortHiLo]]),]	
	}
	statL <- list()
	for(j in names(BLUlist[[1]])[2:length(BLUlist[[1]])]){
		statL[[j]] <- c(NA, sapply(BLUlist, "[[", j))
	}
	rL <- c(list(bluDF), statL)
	names(rL) <- names(BLUlist[[1]])
	return(rL)
}
