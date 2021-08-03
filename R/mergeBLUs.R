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
	flev <- unique(lapply(BLUlist, function(x) {names(x[[1]])}))
	if(length(flev) > 1){
		stop("effect factor levels differ! fix me...")
	} else {
		flev <- flev[[1]]
	}

	if(!is.null(addInfo)){
		addInfo <- lapply(addInfo, "[", flev)
		addToBLU <- list() 
		for(i in names(addInfo)){
			xtra <- 2:length(BLUlist[[1]])
			addToBLU[[i]] <- addInfo[i]
			addToBLU[[i]][names(BLUlist[[1]][xtra])] <- NA
		}
		BLUlist <- c(addToBLU, BLUlist)
	}
	x1 <- BLUlist[[1]][[1]]
	bluDF <- data.frame(effect = names(x1))
	bluDF[[names(BLUlist)[1]]] <- x1

	for(i in 2:length(BLUlist)){
		x2 <- BLUlist[[i]][[1]]
		bluDFi <- data.frame(effect = names(x2))
		bluDFi[[names(BLUlist)[i]]] <- x2
		bluDF <- merge(bluDF, bluDFi, by = "effect", all = TRUE)
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
