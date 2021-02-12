#' capitalizeFirst function
#'
#' function to (do something)
#'
#' @param name [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
capitalizeFirst <- function(name){
	name <- tolower(name)
	paste0(toupper(substring(name, 1, 1)), substring(name, 2))
}
