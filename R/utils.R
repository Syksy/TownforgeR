###
#
# Misc utils
#
###


#' Prune a list with NULL elements, removing them along with their names
#'
#' Description
pruneList <- function(l){
	l[!lengths(l)==0]
}

#' Collapse a nested character list into a character string 'varname=123;othervar=foo', 'varname=0;othervar=bar', ...
#' 
#' Description
parseNest <- function(x, symbol1 = '=', symbol2 = ',', symbol3 = ';') { 
	paste(
		unlist(
			lapply(x, FUN=function(z){ 
				paste(
					paste(
						ifelse(length(names(z))>0, names(z), "value")
					, z, sep=symbol1)
				, collapse=symbol2)
			})
		)
	, collapse=symbol3) 
}
