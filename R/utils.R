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
parseNest <- function(x){ 
	paste(
		unlist(
			lapply(x, FUN=function(z){ 
				# If values are named, send these separated value names inside the string
				if(length(names(z))>0) nam <- names(z)
				# Unnamed values are just called 'value'
				else nam <- "value"
				# Paste names and corresponding actual content
				paste(
					paste(nam, z, sep="="), 
				collapse=",")
			})
		), 
	collapse=";")
}

#' Format NFTs in a nice character string
#'
#' Description
formatNFTs <- function()
{
	items <- TownforgeR::tf_parse_nfts()
	paste(items$id, ":", items$name)
}
