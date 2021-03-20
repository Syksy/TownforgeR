###
#
# Parsers and other useful functions for extracting, manipulating, and structuring data from RPC
#
###

#' tf_parse_accounts
#'
#' Parse existing user accounts and some key info into an R data.frame
#'
#' @param ids TODO
#' @param get_accounts TODO
#' @param get_account TODO
#' @param fields_plain TODO
#' @param fields_nests TODO
#' @param collapse_fun TODO
#' @param truncate_char TODO
#' @param ... TODO
#'
#' @return A data.frame corresponding to queried plain and nested fields over desired ids
#'
#' @examples
#' tf_parse_accounts()
#'
#' @export
tf_parse_accounts <- function(
	# Custom vector of account ids to query data for
	ids, 
	# RPC call for querying a list of accounts
	get_accounts = 'cc_get_accounts', 
	# RPC call for querying a single account and its relevant data
	get_account  = 'cc_get_account',  
	# Vector of account parameters that contain only a single value to retrieve
	fields_plain = c('name','public_key','moose_killed','bears_killed','prestige','description','balance'),	
	# Vector of account parameters that contain nested lists and require further parsing
	fields_nests = c('flags','item_balances','badges'),	
	# A function that parses a nested list into a single character string
	collapse_fun = TownforgeR:::parseNest,
	# If collapsed_fun produces a character count above truncation limit, cut it and add '...' at the end
	truncate_char = 40,
	# Additional parameters
	...
){
	# No custom account id list, extracting all data
	if(missing(ids)){
		# First extract existing user account ids using 'cc_get_accounts'		
		ids <- unlist(lapply(TownforgeR::tf_rpc_curl(method=get_accounts)$result$accounts, FUN=function(x) x$id))
	# Invalid class for custom ids
	}else if(!class(ids) %in% c("character", "numeric")){
		stop(paste("Invalid class in 'ids':", class(ids)))
	# Cast ids to numeric just in case
	}else{
		ids <- as.numeric(ids)
	}
	# l(ist)apply through the obtained metadata and row-bind them into a data.frame
	do.call("rbind", 
		# Loop over individual account ids and extract a row of data for each based on desired fields
		lapply(ids, FUN=function(id) {
			tmp <- TownforgeR::tf_rpc_curl(method=get_account, params=list("id"=id))$result
			tmp_plain <- as.data.frame(tmp[fields_plain])
			tmp_nests <- do.call("cbind", lapply(tmp[fields_nests], FUN=collapse_fun))
			names(tmp_nests) <- fields_nests
			tmp_nests <- as.data.frame(t(apply(tmp_nests, MARGIN=2, FUN=function(str) { ifelse(nchar(str) > truncate_char, paste0(substring(str, 1, truncate_char-3), "..."), str) })))
			colnames(tmp_nests) <- fields_nests
			cbind(id = id, tmp_plain, tmp_nests)
		})
	)
}
