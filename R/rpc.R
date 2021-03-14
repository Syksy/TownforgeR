#' TownforgeR function for curl'ing RPC calls
#'
#' Description
#'
#' @export
tf_rpc_curl <- function(
	# URL for TF RPC connection
	url = "http://127.0.0.1:18881/json_rpc", 
	# Premade JSON template; method and params
	method = "get_block_count",
	params = list(),
	# Additional parameters
	...
){
	try({RJSONIO::fromJSON(
		RCurl::postForm(url,
			.opts = list(
				postfields = RJSONIO::toJSON(
					list(
						jsonrpc = "2.0", 
						id = "0", 
						method = method,
						params = params
					)
				),
				httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
			)
		)
	)})
}
