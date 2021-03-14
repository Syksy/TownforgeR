# Definitions for the TownforgeR web interface 
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Launch TownforgeR shiny app
#'
#' Description
#'
#' @export
shinyTF <- function(){
	app <- shiny::shinyApp(ui = TownforgeR::uiTF, server = TownforgeR::serverTF)
	shiny::runApp(app)
}

#' Shiny UI for browser
#'
#' Description
#'
#' @export
uiTF <- shiny::fluidPage(
	shiny::h2("TownforgeR R-shiny app"),
	shiny::sidebarLayout(
	#textInput("method", "Selected TF RPC method name"),
		shiny::selectInput("command", label="Select a TF RPC command", 
			choices = c(
				"get_block_count", # Get current blockchain height
				#"mining_status", # Get daemon mining status
				"cc_get_account",
				"cc_get_accounts",
				"cc_get_cities",
				"cc_get_flag",
				"cc_get_flags",
				"cc_get_shares",
				"cc_get_last_update_events",
				"cc_get_discoveries",
				"cc_get_temperature",
				"cc_find_flag"
			)
		),
		shiny::uiOutput("pars")
	),
	shiny::mainPanel(
		shiny::verbatimTextOutput("verb")
	)
)

#' Shiny server side
#'
#' Description
#'
#' @export
serverTF <- function(input, output){
	output$pars <- shiny::renderUI({
		if(input$command %in% c("cc_get_account")){
			shiny::textInput("id", "id", value = 0)
		}else if(input$command %in% c("cc_get_discoveries")){
			shiny::textInput("account", "account", value = 0)	
		}else if(input$command %in% c("cc_get_shares","cc_get_temperature")){
			shiny::textInput("city", "city", value = 0)	
		}else if(input$command %in% c("cc_find_flag")){
			shiny::textInput("city", "city", value = 0); shiny::textInput("x", "x", value = 0); shiny::textInput("y", "y", value = 0)			
		}
	})
	output$verb <- shiny::renderPrint({ 
		TownforgeR::tf_rpc_curl(
			method = input$command, 
			params = TownforgeR:::pruneList(
				list(
					id = as.numeric(input$id), 
					account = as.numeric(input$account), 
					city = as.numeric(input$city), 
					x = as.numeric(input$x), 
					y = as.numeric(input$y)
				)
			)
		)
	})
}


#' Prune a list with NULL elements, removing them along with their names
#'
#' Description
pruneList <- function(l){
	l[!lengths(l)==0]
}