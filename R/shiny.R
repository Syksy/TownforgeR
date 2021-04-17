# Definitions for the TownforgeR web interface 
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Shiny UI for browser
#'
#' Description
uiTF <- shiny::navbarPage(paste("TownforgeR", gsub("`|´", "", packageVersion("TownforgeR"))),
	# Raw command panel
	shiny::tabPanel("Raw commands",
		shiny::sidebarLayout(
		#textInput("method", "Selected TF RPC method name"),
			shiny::sidebarPanel(
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
	),
	# Accounts panel
	shiny::tabPanel("Accounts",
		DT::dataTableOutput("accountsDT")
	),
	# Markets panel
	shiny::tabPanel("Markets",
		DT::dataTableOutput("marketsDT")
	),
	# NFTs panel
	shiny::tabPanel("NFTs",
		DT::dataTableOutput("nftsDT")
	),
	# Network panel
	shiny::tabPanel("Network",
		shiny::verbatimTextOutput("network")
	),
	# Inspect panel
	shiny::tabPanel("Inspect",
		shiny::tabsetPanel(type = "tabs",
			shiny::tabPanel("NFTs",
				shiny::sidebarLayout(
					shiny::sidebarPanel(
						shiny::selectInput("item_inspect",
							label = "Select item",
							choices = c("")
							#choices = TownforgeR:::formatNFTs()
						)
					),
					shiny::mainPanel(
						shiny::htmlOutput("inspect_item")
					)
				)
			),
			shiny::tabPanel("Blocks",
				"foo"
			)
		)
	)
)

#' Shiny server side
#'
#' Description
serverTF <- function(input, output, session){
	# Need to load select options on the run
	updateSelectInput(session, "item_inspect",
		choices = TownforgeR:::formatNFTs()
	)

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
	output$accountsDT <- DT::renderDataTable({
		TownforgeR::tf_parse_accounts()	
	})
	output$marketsDT <- DT::renderDataTable({
		TownforgeR::tf_parse_markets()	
	})
	output$nftsDT <- DT::renderDataTable({
		TownforgeR::tf_parse_nfts()	
	})
	output$network <- shiny::renderPrint({ 
		TownforgeR::tf_parse_network()
	})
	output$inspect_item <- shiny::renderText({
		items <- TownforgeR::tf_parse_nfts()
		items <- items[which(paste(items$id, ":", items$name) == input$item_inspect),]
		ret <- ifelse(items$ipfs_multihash=="",
			# Not an IPFS NFT, return ordinary HTML formatting
			#input$item_inspect,
			paste(paste(names(items), items, sep=": "), collapse="<br>"),
			# Try render an NFT representation in addition to just item information
			paste(paste(paste(names(items), items, sep=": "), collapse="<br>"),TownforgeR::tf_shiny_nft_png(items$ipfs_multihash), sep="<br><br>")
		)
		ret
	})
}

#' Launch TownforgeR shiny app
#'
#' Description
#'
#' @export
shinyTF <- function(){
	app <- shiny::shinyApp(ui = TownforgeR:::uiTF, server = TownforgeR:::serverTF)
	#app <- shiny::shinyApp(ui = uiTF, server = serverTF)
	shiny::runApp(app)
}

