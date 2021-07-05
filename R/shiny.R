# Launcher definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Launch TownforgeR shiny app
#'
#' Description
#'
#' @export
shinyTF <- function(url = "http://127.0.0.1:18881/json_rpc"){
  shiny::shinyOptions(url = url)
  shiny::shinyOptions(usecairo = TRUE)
  # Passes url argument down to the server function
  # Thanks to https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  
  thematic::thematic_shiny(font = "auto")
  
	app <- shiny::shinyApp(ui = TownforgeR:::uiTF, server = TownforgeR:::serverTF)
	#app <- shiny::shinyApp(ui = uiTF, server = serverTF)
	shiny::runApp(app)
}

