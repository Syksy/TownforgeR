# Launcher definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

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

