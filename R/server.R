# Server definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

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
  
  sessionVars <- shiny::reactiveValues(wallet_rpc_port = "")
  
  shiny::observeEvent(input$port_submit_button, {
    
    sessionVars$wallet_rpc_port <- input$port_wallet_rpc
    
    wallet_balance <- TownforgeR::tf_rpc_curl(url = paste0("http://127.0.0.1:", sessionVars$wallet_rpc_port, "/json_rpc"),
      method ="get_balance")
    
    #print(wallet_balance)
    
    output$wallet_balance_text <-
      shiny::renderText(paste0( "Total balance: ", prettyNum(wallet_balance$result$balance / 1e+06, big.mark = ","),
        "<br>In-game account balance: ",  prettyNum(wallet_balance$result$cc_balance / 1e+06, big.mark = ",") ) )
    
    output$wallet_init_disappears <- shiny::reactive(TRUE)
    
  })
  
  shiny::observeEvent(input$wallet_pw_submit_button, {
    
    sessionVars$wallet_rpc_port <- "63079"
    
    system_command <- paste0("\"", input$wallet_rpc_path, "\" --wallet-file \"", input$wallet_path,
      "\" --testnet --rpc-bind-port 63079 --daemon-port ", input$port_townforged,
      " --password \"", input$wallet_pw, "\" --disable-rpc-login" )
    
    system(system_command, wait = FALSE)
    
    withProgress(message = "Starting townforge-wallet-rpc...", Sys.sleep(10))
    # Wait for townforge-wallet-rpc to boot up
    
    wallet_balance <- TownforgeR::tf_rpc_curl(url = paste0("http://127.0.0.1:", sessionVars$wallet_rpc_port, "/json_rpc"),
      method ="get_balance")
    
    output$wallet_balance_text <-
      shiny::renderText(paste0( "Total balance: ", prettyNum(wallet_balance$result$balance / 1e+06, big.mark = ","),
        "<br>In-game account balance: ",  prettyNum(wallet_balance$result$cc_balance / 1e+06, big.mark = ",") ) )
    
    output$wallet_init_disappears <- shiny::reactive(TRUE)
    
    
  })
  
  
  output$wallet_init_disappears <- shiny::reactive(FALSE)
  
  shiny::outputOptions(output, "wallet_init_disappears", suspendWhenHidden = FALSE)
  # See https://shinydata.wordpress.com/2015/02/02/a-few-things-i-learned-about-shiny-and-reactive-programming/
  
  shiny::observeEvent(input$deposit_submit_button, {
    
    deposit_result <- TownforgeR::tf_rpc_curl(url = paste0("http://127.0.0.1:", sessionVars$wallet_rpc_port, "/json_rpc"),
      method = "cc_deposit", params = list(amount = formatC(input$deposit_amount * 1e+06, format = "fg")))
    # TODO: cc_deposit won't accept scientific notation, it seems, so "large" deposit amounts fail if not
    # formatted with formatC(). Need a general fix to this. maybe in tf_rpc_curl()
    
    output$deposit_tx_hash <- shiny::renderText(paste0("Transaction hash: ", deposit_result$result$tx_hash_list) )
    
  })
  
  shiny::observeEvent(input$depth_chart_button, {
    
    output$depth_chart <- shiny::renderPlot({
      
      order.book <- TownforgeR::tf_parse_markets()
      order.book <- order.book[order.book$id == 1, ]
      order.book <- order.book[order(order.book$price), ]
      bids.book <- order.book[order.book$bid, ]
      offers.book <- order.book[ ! order.book$bid, ]
      
      bids.book.steps <- sum(bids.book$amount) - c(0, cumsum(bids.book$amount))
      
      offers.book.steps <- c(0, cumsum(offers.book$amount))
      
      plot(0, 0,  type = "n", yaxs = "i",
        sub = paste0("Price spread: ", round(max(offers.book$price) - max(bids.book$price))),
        xlab = "Price", ylab = "Depth",
        main = "Sandstone order book",
        xlim = range(order.book$price),
        ylim = c(0, 1.1 * max(bids.book.steps, offers.book.steps)))
      
      plot(stepfun(bids.book$price, bids.book.steps, right = TRUE), col = "green", do.points = FALSE, add = TRUE)
      
      plot(stepfun(offers.book$price, offers.book.steps, right = TRUE), col = "red", do.points = FALSE,  add = TRUE)
      
    })
    
  })
  
  shiny::observeEvent(input$map_button, {
    
    output$map_chart <- shiny::renderPlot({
      
      flags.ret <- tf_rpc_curl(method = "cc_get_flags")$result$flags
      max.flag.id <- flags.ret[[length(flags.ret)]]$id
      
      coords.met <- matrix(NA_real_, nrow = max.flag.id, ncol = 4, dimnames = list(NULL, c("x0", "x1", "y0", "y1")) )
      owner <- vector(mode = "numeric", length = max.flag.id)
      
      for (i in 1:max.flag.id) {
        ret <- tf_rpc_curl(method = "cc_get_flag", params = list(id = i))
        if (any(names(ret) == "error")) { next }
        coords.met[i, "x0"] <- ret$result$x0
        coords.met[i, "x1"] <- ret$result$x1
        coords.met[i, "y0"] <- ret$result$y0
        coords.met[i, "y1"] <- ret$result$y1
        owner[i] <- ret$result$owner
      }
      
      owner <- owner[complete.cases(coords.met)]
      coords.met <- coords.met[complete.cases(coords.met), ]
      
      plot(0, 0, xlim = range(coords.met[, c("x0", "x1")]), 
        ylim = range(coords.met[, c("y0", "y1")]),
        main = "Flag map, by owner ID")
      rect(coords.met[, "x0"], coords.met[, "y0"], coords.met[, "x1"], coords.met[, "y1"], col = owner)
      legend("topleft", legend = unique(owner), fill = unique(owner), horiz = TRUE)
      
    })
  })
  
}
