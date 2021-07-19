# TownforgeR
An R-package for interacting with the Townforge blockchain

Read more about Townforge at: www.townforge.net

Townforge is a blockchain based game forked originally from the Monero project (https://github.com/monero-project).
This is a package written for the R statistical software language, for curl'ing the Townforge RPC commands and interacting with the daemon. 
TownforgeR parses the input JSON according to the web app, and the returned JSON into R-compatible lists and data frames.

It comes with an R Shiny interface, i.e. a graphical web browser, in addition to the base R functionality.

## Running TownforgeR during Townforge testnet (v0.32):

Launching daemon (in testnet):

```
townforged --testnet
```

Launching wallet rpc (in testnet):

```
townforge-wallet-rpc --rpc-bind-port 63079 --wallet-file "myWalletFile" --password "myWalletPassword" --testnet --daemon-port 28881 --disable-rpc-login
```

Installing TownforgeR from GitHub and testing a basic daemon RPC call:

```
# devtools::install_github("Syksy/TownforgeR")
library(TownforgeR)
# Curl the RPC with the method "get_block_count", assuming URL to be 127.0.0.1:18881 
# Note that testnet daemon port by default is 28881
TownforgeR::tf_rpc_curl(method="get_block_count") 
```


Launching (testnet) R Shiny interface:

```
TownforgeR::shinyTF("http://127.0.0.1:28881/json_rpc")
```
