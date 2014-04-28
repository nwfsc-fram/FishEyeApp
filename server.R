#server

source("external/serverHead.R", local=T)
shinyServer(
  function(input, output, session) {
    
    source("external/explorer/explorer.r", local=T)
    
  }
)