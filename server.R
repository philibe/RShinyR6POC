options(encoding = "UTF-8")

options(error = NULL)
if (interactive()) {
  # Dans RStudio
  options(  error = function() {.rs.recordTraceback(TRUE,minDepth=1,.rs.enqueueError)})
}else {
  # Dans Shiny-Server
  options(  error = function() {traceback() })
}

library(shiny)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(tidyr)
library(DT)
library(DescTools)
library(R6)
library(ggplot2)
library(ggforce)
library(cowplot)
library(stringr)




source("Others_Functions.R")

source ("modules/MiniRapportTabDynUI_moduleR6.R", local = TRUE)
source ("modules/FicheGraphUI_moduleR6.R", local = TRUE)     
source ("modules/FicheTabGraphUI_moduleR6.R", local = TRUE)



shinyServer(function(input, output, session) {
  source ("ui_server_rshiny/server_rshiny/RShinyR6PoCUI_server_rshiny.R", local = TRUE) 
})
