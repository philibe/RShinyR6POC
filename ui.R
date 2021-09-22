options(encoding = "UTF-8")

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


largeur_page_pct<-96

source("Others_Functions.R")
source ("modules/RShinyR6PocUI_moduleR6.R", local = TRUE)

BaseMiniRapportTabDynUI<-MiniRapportTabDynUI$new()
BaseFicheTabGraphUI<-FicheTabGraphUI$new()

source ("ui_server_rshiny/ui_rshiny/RShinyR6PoC_ui_rshiny.R", local = TRUE)  




shinyUI(
  RShinyR6PoC_ui_rshiny

)
