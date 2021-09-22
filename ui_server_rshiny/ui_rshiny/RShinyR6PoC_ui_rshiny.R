RShinyR6PoC_ui_rshiny<-
  fluidPage(
    useShinyjs(),
    # HTML(
    #   "
    #               <script>
    #                 $(document).on('shiny:inputchanged', function(event) {
    #                   if (event.name != 'EventsChanged' && event.name.substring(0,1)!='.') {
    #                     Shiny.setInputValue('EventsChanged', event.name);
    #
    #                   }
    #                 });
    #               </script>
    #               "
    # ),
    tags$style(type = "text/css", HTML(paste0(".data_table_output {font-size:80%;white-space: nowrap;width:",largeur_page_pct,"%;}"))),
    tags$style(type = "text/css", HTML(paste0("
                                    .bsCollapsePanel-petite {width:",largeur_page_pct,"%;
                                              -webkit-transition-delay: 0s;
                                              transition-delay: 0s;
                                              margin-bottom: -20px;
                                              }","
                                              .bsCollapsePanel-petite .panel-body { padding: 0px;}
                                              .bsCollapsePanel-petite .panel-title {font-size:80%;}
                                              .bsCollapsePanel-petite .panel-heading {padding: 0px;}
                                              "))),
    fluidPage(tabsetPanel(id = "tabs",
                tabPanel("First Example", value="1",
                         h1("First Example"),
                         DT::dataTableOutput('MaitreTable'),
                         fluidRow(
                           h2("select a line above to have mini report below "),p("for example 'Merc'")
                         ),
                         fluidRow(
                           BaseMiniRapportTabDynUI$MiniRapportTabDynUI_UI("FirstExample")
                         ),
                         fluidRow(
                           h4("Details"),

                           column (12,
                                   div(DT::dataTableOutput('DetailsTable'),
                                       class="data_table_output")
                           )
                         )),

                tabPanel("Second Example",value="2",
                         fluidRow(
                           div(
                             BaseFicheTabGraphUI$FicheTabGraphUI_UI("SecondExample"),
                             style="margin-left: 20px;"
                           )
                         )
                )
    ))
  )
