#  called in UI
FicheTabGraphUI = R6Class(
  "FicheTabGraphUI",
  public = list(
    FicheTabGraphUI_UI= function (prefixe){
      ns<-NS(prefixe)
      tagList(
        uiOutput(ns("FicheTabGraphUI_UI"))
      )
    }
  )
)

#  called in SERVER
FicheTabGraph = R6Class(
  "FicheTabGraph",
  public = list(
    id = NULL,
    ns =NULL,
    ListeTitres=NULL,
    ListeIdGraphs=NULL,
    DetailsTableIn=NULL,
    RapportCourant.react=NULL,
    DetailsTableInFormatOutput.Fct=NULL ,
    # initializer
    initialize = function(input,output, session,id,ListeTitres,ListeIdGraphs,DetailsTableIn,
                          DetailsTableInFormatOutput.Fct =NULL){
      self$id = id
      self$ns = NS(session$ns(id))
      self$SetListeTitres(ListeTitres)
      self$SetListeIdGraphs(ListeIdGraphs)
      self$DetailsTableInFormatOutput.Fct=function (mydatatable) {DT::datatable( mydatatable)}
      callModule(private$FicheTabGraphSERVER,self$id )
      private$server(input, output, session, DetailsTableIn,DetailsTableInFormatOutput.Fct)
    },
    SetListeTitres=function (ListeTitres){
      self$ListeTitres= ListeTitres
    },
    SetListeIdGraphs=function (ListeIdGraphs){
      self$ListeIdGraphs= ListeIdGraphs
    },
    FicheTabGraph_renderUI= function (ListeTitres=self$ListeTitres){

      tagList(
        fluidRow(
          h4(ListeTitres[[1]]),
          column (12,
                  div(
                    DT::dataTableOutput(self$ns("FichePrixTableUI")),
                    class="data_table_output"
                  )
          )
        ),
        fluidRow(
          h4(ListeTitres[[2]]),

          column (12,
                  div(
                    self$FichePrixPlotUI_UI()
                  )
          )
        )
      )
    },
    FichePrixPlotUI_UI = function(ListeIdGraphs= self$ListeIdGraphs){
      divGraphs <- div()
      for (num in 1:length(ListeIdGraphs))  {
        divGraphs <- tagAppendChild(divGraphs, column (6,plotOutput(self$ns(ListeIdGraphs[[num]]))))
      }
      tagList(
        divGraphs
      )
    }
  ),

  private = list(
    SetDetailsTableIn = function(DetailsTableIn ) {
      self$DetailsTableIn<-DetailsTableIn
    },
    DetailsTableSERVER = function(input, output, session ) {

      output$FichePrixTableUI <- DT::renderDataTable(self$DetailsTableInFormatOutput.Fct(self$DetailsTableIn())
      )
    },
    SetDetailsTableInFormatOutput.Fct= function(DetailsTableInFormatOutput.Fct=NULL ) {
      if (!is.null(DetailsTableInFormatOutput.Fct)) {
        self$DetailsTableInFormatOutput.Fct<-DetailsTableInFormatOutput.Fct

      }
    },

    FicheTabGraphSERVER = function(input, output, session) {
      output$FicheTabGraphUI_UI<- renderUI(self$FicheTabGraph_renderUI(  ))
    },
    server= function(input, output, session, DetailsTableIn,
                     DetailsTableInFormatOutput.Fct =NULL){
      private$SetDetailsTableIn(DetailsTableIn)
      private$SetDetailsTableInFormatOutput.Fct(DetailsTableInFormatOutput.Fct)
      callModule(private$DetailsTableSERVER, self$id )

    }
  )
)

