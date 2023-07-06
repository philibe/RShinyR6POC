#  called in SERVER by FicheTabGraph (server) and FicheTabGraphUI (ui)
FicheGraph = R6Class(
  "FicheGraph",
  public = list(
    id = NULL,
    ns =NULL,
    DetailsTableIn=NULL,

    # initializer
    initialize = function(input,output, session,id,DetailsTableIn,
                          RatioTable.Fct,RatioPlot.Fct,cible
    ){
      self$id = id
      self$ns = NS(session$ns(id))

      self$SetDetailsTableIn(DetailsTableIn)
      callModule(private$RatioPlotSERVER, self$id,self$DetailsTableIn, RatioTable.Fct,RatioPlot.Fct,cible )

    },

    SetDetailsTableIn = function(DetailsTableIn ) {
      if (missing(DetailsTableIn)) return(self$DetailsTableIn)
      self$DetailsTableIn<-DetailsTableIn
    },
    server= function(input, output, session,DetailsTableIn=self$DetailsTableIn,
                     RatioTable.Fct,RatioPlot.Fct,cible ) {

      callModule(private$RatioPlotSERVER, self$id,DetailsTableIn, RatioTable.Fct,RatioPlot.Fct,cible )

    }),
  private= list(
    RatioPlotSERVER = function(input, output, session,
                               DetailsTableIn,RatioTable.Fct,RatioPlot.Fct,cible ) {

      output[[cible]] <- renderPlot(RatioPlot.Fct( RatioTable.Fct(DetailsTableIn())))
    }
  )
)
