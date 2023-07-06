# called in UI
MiniRapportTabDynUI = R6Class(
  "MiniRapportTabDynUI",
  public = list(
    MiniRapportTabDynUI_UI= function (prefixe, tagParamFiltre){
      ns<-NS(prefixe)
      tagList(
        uiOutput(ns("MiniRapportTabDynUI_UI"))
      )
    }
  )
)


# called in SERVER
MiniRapportTabDyn = R6Class(
  "MiniRapportTabDyn",
  public = list(
    id = NULL,
    ns =NULL,
    ConsolidationFormatOutput.Fct=NULL,
    DetailsTable=NULL,
    RapportsList=NULL,
    RapportCourant.react=NULL,
    liste_colonnes_choisies.react=NULL,
    reactValues=NULL,
    # initializer
    initialize = function(input, output, session,id, tagParamFiltre=div()){
      self$id = id
      self$ns = NS(session$ns(id))
      callModule(self$MiniRapportTabDynSERVER, self$id, tagParamFiltre )
      self$ConsolidationFormatOutput.Fct=function (mydatatable) {DT::datatable( mydatatable)}
    },
    MiniRapportTabDyn_renderUI= function (tagParamFiltre=div()){
      tagList(
        fluidRow(
          
          fluidRow(div(bsCollapsePanel_panneau_masquable.fct("Click on column name (are excluded columns whith calc, qte, num )",
                                                             div(
                                                               p("Click on column name (are excluded columns whith calc, qte, num )"),
                                                               column (12,
                                                                       div(
                                                                         uiOutput(self$ns("ChoixDimRegroupUI"))
                                                                         #, style=""
                                                                       )
                                                               )
                                                             )
          ), style="margin-left: 20px;"))
        ),
        fluidRow(
          column (12,
                  uiOutput(self$ns("ChoixDimRegroupChoisiUI"))
          )
        ),
        tagParamFiltre,
        fluidRow(
          column (12,
                  div(
                    div(uiOutput(self$ns("ChoixRapportUI")),
                        class='label_non_fixe_items_fixes'
                    )
                  )
          ) ,
          column (12,
                  div( DT::dataTableOutput(self$ns("ConsolidationDataTableUI")),
                       class="data_table_output")
          )
        )
      )
      
    },
    MiniRapportTabDynSERVER = function(input, output, session, tagParamFiltre = div()) {
      output$MiniRapportTabDynUI_UI<- renderUI(self$MiniRapportTabDyn_renderUI(tagParamFiltre  ))
    },
    server= function(input, output, session, MaitreTable_rows_selected,DetailsTable,RapportsList,
                     ConsolidationFormatOutput.Fct = NULL ){
      private$SetDetailsTable(DetailsTable)
      private$SetRapportsList( RapportsList)
      callModule(private$ChoixDimRegroupSERVER, self$id, MaitreTable_rows_selected)
      callModule(private$ChoixRapportSERVER, self$id )
      callModule(private$ChoixDimRegroupChoisiSERVER, self$id )
      private$SetConsolidationFormatOutput.Fct(ConsolidationFormatOutput.Fct)
      callModule(private$ConsolidationDataTableSERVER, self$id )
    }
    
  ),
  private = list(
    
    ListeColonnesDuChoixRapports.fct=function (DetailsTable =   self$DetailsTable) {
      
      list_colonnes=names(DetailsTable()  )
      list_colonnes<-list_colonnes[!grepl("calc|qte|num",list_colonnes)]
      
      list_colonnes<-list_colonnes[order(list_colonnes)]
      list_colonnes
    },
    RapportCourant.fct=function(input_choix_rapport, ListeRapportsDf=private$ListeRapportsDf()){
      selection<-((ListeRapportsDf
                   # attention le Coalesce est avec un 1, comme rapport 1
                   %>% filter (value==DescTools::Coalesce(input_choix_rapport,1))
                   %>% select (choix_dim_regroup)
      )[[1]]
      )
      selection <- str_split(selection,",")[[1]]
      selection
      
    },
    
    
    checkboxGroupInput_renderUI= function (input_maitre_rows_selected,
                                           ListeColonnesDuChoixRapports=private$ListeColonnesDuChoixRapports.fct(),
                                           ElementsCoches = self$liste_colonnes_choisies.react()
                                           
    )
    {
      #print(input_maitre_rows_selected)
      if (DescTools::Coalesce(input_maitre_rows_selected,0)!=0) {
        checkboxGroupInput(self$ns("ChoixDimRegroup"),
                           label = "",
                           choices  = ListeColonnesDuChoixRapports,
                           inline = TRUE,
                           selected = ElementsCoches
        )
        
      }else return()
    },
    ChoixDimRegroupSERVER = function(input, output, session,
                                     input_maitre_rows_selected
    ) {
      self$reactValues<-reactiveValues(choix="RapportCourant")
      self$RapportCourant.react<-reactive({
        private$RapportCourant.fct(input$ChoixRapport)
      })
      observeEvent(input$ChoixDimRegroup,
                   self$reactValues$choix<-"ChoixDimRegroup"
      )
      observeEvent(input$ChoixRapport,
                   self$reactValues$choix<-"RapportCourant"
      )
      self$liste_colonnes_choisies.react<-reactive(private$liste_colonnes_choisies.fct(input$ChoixDimRegroup, RapportCourant=self$RapportCourant.react()))
      output$ChoixDimRegroupUI <- renderUI(private$checkboxGroupInput_renderUI(input_maitre_rows_selected()  ))
    },
    
    ListeRapportsDf=function (RapportsList=self$RapportsList) {
      
      setNames(
        data.frame(
          t(data.frame(
            RapportsList
          ))
          ,row.names = NULL,stringsAsFactors = FALSE
        ),
        c("value","label","choix_dim_regroup")
      )
    },
    ListeRapportsSetNames=function (ListeRapportsDf= private$ListeRapportsDf()) {
      
      
      list_label_value <- ListeRapportsDf
      
      setNames(list_label_value$value,list_label_value$label)
    },
    
    selectizeInput_create_renderUI  =function(ListeRapportsSetNames=private$ListeRapportsSetNames()) {
      selectizeInput(self$ns( "ChoixRapport"),
                     label="Report Choice",
                     choices =ListeRapportsSetNames,
                     width = '500px',
                     selected = "1"
                     #  , options = list(render = I(''))
      )
    },
    RapportChoisi_renderUI  =function(list_colonnes) {
      paste(unlist(list_colonnes),collapse=', ')
    },
    liste_colonnes_choisies.fct=function(input_ChoixDimRegroup,
                                         RapportCourant,
                                         Choix =self$reactValues$choix
    ) {
      list_colonnes<-switch (Choix,
                             "ChoixDimRegroup"= input_ChoixDimRegroup,
                             "RapportCourant"= RapportCourant,
                             RapportCourant
      )
      list_colonnes
    },
    ConsolidationDataTable_renderDT=function(list_colonnes,
                                             DetailsTable=self$DetailsTable,
                                             ConsolidationFormatOutput.Fct=self$ConsolidationFormatOutput.Fct){
      res<-NULL
      
      res<-  DetailsTable()
      
      if (!is.null(res)) {
        
        
        res2 <- (res
                 %>% group_by_at(., .vars = (intersect(list_colonnes,colnames(res))))
                 %>% summarise_at(vars(contains("calc", ignore.case = TRUE)),~sum(., na.rm = TRUE))
        )
        res_datas<-res2
      }else {
        res_datas<-data.frame(stringsAsFactors = FALSE)
      }
      ConsolidationFormatOutput.Fct(res_datas)
      
    },
    ChoixRapportSERVER = function(input, output, session ) {
      output$ChoixRapportUI <- renderUI(private$selectizeInput_create_renderUI())
      
    },
    ChoixDimRegroupChoisiSERVER = function(input, output, session ) {
      output$ChoixDimRegroupChoisiUI <- renderUI(private$RapportChoisi_renderUI(
        self$liste_colonnes_choisies.react()
      ))
    },
    ConsolidationDataTableSERVER = function(input, output, session ) {
      output$ConsolidationDataTableUI <- DT::renderDataTable(private$ConsolidationDataTable_renderDT(
        self$liste_colonnes_choisies.react()
      ))
      
    },
    SetDetailsTable = function(DetailsTable ) {
      self$DetailsTable<-DetailsTable
    },
    SetRapportsList = function(RapportsList ) {
      RapportsList<-lapply(RapportsList, function (x,p,r) {
        # To delete spaces from 3rd item
        x[3]<-str_replace_all(x[3],p,r);
        x
      }," ","")
      self$RapportsList<-RapportsList
    },
    SetConsolidationFormatOutput.Fct = function(ConsolidationFormatOutput.Fct=NULL ) {
      if (!is.null(ConsolidationFormatOutput.Fct)) {
        self$ConsolidationFormatOutput.Fct<-ConsolidationFormatOutput.Fct
        
      }
      
    }
    
  )
)
