FakeDatas <- reactive({
  vector_calc<-  c("disp","hp","drat","wt","qsec")
  (mtcars
    %>% mutate(rowname=rownames(.),
               TR=ifelse(cyl!=6,"NORM","TR")
    )
    %>% separate(rowname,c("brand","model"), sep=" ", fill="right", extra="merge")
    %>% rename_at(vars(all_of(vector_calc)),list(calc=~paste0(.,"_calc")) )
    %>% select (brand, model,everything())
    %>% select_at(vars(-contains("calc"),contains("calc")))
  )
}

)


DetailsTable <-  reactive({

  input_appelant=  input$MaitreTable_rows_selected
  validate(
    need(!is.null(input_appelant) , "select a line above (for example : Merc")
  )

  res<-  data.frame(stringsAsFactors = FALSE)
  isolate(FakeDatas())%>% filter (brand==isolate(MaitreTable())[as.integer(input_appelant), ])

})


consolidationDatas <- reactive({

  res<-DetailsTable()

  if ( DescTools::Coalesce(input$CheckbFilter,FALSE)==FALSE) {

    res<-(res  %>% filter (is.na(TR) | TR=="NORM")
    )
  }

  if (nrow(res)>0)  {
    return(res)
  } else {
    return( res [FALSE,])
  }

})



DetailsTable_filled<-reactive ({

  if (
    DescTools::Coalesce(nrow(DetailsTable()),0)>0
  ) TRUE else NULL
})



observeEvent(DetailsTable_filled(),
             {
               FirstExample<-MiniRapportTabDyn$new(input, output, session,"FirstExample",
                                                   div(
                                                     fluidRow(
                                                       column (3,
                                                               div(
                                                                 p(checkboxInput("CheckbFilter",
                                                                                 "checked: take the TR",
                                                                                 FALSE,
                                                                                 width="100%"
                                                                 ))
                                                               )
                                                       )
                                                     )
                                                   )

               )
               FirstExample$server(input, output, session,
                                   reactive(input$MaitreTable_rows_selected),
                                   reactive(consolidationDatas()) ,
                                   list( c(1,"basic report (brand)","brand"),
                                         c(2,"other report (brand,model)","brand,model")),
                                   Global.detail.synthese.table.output.fct
               )
             }
             ,ignoreNULL = TRUE  ,once=TRUE
)

observeEvent(input$tabs,
             {
               if (input$tabs=="2") {
                 FicheTabGraph$new(input, output, session,"SecondExample",
                                   list("datas","graphs"),
                                   list("RatioPlotUI","RepartitionCoutPlotUI"),
                                   reactive(DonneesPie()),
                                   DetailsTableInFormatOutput.Fct=Global.Fiche.output.fct
                 )
                 FicheGraph1<-FicheGraph$new(input, output, session,"SecondExample",reactive(DonneesPie()),
                                             pie_plot_table.fct,
                                             pie_plot_plot.fct,
                                             cible="RatioPlotUI"
                 )
                 FicheGraph1
                 FicheGraph2<-FicheGraph1$clone(deep=TRUE)
                 FicheGraph2$server(input, output, session,
                                    RatioTable.Fct=pie_plot_table.fct,
                                    RatioPlot.Fct=pie_doubleplot_plot.fct,
                                    cible="RepartitionCoutPlotUI"
                 )
               }
             }
             ,ignoreInit=TRUE,once=TRUE
)
MaitreTable <-  reactive({

  unique(isolate(FakeDatas()) %>% select(brand)%>% arrange(brand))
})


output$MaitreTable <- DT::renderDataTable(
  DT::datatable( MaitreTable(),
                 style = "bootstrap",   class = "compact", filter='top',
                 selection = c("single"),
                 options = list(
                   deferRender = TRUE,
                   bSortClasses = TRUE,iDisplayLength = 3,   width = "100%",
                   scrollX=TRUE,
                   autoWidth = TRUE
                 )
  )
)


output$DetailsTable <- DT::renderDataTable(
  DT::datatable( DetailsTable()      ,
                 style = "bootstrap",   class = "compact", filter='top',
                 selection = c("single"),
                 options = list(
                   deferRender = TRUE,
                   bSortClasses = TRUE,iDisplayLength = 3,   width = "100%",
                   scrollX=TRUE,
                   autoWidth = TRUE
                 )
  )
)
