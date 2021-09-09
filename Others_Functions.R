formatRound.try.fct <- function(mydatatable, mycolumn, taille) {
  tryCatch({
    return(DT::formatRound(mydatatable, mycolumn, taille))
  }, error = function(cond) {
    print(paste0("Warning: Erreur de nom de colonne (", mycolumn, ") pour formatRound"))
    return(mydatatable)
  })
}



Global.Fiche.output.fct <- function (mydatatable) {
  res<-DT::datatable( mydatatable,
                      style = "bootstrap",   class = "compact", filter='top',
                      selection = c("none"),
                      options = list(
                        deferRender = TRUE,   bSortClasses = TRUE,iDisplayLength = 30,   width = "100%",
                        scrollX=TRUE,   autoWidth = TRUE
                      )
  )



  return (res)
}


Global.detail.synthese.table.output.fct <- function (mydatatable) {
  res<-DT::datatable( mydatatable,

                      style = "bootstrap",   class = "compact", filter='top',
                      selection = c("single"),
                      options = list(
                        deferRender = TRUE,   bSortClasses = TRUE,iDisplayLength = 30,   width = "100%",
                        scrollX=TRUE,   autoWidth = TRUE
                      )
  )

  res <- (res
          %>% formatRound.try.fct('disp_calc', 2)
          %>% formatRound.try.fct('hp_calc', 2)
          %>% formatRound.try.fct('drat_calc', 2)
  )

  return (res)
}


DonneesPie<- reactive(
  data.frame(
    state = c('eaten', 'eaten but said you didn\'t', 'cat took it',
              'for tonight', 'will decompose slowly'),
    focus = c(0.2, 0, 0, 0, 0),
    start = c(0, 1, 2, 3, 4),
    end = c(1, 2, 3, 4, 2*pi),
    amount = c(4,3, 1, 1.5, 6),
    coul=c(1,"aa","aa","bb","bb"),
    stringsAsFactors = FALSE
  )
)

pie_plot_table.fct=function (pie) {
  pie %>%
    mutate(end=2*pi*cumsum(amount)/sum(amount),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1),
           label=paste(state, paste0(round(((amount/sum(amount))*100),2),"%;",amount,"euros"))
    )
}

pie_plot_plot.fct=function(pie){
  ggplot(pie) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,amount = amount,
                     fill = label,explode = focus),stat = 'pie') +
    ggtitle("Plot of length by dose") +
    labs(fill = "Dose (mg)")+
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle),
                  label = label, hjust = hjust, vjust = vjust
    )) +
    coord_fixed() +theme_no_axes() +
    scale_x_continuous(limits = c(-2, 2),  name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5),    name = "", breaks = NULL, labels = NULL)


}

pie_doubleplot_plot.fct=function(mydata){

  mydata<-mydata

  p0<-ggplot(mydata)+ ggtitle("Plot of length by dose") +
    coord_fixed() +theme_no_axes() +
    scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5),      # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL)

  toto<-unlist(list(colorspace::qualitative_hcl(length(mydata$coul),"Dynamic"),
                    colorspace::qualitative_hcl(length(mydata$label),"Dark 3")))


  titi<-setNames(toto,unlist(list(mydata$coul,mydata$label)))

  p1<-p0 +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,amount = amount,
                     fill = label,explode = focus),stat = 'pie') +
    labs(fill = "ratio")  +scale_fill_manual(values =titi)


  p2<-p0+
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.5,amount = amount,
                     fill = coul,explode = focus),stat = 'pie',data=mydata) +
    labs(fill = "produit")+  scale_fill_manual(values =titi)

  ptotal<-p0 +

    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.5,amount = amount,
                     fill = coul,explode = focus),stat = 'pie',data=mydata) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,amount = amount,
                     fill = label,explode = focus),stat = 'pie',data=mydata) +
    scale_fill_manual(values = titi)+geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle),
                                                   label = label, hjust = hjust, vjust = vjust
    ))

  plot_grid(ptotal+ theme(legend.position = "none"),
            plot_grid(
              get_legend(p1 + theme(legend.position = "right",plot.margin = unit(c(0,0,0,0), "cm"))),
              NULL,
              get_legend(p2 + theme(legend.position = "bottom",plot.margin = unit(c(0,0,0,0), "cm"))),
              rel_heights =  c(1, -0.7, 1), ncol=1
            )
  )
}


bsCollapsePanel_panneau_masquable.fct<- function (titre,contenu) {
  div(shinyBS::bsCollapsePanel(titre,"",
                               contenu
  ),class="bsCollapsePanel-petite")
}
