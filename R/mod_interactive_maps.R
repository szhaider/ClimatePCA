#' interactive_maps UI Function
#'
#' @description A shiny Module for the main PCA based Maps.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList observeEvent reactive tabPanel sidebarLayout sidebarPanel navbarPage div
#' @importFrom shinyWidgets pickerInput
#' @importFrom leaflet leaflet leafletOutput renderLeaflet addProviderTiles setView
#' @importFrom  leaflet leafletOptions
#' @importFrom htmltools HTML
#' @importFrom glue glue
#' @import ggplot2
#' @importFrom dplyr mutate select sym
#' @import recipes
#' @import tidyr
#' @importFrom purrr map_df possibly
#' @importFrom scales percent_format


mod_interactive_maps_ui <- function(id, ...){
  ns <- NS(id)
  tagList(
    # navbarPage(title = "CCDR GEO-Spatial PCA",
               tabPanel("INTERACTIVE MAPS",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            style = "background-color: white;",
                            tags$strong(tags$em(tags$h6("Select the features to compute the Geographic Targeting Index based on Principal Component Analysis (PC1)"))),
                            shinyWidgets::pickerInput(ns("features_selected"),
                                                      "Select featues for PCA",
                                                      choices =  mychoices,
                                                      selected = mychoices[1],
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple=TRUE),

                            br(),
                            # br(),
                            # Variance graph of the PCs selected

                            shiny::plotOutput(ns("var_explained_pcs"),
                                              height = "150px",
                                              width = '100%'),
                            br(),
                            shiny::fluidRow(shiny::actionButton(ns("pca_help"),
                                                                "HELP",
                                                                icon= icon("help"),
                                                                class = "btn-sm")),
                            br(),
                            shiny::fluidRow(shiny::downloadLink(ns("pca_download"),
                                                                "Download PCA",
                                                                icon= icon("download"),
                                                                class = "btn-sm"))
                          ),


                          shiny::mainPanel(
                            width = 9,
                            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                            tags$style(type = "text/css", "#main_map {height: calc(100vh - 80px) !important;}"),

                            leaflet::leafletOutput(ns("main_map"),
                                                   height = '100vh',
                                                   width = "75.5vw"
                            ),

                            tags$style(' #main_map {
                        position: relative;
                        margin-left: -26px;
                        padding: 0px;
                        }')
                          )
                        ))

    )

}

#' interactive_maps Server Functions
#'
#' @noRd
mod_interactive_maps_server <- function(id, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Lealfet static options
    output$main_map <- leaflet::renderLeaflet({
      # message("rendering local map")
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
        leaflet::addProviderTiles(provider =  "CartoDB.Voyager", group = "CARTO") %>%
        leaflet::setView(lng=69.5, lat = 30, zoom = 5)
    })

    #`Population (WorldPop 2020)`
    #Script for running Principal Component Analysis and prepping district level standing in PC planes

    #If no NAs in the selected columns, as it is calculation. Otherwise removing NAs


     data_pca_final <- shiny::reactive({

       data_pca_updated <- data_pca %>% dplyr::select(district, input$features_selected)
       if(any(is.na(data_pca_updated))) {
         data_pca_updated <- data_pca_updated %>% na.omit()
       }else{
         data_pca_updated
       }
     })

       # data_pca_1 <- data_pca %>% dplyr::select(district,
       #                                          `Population (WorldPop 2020)`,
       #                                          `Housing with improved roof and wall material (PSLM 2014)`)
       # if(any(is.na(data_pca_1))) {
       #   data_pca_1 <- data_pca_1 %>% na.omit()
       # }else{
       #   data_pca_1
       # }

      # data_pca %>%
      #   dplyr::select(district, input$features_selected)


    #Recipe for algorithm
    #District as ID,
    #Normalizing predictors to compare variability across disparate features
    #PCA step
    pca_rec <- shiny::reactive({
      recipes::recipe(~., data = data_pca_final()) %>%
        recipes::update_role(district, new_role = "id") %>%
        recipes::step_normalize(recipes::all_numeric_predictors()) %>%
        recipes::step_pca(recipes::all_numeric_predictors())
    })
    # pca_rec


    #Prepping PCA Recipe to execute steps

    pca_prep <- shiny::reactive({
      recipes::prep(pca_rec())
    })

    #Getting contribution of features to respective PCAs
    tidied_pca <- shiny::reactive({
      recipes::tidy(pca_prep(), 2)
    })

    #District level PCA scores
    pca_scores <- shiny::reactive({
      recipes::juice(pca_prep())
    })

    #To put omitted districts as NAs for Maps
    districts_for_nas <- shiny::reactive(districts)

    na_districts <- shiny::reactive({
      districts_for_nas() %>%         ##As a function insert
        dplyr::anti_join(pca_scores()) %>%
        dplyr::mutate(PC1 = NA)
    })

    #Updated pca score with NA districts scores as NAs
    pca_scores_updated <- shiny::reactive({
      pca_scores() %>%
        dplyr::bind_rows(na_districts()) %>%
        dplyr::arrange(district)
    })
    #Data for Maps
    map_data <- shiny::reactive({
      pca_scores_updated()
    })


    #Labelling
    labels_map <- reactive({
      paste0(glue("<b>District</b>: { pak_district$admin1Name } </br>"), "\n",
             glue("<b>Weighting scheme: </b> Principal Component Analysis (1)"), "<br/>",
             glue("<b>PTI score:</b> "), "\n",
             glue("{ round(map_data()$PC1, 4)  }"), sep = "") %>%
        lapply(htmltools::HTML)
    })

    pal <- reactive ({
      leaflet::colorBin(palette =  c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'),
                        bins= 5,
                        na.color = "grey",
                        domain = NULL,
                        map_data()[,"PC1"],
                        pretty = F,
                        reverse=F
      )

    })

    # Pal_legend
    pal_leg <- reactive ({
      leaflet::colorBin(palette = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'),
                        bins= 5,
                        na.color = "grey",
                        domain =(map_data()[,"PC1"]),
                        pretty = F,
                        reverse=F
      )
    })

    #Dynamic leaflet
    shiny::observeEvent(input$features_selected,{

      leaflet::leafletProxy("main_map", data=pak_district,
                            deferUntilFlush = TRUE) %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(label= labels_map(),
                             labelOptions = leaflet::labelOptions(
                               style = list("font-weight"= "normal",
                                            padding= "3px 8px",
                                            "color"= "black"),
                               textsize= "10px",
                               direction = "auto",
                               opacity = 0.9

                             ),
                             fillColor =  ~pal()(map_data()$PC1),
                             fillOpacity = 1,
                             stroke = TRUE,
                             color= "white",
                             weight = 1,
                             opacity = 0.9,
                             fill = TRUE,
                             dashArray = c(5,5),

                             smoothFactor = 0.8,
                             highlightOptions = leaflet::highlightOptions(weight= 2.5,
                                                                          color = "darkgrey",
                                                                          fillOpacity = 1,
                                                                          opacity= 1,
                                                                          bringToFront = TRUE),
                             group = "Polygons")


      leaflet::leafletProxy("main_map", data= map_data()) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend("bottomright",
                           pal= pal_leg(),
                           values= map_data()$PC1,
                           title = "PCA Scores",
                           opacity= 1,
                           labFormat = leaflet::labelFormat(
                             between = " : ",
                             digits = 2)
        )
    })

    #Plot for PCs
    #By final 5 PCs juiced up




    output$var_explained_pcs <- shiny::renderPlot({

      min_vars_for_var_exp <-  (input$features_selected)

      shiny::validate(
        shiny::need(!is.null(min_vars_for_var_exp), "Choose atleast one variable in the first tab for the chart to show up!")
      )

      sdev_id <- function(){
        pca_scores() %>%
          dplyr::select(-district) %>%
          purrr::map_df(., purrr::possibly(var, NA_integer_)) %>%
          tidyr::pivot_longer(everything(), names_to = "component", values_to = "value") %>%
          # group_by(component) %>%
          dplyr::mutate(
            percent_var = value/sum(value),
            cumsum = sum(percent_var))
      }

      sdev_id()  %>%
        # mutate(component = forcats::fct_reorder(factor(component), percent_var)) %>%
        ggplot2::ggplot(ggplot2::aes(component, percent_var))+
        ggplot2::geom_col(alpha=0.5, fill = "seagreen", width = 0.4)+
        # geom_point(aes(component,percent_var))+
        ggplot2::labs(x="Principal Components",
                      y="Variane Explained")+
        ggplot2::scale_y_continuous(labels = scales::percent_format())

    })

    output$pca_download <- downloadHandler(
      filename = function(){
        paste0("PCA Scores", ".csv")
      },
      content = function(file){
        write.csv(map_data(), file)
      }

    )

  })
}
