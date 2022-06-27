# UI

ui<-fluidPage(

  navbarPage("DO sites", id="nav",

             tabPanel("Interactive map"
                      , div(class="outer"

                            , tags$head(
                              includeCSS("styles.css")
                              , includeScript("gomap.js")
                            )

                            , leafletOutput("map", width="100%", height="100%")

                            , absolutePanel(id = "controls", class = "panel panel-default"
                                            , fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 20
                                            , bottom = "auto", width = 330, height = "auto"

                                            , h3("Site Type Screen")

                                            , checkboxGroupInput(inputId = "CBPCoreMap"
                                                                 , label = ""
                                                                 , choices = c("CBP" = TRUE, "Supplemental" = FALSE)
                                                                 , selected = c(TRUE, FALSE)
                                            )

                                            , hr()

                                            , h3("Episode Screen")
                                            , numericInput(inputId = "minEpisodesMap"
                                                           , label = "Minimum Number of Episodes:"
                                                           , min = 0
                                                           , max = 600
                                                           , step = 10
                                                           , value = c(0)
                                                           , width = "300px"
                                            )
                                            , numericInput(inputId = "maxEpisodesMap"
                                                           , label = "Maximum Number of Episodes:"
                                                           , min = 0
                                                           , max = 600
                                                           , step = 10
                                                           , value = c(600)
                                                           , width = "300px"
                                            )

                                            , hr()

                                            , h3("Year Screen")
                                            , numericInput(inputId = "minYearMap"
                                                           , label = "First Year of Collected in this Year or Earlier: (Decreasing the below value closer to 1990 makes sure that older data are available.)"
                                                           , min = 1990
                                                           , max = 2019
                                                           , value = c(2019)
                                                           , width = "300px"
                                            )
                                            , numericInput(inputId = "maxYearMap"
                                                           , label = "Last Year of Collected in this Year or Later:  (Increasing the below value closer to 2019 makes sure that more recent data are available.)"
                                                           , min = 1990
                                                           , max = 2019
                                                           , value = c(1990)
                                                           , width = "300px"
                                            )

                            ) # absolutePanel

                            , tags$div(id="cite"
                                       , 'Data compiled from CBP DataHub, 1990-2019'
                            )

                      ) # div
             ) # tabPanel("Interactive map" ...

             , tabPanel("Data explorer"
                        , fluidRow(

                          column(2
                                 , h4("Site Type")
                                 , checkboxGroupInput(inputId = "CBPCoreTable"
                                                      , label = ""
                                                      , choices = c("CBP" = TRUE, "Supplemental" = FALSE)
                                                      , selected = c(TRUE, FALSE)
                                 )
                          )

                          , column(2
                                   , h4("Episode Screen")
                                   , numericInput(inputId = "minEpisodesTable"
                                                  , label = "Minimum Number of Episodes:"
                                                  , min = 0
                                                  , max = 600
                                                  , step = 10
                                                  , value = c(0)
                                                  , width = "300px"
                                   )
                                   , numericInput(inputId = "maxEpisodesTable"
                                                  , label = "Maximum Number of Episodes:"
                                                  , min = 0
                                                  , max = 600
                                                  , step = 10
                                                  , value = c(600)
                                                  , width = "300px"
                                   )
                          )

                          , column(2
                                   , h4("Year Screen")
                                   , numericInput(inputId = "minYearTable"
                                                  , label = "First Year of Collected in this Year or Earlier: (Decreasing the below value closer to 1990 makes sure that older data are available.)"
                                                  , min = 1990
                                                  , max = 2019
                                                  , value = c(2019)
                                                  , width = "300px"
                                   )
                          )

                          , column(2
                                   , h4("Year Screen")
                                   , numericInput(inputId = "maxYearTable"
                                                  , label = "Last Year of Collected in this Year or Later:  (Increasing the below value closer to 2019 makes sure that more recent data are available.)"
                                                  , min = 1990
                                                  , max = 2019
                                                  , value = c(1990)
                                                  , width = "300px"
                                   )
                          )

                        ) # fluidflow

                        , hr()
                        , DT::dataTableOutput("ziptable")

             ) # tabPanel("Data explorer"

             , conditionalPanel("false", icon("crosshair"))

  ) # navbarPage

) # ui<-fluidPage(...)
