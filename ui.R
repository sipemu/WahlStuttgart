library(leaflet)
library(ShinyDash)

shinyUI(navbarPage("Bundestagswahl 2013", id="nav",
                   tabPanel("Interaktive Karte",
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("style.css"),
                                  includeScript("geomap.js")
                                ),
                                leafletMap("map", width="100%", height="100%",
                                           initialTileLayer = "//{s}.tiles.mapbox.com/v3/muon-stat.20b74f26/{z}/{x}/{y}.png",
                                           initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                  options = list(
                                    center = c(48.77735, 9.180072),
                                    zoom = 11,
                                    maxBounds = list(list(17, -180), list(59, 180))
                                  )
                                ),
                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              h2("Zweitstimmen"),
                                              selectInput("Parteien", "Partei", vars),
                                              br(),
                                              plotOutput("histCentile", height = 200),
                                              br(),
                                              br(),
                                              tags$div(id="cite",
                                                       'Daten und Shapefiles: ', 
                                                       tags$em('Stadt Stuttgart: '), 
                                                       tags$a("Bürgerservice", href="http://www.stuttgart.de/item/show/504442")
                                                       )
                                              )
                                )
                            ),
                   tabPanel("Über",
                            p("Nach einer Vorlage von RStudio:", tags$a("RStudio", href="http://shiny.rstudio.com/gallery/superzip-example.html")),
                            p("Die App wurde erstellt von", tags$a("MUON-STAT", href="http://muon-stat.com"))
                            )
                            
                   )
)
