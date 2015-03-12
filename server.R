bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

library(shiny)
source("Functions/functions.R")
load("data.RData")

shinyServer(function(session, input, output) {

  map <- createLeafletMap(session, 'map')

  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      if (is.null(input$Parteien)) {
        return(NULL) 
      }
      
      resDat <- reshapeData(polygonsStgt)
      datP <- subset(dataWm, variable == input$Parteien)
      
      opts <- getColour(min(datP$value), 
                        max(datP$value), 
                        dataWm$value, 
                        datP$value, 
                        20, 
                        nrow(datP))
      map$addPolygon(resDat$lat, resDat$long, layerId = names(polygonsStgt), options = opts)
    })
    session$onSessionEnded(paintObs$suspend)
  })
  
  # Show a popup at the given location
  showPercent <- function(partei, lat, lng) {
    
    datP <- subset(dataWm, WAHLBEZIRK == partei)
    or <- order(datP$value, decreasing = T)
    top5 <- datP[or, ]
    top5 <- top5[1:5, ]
    datP <- subset(dataWm, variable == input$Parteien)
    datBW <- subset(datP, WAHLBEZIRK == partei)
    datBWAllS <- subset(dataBWAll, WAHLBEZIRK == partei)
    content <- as.character(tagList(
      tags$h4(paste0("Ergebnis ", input$Parteien, ":"), paste0(datBW$value, "%")),
      tags$h5(paste0("Wahlbezirk: ", partei)),
      tags$h5(datBWAllS$STADTTEILE_NAMEN), 
      tags$h5(paste0("Wahllokal: ", datBW$WAHLLOKAL)),
      sprintf(paste0("1. ", top5$variable[1], " %s"), paste0(top5$value[1]), "%"), tags$br(),
      sprintf(paste0("2. ", top5$variable[2], " %s"), paste0(top5$value[2]), "%"), tags$br(),
      sprintf(paste0("3. ", top5$variable[3], " %s"), paste0(top5$value[3]), "%"), tags$br(),
      sprintf(paste0("4. ", top5$variable[4], " %s"), paste0(top5$value[4]), "%"), tags$br(),
      sprintf(paste0("5. ", top5$variable[5], " %s"), top5$value[5]), tags$br()
    ))
    map$showPopup(lat, lng, content, partei)
  }
  
  # When map is clicked, show a popup with info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      id <- which(names(polygonsStgt) == event$id)
      lat <- mean(polygonsStgt[[id]][, 2])
      lng <- mean(polygonsStgt[[id]][, 1])
      showPercent(event$id, lat, lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (is.null(input$Parteien))
      return(NULL)
    
    datP <- subset(dataWm, variable == input$Parteien)
    colT <- quantile(datP$value, probs = seq(0, 1, length=20))
    r <- range(datP$value)
    colfunc <- colorRampPalette(c("white", "deepskyblue"))
    colPal <- colfunc(19)
    hist(datP$value, ylab = "Anteil", freq = FALSE,
         main = "Verteilung der relativen Werte",
         xlab = "Prozent",
         xlim = c(0, range(datP$value)[2]),
         col = colPal,
         border = 'black',
         breaks=colT)
  })
})
