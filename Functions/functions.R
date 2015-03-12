prepPolygons <- function(x) {
  poly <- list(NULL)
  for (i in 1:length(x)) {
    gp <- 1
    if (length(stgt@polygons[i][[1]]@Polygons) == 2) {
      gp <- 2
    }
    poly[[i]] <- cbind(slot(x@polygons[i][[1]]@Polygons[[gp]], "coords")[, 1],
                       slot(x@polygons[i][[1]]@Polygons[[gp]], "coords")[, 2])
  }
  names(poly)
  return(poly)
}


reshapeData <- function(x) {
  n <- length(x)
  lat <- c()
  long <- c()
  ID <- names(x)
  for (i in 1:n) {
    if (i < n) {
      lat <- c(lat, x[[i]][, 2], NA)
      long <- c(long, x[[i]][, 1], NA)
    } else {
      lat <- c(lat, x[[i]][, 2])
      long <- c(long, x[[i]][, 1])
    }
  }
  return(list(ID=ID, lat=lat, long=long))
}


getColour <- function(xMin, xMax, allValues, values, nCol=10, n) {
  # colPal <- c("deepskyblue", "blue", "gold", "darkorange", "red") #
  colfunc <- colorRampPalette(c("white", "deepskyblue"))
  # colT <- hist(plot = FALSE, values, breaks = nCol + 1)$breaks
  colT <- quantile(values, probs = seq(0, 1, length=nCol+1))
  r <- range(values)
  colT <- seq(r[1], r[2], length=nCol)
  colPal <- colfunc(nCol)
  opts <- list(NULL)
  for (i in 1:n) {
    idCol <- which(values[i] <= colT)[1]
    opts[[i]] <- list(color='black', opacity=1, fillColor=colPal[idCol], fillOpacity=.75, weight = 0.8)
  }
  return(opts)
}
