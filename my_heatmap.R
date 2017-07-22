# Patched leaflet.extras heat map showing gradient correctly.

heatmapDependency2 <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.heat",version = "0.2.0",
      system.file("htmlwidgets/lib/heat", package = "leaflet.extras"),
      script = c("leaflet-heat2.js", "heat-bindings.js")
    )
  )
}

addHeatmap2 = function(
  map, lng = NULL, lat = NULL, intensity = NULL, layerId = NULL, group = NULL,
  minOpacity = 0.05,
  max = 1.0, radius = 25,
  blur = 15, gradient = NULL, cellSize = NULL,
  data = leaflet::getMapData(map)
) {
  map$dependencies <- c(map$dependencies,
                        heatmapDependency2())
  
  #convert gradient to expected format from leaflet
  if (!is.null(gradient) && !is.function(gradient)) {
    gradient <- colorNumeric( gradient, 0:1 )
    gradient <- as.list(gradient(0:20 / 20))
    names(gradient) <- as.character(0:20 / 20)
  }
  
  pts = leaflet::derivePoints(
    data, lng, lat, missing(lng), missing(lat), "addHeatmap")
  
  if(is.null(intensity)) {
    points <- cbind(pts$lat, pts$lng)
  } else {
    if(inherits(intensity,'formula')) {
      intensity <- eval(intensity[[2]], data, environment(intensity))
    }
    points <- cbind(pts$lat, pts$lng, intensity)
  }
  
  leaflet::invokeMethod(
    map, data, 'addHeatmap', points,
    layerId, group,
    leaflet::filterNULL(list(
      minOpacity = minOpacity,
      max = max,
      radius = radius,
      blur = blur,
      gradient = gradient,
      cellSize = cellSize
    ))
  ) %>% leaflet::expandLimits(pts$lat, pts$lng)
}

#' removes the heatmap
#' @rdname heatmap
#' @export
removeHeatmap2 = function(map, layerId) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'removeHeatmap', layerId)
}

#' clears the heatmap
#' @rdname heatmap
#' @export
clearHeatmap2 = function(map) {
  leaflet::invokeMethod(map, NULL, 'clearHeatmap')
}