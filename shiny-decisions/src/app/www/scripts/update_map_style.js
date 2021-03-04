let updateMapStyle = function(options) {
  let element = document.getElementById("updateMapStyles")
  let sepia = (100 - options.environment) / 100

  element.innerHTML = `
    #map-mainMap .leaflet-tile { filter: sepia(${sepia})}
  `
}
Shiny.addCustomMessageHandler('updateMapStyle', updateMapStyle)
