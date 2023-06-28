library(shiny)
library(leaflet)


# Définition du serveur
server <- function(input, output) {
  
  # Rendre la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("http://wxs.ign.fr/essentiels/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}") %>%
      setView(lng = -1.4932577154775046, lat = 46.46946179131805, zoom = 12)
  })
  
  # Connexion à la base de données
  cnx <- connect_to_db()                                                        #fonction codé par nous dans util.R
  
  # Initialisation d'une "reactive value" pour le point sélectionné
  selectedPoint <- reactiveValues(lat = NULL, lng = NULL)                       
  
  # Gestion de l'évènement "clic"
  observeEvent(input$map_click, {
    
    clickData <- input$map_click
    if (!is.null(clickData)) {
      # Stockage des coordonnées du point
      selectedPoint$lat <- clickData$lat
      selectedPoint$lng <- clickData$lng
      
      buffer_radius <- input$buffer_radius
      sf <- query_db(cnx, selectedPoint$lat, selectedPoint$lng, buffer_radius)  #fonction codé dans utils.R
      
      # Mise à jour de la carte
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lng = selectedPoint$lng, lat = selectedPoint$lat) %>%
        plot_surroundings(sf)                                                   #fonction codé dans utils.R
      
      # Calculs sur les données de parcelles
      df <- compute_stats(sf)                                                   #fonction codé dans utils.R
      
      # Update de la table affichée
      output$table <- renderTable({
        df
      })
    }
  })
  
  observeEvent(input$buffer_radius, {
    # Vérification qu'un point a été sélectionné
    if (!is.null(selectedPoint$lat) && !is.null(selectedPoint$lng)) {
      # Requête avec le nouveau rayon
      buffer_radius <- input$buffer_radius
      sf <- query_db(cnx, selectedPoint$lat, selectedPoint$lng, buffer_radius)
      
      # Mise à jour de la carte
      leafletProxy("map") %>%
        clearShapes() %>%
        plot_surroundings(sf)
      
      # Calculs sur les données de parcelles
      df <- compute_stats(sf)
      
      # Update de la table affichée
      output$table <- renderTable({
        df
      })
    }
  })
}