#Léna 
library(sf)
library(aws.s3)
library(janitor)
library(readr)
library(RPostgres)
library(RColorBrewer)
library(dplyr)
library(leaflet)


# Récupération des libellés des différentes cultures
lib_cult <- s3read_using(FUN = read_csv2,
                         object = "2023/sujet2/diffusion/ign/rpg/REF_CULTURES_GROUPES_CULTURES_2020.csv",
                         col_types = cols(.default = col_character()),
                         bucket = "projet-funathon",
                         opts = list("region" = "")) %>% clean_names()


lib_group_cult <- lib_cult %>% 
  select(code_groupe_culture, libelle_groupe_culture) %>% 
  distinct(code_groupe_culture, .keep_all=T)


# Création d'une palette de couleurs associée au groupe de culture
pal <- brewer.pal(12, "Paired")
pal <- colorRampPalette(pal)(24)
factpal <- colorFactor(pal, lib_group_cult$code_groupe_culture)











connect_to_db <-function(){
  cnx <- dbConnect(Postgres(),
                   user = "funathon4",
                   password = "postgre4",
                   host = "postgresql-758156.projet-funathon",
                   dbname = "defaultdb",
                   port = 5432,
                   check_interrupts = TRUE)
  return (cnx)
}



query_db <- function(cnx, lat, lng, radius) {
  # Les données spatiales sur PostgreSQL sont stockées en Lambert 93.
  # Pour faire le join on veut donc projeter les coordonnées `lat`` et `lng`
  postgis_crs <- "EPSG:2154"
  coordinates <- data.frame(lng = c(lng), lat = c(lat)) %>%
    st_as_sf(coords = c("lng", "lat"), remove = TRUE) %>%
    st_set_crs("EPSG:4326") %>%
    st_transform(postgis_crs)
  
  # Requête PostgreSQL
  query <- sprintf(
    "SELECT * FROM rpg.parcelles WHERE ST_Intersects(geom, ST_Buffer(ST_SetSRID(ST_MakePoint(%f, %f), 2154), %.0f));",
    st_coordinates(coordinates)[1],
    st_coordinates(coordinates)[2],
    radius*1000)
  
  # Récupération des résultats
  sf <- st_read(
    cnx,
    query = query
  )
  
  return(sf)
}

#cette fois-ci on a pas créer en dur la table point on a juste directement donné les informations du points qu'on a transformé selon les normes 
#ST_Intersects : intersection entre geom de la table rpg.parcelles et le buffer qui contient tous les poins dans un rayon radius*1000
# du point ST_SetSRID(ST_MakePoint(%f, %f) transformer en norme 2154 
#les %f font références aux variables float lister dans l'ordre d'apparition des %f pour insérer les valeurs 


# Création d'une palette de couleurs associée au groupe de culture
pal <- brewer.pal(12, "Paired")
pal <- colorRampPalette(pal)(24)
factpal <- colorFactor(pal, lib_group_cult$code_groupe_culture)



plot_surroundings <- function(leaflet_proxy, sf) {
  # Transformation de la projection (WGS 84)
  sf <- sf %>% st_transform(4326)
  
  # Ajout des libellés des cultures
  sf <- sf %>% 
    left_join(lib_cult %>% select(-code_groupe_culture), by = c("code_cultu" = "code_culture")) 
  
  # Création des labels à afficher au passage de la souris sur la carte.
  labels <- sprintf("<strong>Identifiant de la parcelle : </strong>%s<br/>
                    <strong>Groupe culture : </strong>%s<br/>
                    <strong>Culture : </strong>%s<br/>
                    <strong>Surface (ha) : </strong>%s<br/>
                    <strong>Département : </strong>%s<br/>
                    <strong>Commune : </strong>%s<br/>",
                    sf$id_parcel,
                    sf$libelle_groupe_culture,
                    sf$libelle_culture,
                    sf$surf_parc,
                    sf$insee_dep,
                    sf$nom_com) %>%
    lapply(htmltools::HTML)
  
  return(
    leaflet_proxy %>%
      addPolygons(
        data = sf,                                                              #table des données spatiales avec les codes cultures et les labels
        fillColor = ~factpal(code_group),                                       #palette 
        weight = 2,                                                             #esthétiqque des polygones
        opacity = 1,
        color = "#ffd966",
        dashArray = "3",
        fillOpacity = 0.5,
        highlight = highlightOptions(                                           #esthétique des hightlight
          weight = 5,
          color = "#A40000",
          dashArray = "",
          fillOpacity = 0.0,
          bringToFront = TRUE),
        label = labels,                                                         #affichage des labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          encoding="UTF-8"))
  )
}



compute_stats <- function(sf){
  nb_exploit<-nrow(parc_prox_lib)
  sau_tot<-sum(parc_prox_lib$surf_parc)
  
  t1 <- parc_prox_lib %>%
    st_drop_geometry() %>%
    group_by(code_group,libelle_groupe_culture) %>%
    summarise(
      Nombre_de_parcelle = n(),
      pourentage = round((n() / nb_exploit) * 100,2),
      Surface_ha = round(sum(surf_parc),2),
      Surface_pour = round((sum(surf_parc) / sau_tot) * 100,2),
    )%>% 
    mutate(code_group=as.numeric(code_group))%>%
    adorn_totals() %>% #rajouter le total 
    mutate(Taille_moyenne_ha = round( Surface_ha/ Nombre_de_parcelle, 2))

  
  return(t1%>% 
           select(-code_group)%>%
           set_names(c("Groupe de cultures", "Nombre de parcelles", "(%)", "Surface (ha)", 
                       "Surface (%)", "Surface moyenne d'une parcelle (ha)")))
    
}






















