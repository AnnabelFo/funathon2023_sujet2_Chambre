##Fichier utilitaire avec toutes les fonctions utiles pour le fichier server.R

#Connexion à PostgreSQL
connect_to_db <- function() {
  conn <- dbConnect(Postgres(),
            user = "funathon4",
            password = "postgre4",
            host = "postgresql-758156.projet-funathon",
            dbname = "defaultdb",
            port = 5432,
            check_interrupts = TRUE)
  return(conn)
}


#' Requête la table `parcelles` pour récupérer les parcelles qui se situent
#' dans un certain rayon autour d'un point repéré par une latitude et 
#' une longitude.
#' 
#' @param cnx Connexion à PostgreSQL.
#' @param lat Latitude.
#' @param lng Longitude.
#' @param radius Rayon.
#' @returns Objet `sf` avec les données des parcelles concernées.
#' 
query_db <- function(cnx, lat, lng, radius) {
  # Les données spatiales sur PostgreSQL sont stockées en Lambert 93.
  # Pour faire le join on veut donc projeter les coordonnées `lat`` et `lng`
  postgis_crs <- "EPSG:2154"
  coordinates <- data.frame(lng = c(lng), lat = c(lat)) %>%
    st_as_sf(coords = c("lng", "lat"), remove = TRUE) %>%
    st_set_crs("EPSG:4326") %>%
    st_transform(postgis_crs)
  
  #Requete PostgreSQL : création point, du buffer autour, de l'intersection avec le rpg
query <- sprintf(
    "SELECT * FROM rpg.parcelles WHERE ST_Intersects(geom, ST_Buffer(ST_SetSRID(ST_MakePoint(%f, %f), 2154), %.0f));",
    #Premier et 2ème arguments correspondent au %f pour float
    st_coordinates(coordinates)[1],
    st_coordinates(coordinates)[2],
    #correspond au %.0f
    radius*1000)

  #Envoie de la requête dans la bd  
  sf <- st_read(cnx, query = query)
  
  return(sf)
}


# Récupération des libellés des différentes cultures
lib_cult <- s3read_using(FUN = read_csv2,
                         object = "2023/sujet2/diffusion/ign/rpg/REF_CULTURES_GROUPES_CULTURES_2020.csv",
                         col_types = cols(.default = col_character()),
                         bucket = "projet-funathon",
                         opts = list("region" = "")) %>% clean_names()


lib_group_cult <- lib_cult %>% 
  select(code_groupe_culture, libelle_groupe_culture) %>% 
  distinct(code_groupe_culture, .keep_all=T)

lib_group_cult %>% kable()

#' Crée la table à afficher sur l'application grâce à des calculs sur les
#' données requêtées depuis PostgreSQL.
#' 
#' @param sf Données spatiales.
#' @returns data.frame à afficher.
compute_stats <- function(sf) {
 
   
  
  
  #ajout du libellé des cultures
  parc_prox_lib <- sf %>% 
    left_join(lib_cult %>% select(-code_groupe_culture),
              by = c("code_cultu" = "code_culture")) 
  
  #Composition des parcelles agricoles récupérées ####
  nb_parcelles_tot <- nrow(parc_prox_lib)
  surface_tot <- sum(parc_prox_lib$surf_parc)
  
  t1 <- parc_prox_lib %>%
    st_drop_geometry() %>%
    group_by(code_group, libelle_groupe_culture)%>%
    summarise(nb_parcelles = n(),
              pc_parcelles = round(nb_parcelles/nb_parcelles_tot*100,1),
              surf_ha = sum(surf_parc),
              pc_surf = round(surf_ha/surface_tot *100,1)
              
    )%>%
    adorn_totals()%>%
    mutate(taille_moyenne = round(surf_ha/nb_parcelles,1))
  
  #Mise en forme tableau
  t1  %>% 
    setNames(c("Code", "Groupe de cultures", "Nombre de parcelles", "(%)", "Surface (ha)", "Surface (%)", "Taille moyenne (ha)")) %>% 
    kable(
      format="html",
      caption="<span style='font-size:medium'>Groupes de cultures <strong>locales</strong> par surfaces décroissantes</span>",
      format.args = list(decimal.mark = ",", big.mark = " "),
      booktabs = TRUE) %>%
    kable_styling(font_size = 15) %>% 
    gsub("font-size: initial !important;",
         "font-size: 20pt !important;",.)%>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>% 
    row_spec(nrow(t1), bold = T, color = "white", background = "grey")
  
  return(t1)
}


# Création d'une palette de couleurs associée au groupe de culture
pal <- brewer.pal(12, "Paired")
pal <- colorRampPalette(pal)(24)
factpal <- colorFactor(pal, lib_group_cult$code_groupe_culture)

#' Rajoute les données d'un objet `sf` sous forme de polygones à une
#' carte `leaflet`.
#' 
#' @param leaflet_proxy Carte.
#' @param sf Données spatiales.
#' @returns Carte enrichie.
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
        data = sf,
        fillColor = ~factpal(code_group),
        weight = 2,
        opacity = 1,
        color = "#ffd966",
        dashArray = "3",
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 5,
          color = "#A40000",
          dashArray = "",
          fillOpacity = 0.0,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          encoding="UTF-8"))
  )
}

