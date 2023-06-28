#Préalable####
install.packages("paws", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "RYIWRSQRC9FI1KZMY7S4",
           "AWS_SECRET_ACCESS_KEY" = "hUFHYz1YadDfuZrH3iymcGEA4f5BnVu0UiTXZACT",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJSWUlXUlNRUkM5RkkxS1pNWTdTNCIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNjg3OTU4MDE0LCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6ImFubmFiZWwuZm91cmNhZGVAb2NjaXRhbmllLmNoYW1iYWdyaS5mciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJleHAiOjE2ODgwNDQ0MTUsImZhbWlseV9uYW1lIjoiRm91cmNhZGUiLCJnaXZlbl9uYW1lIjoiQW5uYWJlbCIsImdyb3VwcyI6W10sImlhdCI6MTY4Nzk1ODAxNSwiaXNzIjoiaHR0cHM6Ly9hdXRoLmxhYi5zc3BjbG91ZC5mci9hdXRoL3JlYWxtcy9zc3BjbG91ZCIsImp0aSI6IjdiNmEwZjBiLWY3YzYtNDc3ZC1iZThjLWNmMWU3NGM3YWQ4OSIsIm5hbWUiOiJBbm5hYmVsIEZvdXJjYWRlIiwibm9uY2UiOiJiY2EyNDljYy1kYTg4LTRjNWQtOTNjMi00NmExYmEwZjg3OGUiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoiYW5uYWJlbGZvIiwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBncm91cHMgZW1haWwiLCJzZXNzaW9uX3N0YXRlIjoiOWY3OGVhMGItOGJkYy00MzVkLTkzZDgtYTA3NjI5MmU3MTc3Iiwic2lkIjoiOWY3OGVhMGItOGJkYy00MzVkLTkzZDgtYTA3NjI5MmU3MTc3Iiwic3ViIjoiODQ3MWQzNDItM2MwYi00OGMwLWI1MzItNzIyYzEyZDVmZjljIiwidHlwIjoiQmVhcmVyIn0.PgwEby2iHKakiQEMvOGTeRVcGrV1Q489AIf_UhunpuEgnLONiagGIYCXX-z1kxxkbr7yu-1ybu0iYqXiFZbiCg",
           "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr")

library("paws")
minio <- paws::s3(config = list(
  credentials = list(
    creds = list(
      access_key_id = "RYIWRSQRC9FI1KZMY7S4",
      secret_access_key = "hUFHYz1YadDfuZrH3iymcGEA4f5BnVu0UiTXZACT",
      session_token = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJSWUlXUlNRUkM5RkkxS1pNWTdTNCIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNjg3OTU4MDE0LCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6ImFubmFiZWwuZm91cmNhZGVAb2NjaXRhbmllLmNoYW1iYWdyaS5mciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJleHAiOjE2ODgwNDQ0MTUsImZhbWlseV9uYW1lIjoiRm91cmNhZGUiLCJnaXZlbl9uYW1lIjoiQW5uYWJlbCIsImdyb3VwcyI6W10sImlhdCI6MTY4Nzk1ODAxNSwiaXNzIjoiaHR0cHM6Ly9hdXRoLmxhYi5zc3BjbG91ZC5mci9hdXRoL3JlYWxtcy9zc3BjbG91ZCIsImp0aSI6IjdiNmEwZjBiLWY3YzYtNDc3ZC1iZThjLWNmMWU3NGM3YWQ4OSIsIm5hbWUiOiJBbm5hYmVsIEZvdXJjYWRlIiwibm9uY2UiOiJiY2EyNDljYy1kYTg4LTRjNWQtOTNjMi00NmExYmEwZjg3OGUiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoiYW5uYWJlbGZvIiwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBncm91cHMgZW1haWwiLCJzZXNzaW9uX3N0YXRlIjoiOWY3OGVhMGItOGJkYy00MzVkLTkzZDgtYTA3NjI5MmU3MTc3Iiwic2lkIjoiOWY3OGVhMGItOGJkYy00MzVkLTkzZDgtYTA3NjI5MmU3MTc3Iiwic3ViIjoiODQ3MWQzNDItM2MwYi00OGMwLWI1MzItNzIyYzEyZDVmZjljIiwidHlwIjoiQmVhcmVyIn0.PgwEby2iHKakiQEMvOGTeRVcGrV1Q489AIf_UhunpuEgnLONiagGIYCXX-z1kxxkbr7yu-1ybu0iYqXiFZbiCg"
      )),
  endpoint = paste0("https://", "minio.lab.sspcloud.fr"),
  region = "us-east-1"))

minio$list_buckets()

#mc cp minio/projet-funathon/2023/sujet2/diffusion/era5.zip ~/work/funathon_sujet2_Chambre/data/era5.zip
#unzip ~/work/funathon_sujet2_Chambre/data/era5.zip -d ~/work/funathon_sujet2_Chambre/data/
#  rm ~/work/funathon_sujet2_Chambre/data/era5.zip

# Début du projet 4 Evolution de la date théorique de récolte #### 

#Libraries et configuration ####
#install.packages("stars")
library(stars)     # manipulation de rasters
library(terra)     # manipulation de rasters
library(tidyverse) # manipulation des données
library(glue)      # interpolation de chaine de caractères
library(fs)        # gestion du système de fichier
library(RPostgres) # connexion PostgreSQL
library(sf)        # manipulation de données spatiales vecteur
library(leaflet)   # carto web
library(gt)        # mise en forme tableaux
library(gtsummary) # tableaux de modèles stat
library(ggrepel)   # étiquettage graphiques
# D'autres packages sont à installer et seront appelés directement avec leur
# namespace : btb, raster, fasterize, rlang, memoise

# localisation des données dans le stockage "local"
rep_era5 <- "data/era5"

# pour avoir les noms de dates en français
invisible(Sys.setlocale("LC_ALL", "fr_FR.UTF-8"))

options(OutDec = ",",
        scipen = 999)

# Connexion à la base PostgreSQL
conn <- dbConnect(Postgres(),
                  user = "funathon4",
                  password = "postgre4",
                  host = "postgresql-758156.projet-funathon",
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

#Définition des bassins de production du maïs ####

#Vérif des chiffres maïs
requete <- "SELECT code_cultu, sum(surf_parc) surface
  FROM rpg.parcelles
  GROUP BY code_cultu ;
   "
rpg_par_cult <- dbGetQuery(conn, requete) 

rpg_calc <- rpg_par_cult %>%
  mutate(surf_tot = sum(surface))%>%
  filter(code_cultu == 'MIS' |
           code_cultu == 'MIE'|
           code_cultu == 'MID')%>%
  mutate(surf_mais = sum(surface))%>%
  group_by(code_cultu)%>%
  summarise(surf_ha = sum(surface),
            pc_sur_tot = surface/surf_tot*100,
            pc_sur_mais = surface/surf_mais*100
            )
rpg_calc %>%kbl() %>% kable_minimal()

#récupération des données
# contour de la métropole (pour limiter le lissage à l'intérieur des frontières)
fr <- read_sf(conn, query = "
  SELECT 
    st_union(st_transform(geom, 3035)) as geom
  FROM adminexpress.region
  WHERE insee_reg > '06'")

# communes (pour donner ultérieurement un nom à nos bassins de production)
com <- read_sf(conn, query = "
  SELECT 
    nom,
    insee_dep,
    population,
    st_transform(geom, 3035) as geom
  FROM adminexpress.commune
  WHERE insee_reg > '06'")

# un point par parcelle de maïs avec sa surface qui servira de poids au lissage
mais <- read_sf(conn, query = "
  SELECT 
    st_transform(st_pointonsurface(geom), 3035) as geom,
    surf_parc
  FROM rpg.parcelles
  WHERE code_cultu = 'MIS'")

#Lissage ####
  #Paramètres
  seuil_lissage  <- 8     # seuil de densité à prendre en compte (ha/km²)
  nb_bassins     <- 10    # combien de bassins on conserve
  bande_passante <- 10000 # "étalement" du lissage en m
  pixel          <- 1000  # taille du pixel en sortie en m
  
 
  # Lissage : fonctions simplifiant la création de rasters lissés à partir de 
  # points avec {btb}
  #
  # michael.delorme - 2021-08-26
  
  # utils -------------------------------------------------------------------
  
  #' rounding
  #' from plyr
  #'
  #' @param x
  #' @param accuracy
  #' @param f
  #'
  #' @return
  round_any <- function(x, accuracy, f = round) {
    
    f(x / accuracy) * accuracy
  }
  
  #' Generate a grid of coordinates from a spatial layer
  #'
  #' Memoised to get a faster result when used multiple times on the same extent
  #'
  #' @param zone sf object (polygons) : spatial extent
  #' @param margin number : buffer of bounding box
  #' @param resolution number : distance between nodes
  #'
  #' @return dataframe of coordinates (x, y)
  generate_grid <- memoise::memoise(function(zone, margin, resolution) {
    
    zone_bbox <- sf::st_bbox(zone)
    
    zone %>%
      sf::st_make_grid(cellsize = resolution,
                       offset = c(round_any(zone_bbox[1] - margin, resolution, floor),
                                  round_any(zone_bbox[2] - margin, resolution, floor)),
                       what = "centers") %>%
      sf::st_sf() %>%
      sf::st_join(zone, join = st_intersects, left = FALSE) %>%
      sf::st_coordinates() %>%
      tibble::as_tibble() %>%
      dplyr::select(x = X, y = Y)
  })
  
  
  # main function -----------------------------------------------------------
  
  #' Kernel weighted smoothing with arbitrary bounding area
  #'
  #' @param df sf object (points) : features to smooth
  #' @param field expression : weight field in df (unquoted) ; the values must not have NAs
  #' @param bandwidth numeric : kernel bandwidth (output map units)
  #' @param resolution numeric : output grid resolution (output map units)
  #' @param zone sf objet (polygons) : study zone boundary. If null will use df extent
  #' @param out_crs integer : EPSG code projection for output raster (should be an equal-area projection)
  #' @param ... other arguments passed to btb::kernelSmoothing
  #'
  #' @return a raster object
  #' @export
  #' @import btb, raster, fasterize, dplyr, sf, rlang, memoise
  lissage <- function(df, field, bandwidth, resolution, zone = NULL, out_crs = 3035, ...) {
    
    field_name <- rlang::as_name(rlang::enquo(field))
    
    if (!"sf" %in% class(df)
        | sf::st_geometry_type(df, FALSE) != "POINT") {
      stop("« df » should be a point sf object.")
    }
    
    if (!is.numeric(bandwidth)) stop("bandwidth sould be numeric.")
    if (!is.numeric(resolution)) stop("resolution sould be numeric.")
    
    nb_na <- sum(is.na(dplyr::pull(df, {{field}})))
    if (nb_na > 0) {
      warning(paste("removing", nb_na, "NA",
                    paste0("value", ifelse(nb_na > 1, "s", "")),
                    "in «", field_name, "»..."))
      df <- tidyr::drop_na(df, {{field}}) %>%
        sf::st_as_sf()
    }
    
    # check projections
    if (is.na(sf::st_crs(df))) {
      stop("missing projection in sf object « df ».")
    }
    
    if (sf::st_crs(df)$epsg != out_crs) {
      message("reprojecting data...")
      df <- sf::st_transform(df, out_crs)
    }
    
    if (!is.null(zone)) {
      if (!"sf" %in% class(zone)
          |!sf::st_geometry_type(zone, FALSE) %in% c("POLYGON", "MULTIPOLYGON")) {
        stop("« zone » should be a polygon/multiploygon sf object.")
      }
      
      # check projections
      if (is.na(sf::st_crs(zone))) {
        stop("missing projection in sf object « zone ».")
      }
      
      if (sf::st_crs(zone)$epsg != out_crs) {
        message("reprojecting study zone...")
        zone <- sf::st_transform(zone, out_crs)
      }
      
      # grid generation
      if (memoise::has_cache(generate_grid)(zone, bandwidth, resolution)) {
        message("retrieving reference grid from cache...")
      } else {
        message("generating reference grid...")
      }
      
      zone_xy <- generate_grid(zone, bandwidth, resolution)
      zone_bbox <- sf::st_bbox(zone)
      
    } else {
      message("using default reference grid...")
      
      zone_xy <- NULL
      zone_bbox <- sf::st_bbox(df)
    }
    
    # kernel
    message(paste0("computing kernel on « ", field_name, " »..."))
    kernel <- df %>%
      bind_cols(., sf::st_coordinates(.) %>% # si pas de données renvoie vecteur non nommé
                  as.data.frame() %>%     # donc on le modifie
                  set_names(c("x", "y"))) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(x, y, {{ field }}) %>%
      btb::btb_smooth(sEPSG = out_crs,
                      iCellSize = resolution,
                      iBandwidth = bandwidth,
                      dfCentroids = zone_xy, ...)
    
    # rasterization
    message("\nrasterizing...")
    raster::raster(xmn = round_any(zone_bbox[1] - bandwidth, resolution, floor),
                   ymn = round_any(zone_bbox[2] - bandwidth, resolution, floor),
                   xmx = round_any(zone_bbox[3] + bandwidth, resolution, ceiling),
                   ymx = round_any(zone_bbox[4] + bandwidth, resolution, ceiling),
                   resolution = resolution,
                   crs = sf::st_crs(out_crs)$input
    ) %>%
      fasterize::fasterize(kernel, ., field = field_name)
  }
  
  #application de la fonction de lissage
  mais_liss <- mais %>% 
    lissage(surf_parc, bandwidth = bande_passante, resolution = pixel, zone = fr) %>% 
    rast() 

  #affichage de la carte
  plot(mais_liss, main = "Densité de culture de maïs grain 2021 (ha/km²)")

  #affichage des zones supérieures à notre seuil (ici 8km/ha)
  plot(mais_liss > seuil_lissage, main = glue("Zones de culture de maïs grain 2021 (> {seuil_lissage} ha/km²)"))
  
  # vectorisation et conservation des nb_bassins + grandes zones
  cluster_liss <- (mais_liss > seuil_lissage) %>% 
    as.polygons() %>% 
    st_as_sf() %>% 
    filter(layer == 1) %>% 
    st_cast("POLYGON") %>% 
    mutate(surf = st_area(geometry)) %>% 
    slice_max(surf, n = nb_bassins) %>% 
    mutate(id = row_number())
  
  # nommage des clusters avec le nom de la plus grosse ville de la zone
  noms <- cluster_liss %>% 
    st_join(st_point_on_surface(com), left = TRUE) %>% 
    st_drop_geometry() %>% 
    group_by(id) %>% 
    slice_max(population, n = 1, with_ties = FALSE) %>% 
    select(id, nom, insee_dep)
  
  #Affichage avec leaflet
  cluster_liss %>% 
    inner_join(noms, by = "id") %>% 
    st_transform("EPSG:4326") %>% 
    leaflet() %>% 
    addPolygons(popup = ~ glue("{id}. bassin de {nom}")) %>% 
    addTiles()
  
  