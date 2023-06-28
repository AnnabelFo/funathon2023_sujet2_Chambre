#Cultures et prévisions climatiques

#Libraries et paramètres ####
library(RPostgres)
library(dplyr)
library(aws.s3)
library(ggplot2)
library(raster)
library(sf)
library(janitor)
library(knitr)
library(kableExtra)
#raster
library(raster)
#BD
#install.packages("DBI")
library(DBI)


# Pour avoir les noms de dates en français
invisible(Sys.setlocale("LC_ALL", "fr_FR.UTF-8"))

options(knitr.kable.NA = "")

#Connexion à la base de données
conn <- dbConnect(Postgres(),
                  user = "funathon4",
                  password = "postgre4",
                  host = "postgresql-758156.projet-funathon",
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

#vérifier la liste des tables
dbListTables(conn)

#Visualisations ####

#Chargement des données DRIAS
drias_raster <- s3read_using(
  function(f) readAll(brick(f)),
  object = "2023/sujet2/diffusion/resultats/drias.tif",
  bucket = "projet-funathon",
  opts = list("region" = ""))

drias_df <- as.data.frame(drias_raster, xy = TRUE) %>% tidyr::drop_na()
colnames(drias_df) <- c(
  "x",
  "y",
  "NORRRA",
  "NORSTM6",
  "NORSTM0",
  "NORSDA",
  "NORDATEVEG",
  "NORDATEDG",
  "NORDATEPG",
  "ARRA",
  "ASTM6",
  "ASTM0",
  "ASDA",
  "ADATEVEG",
  "ADATEDG",
  "ADATEPG",
  "ALTI"
)

#drias_df %>%  head(10) %>% kable()



# Visualisation d'ARRA
#variable ARRA, qui correspond à la huitième couche du raster
#et correspond à l’écart de cumul de précipitations d’avril à octobre en mm. 

#récupération de la bande 8
data_arra <- raster(drias_raster, layer= 8)

#Affichage dans une carte
  # Avec palette custom
  palette <- c("#1457ff", "#3c9aff", "#6dc4ff", "#a8e1ff", "#dff1fb", "#f8e9eb", "#fca8ab", "#f9575d", "#f2060b", "#a20102")
  breaks <- c(-200, -160, -120, -80, -40, -0, 40, 80, 120, 160, 200)
  #Création de la carte
  plot(data_arra,
       col = rev(palette),
       breaks = breaks,
       main = "Ecart de cumul de précipitations d'avril à octobre (mm)\nentre 2021-2050 et 1976-2005"
       )

#Requêtes PostgreSQL ####
  
  #Extraire la table drias.previsions
  requete <- "SELECT * FROM drias.previsions"
  table_drias_bd <- st_read(conn, query = requete)
  str(table_drias_bd)

  # Visualisation avec geom_sf
  fig <- ggplot()+
    geom_sf(data = table_drias_bd,
            aes(fill = arra),
            color = NA)+
    binned_scale(aesthetics = "fill", scale_name = "custom", 
                 palette = ggplot2:::binned_pal(scales::manual_pal(values = rev(palette)[-1])),
                 guide = "bins",
                 breaks = breaks)+
    ggtitle("Ecart de cumul de précipitations d'avril à octobre (mm)\nentre 2021-2050 et 1976-2005") +
    theme_minimal()

  fig  
  
# Appariement spatial entre données DRIAS et RPG ####
  
  #Requête pour récupérer pour chaque culture la surface des parcelles existantes 
  #dans chaque carreau de la grille DRIAS
  
  #Exploration de la bd
  #Obtenir les schémas de la bd
  query <- "SELECT schema_name FROM information_schema.schemata"
  dbGetQuery(conn, query)
  #Obtenir les tables au sein du schéma rpg
  query <- "SELECT table_name FROM information_schema.tables WHERE table_schema = 'rpg'"
  dbGetQuery(conn, query)
  #Obtenir la liste des champs de la table parcelles
  query <- "SELECT column_name FROM information_schema.columns WHERE table_name = 'parcelles'"
  liste_champs_rpg <- dbGetQuery(conn, query)
  
  requete <- "SELECT d.point, r.code_cultu, sum(r.surf_parc) surface, d.arra
  FROM drias.previsions d
  JOIN rpg.parcelles r
  ON ST_Intersects(r.geom, d.geometry)
  GROUP BY r.code_cultu, d.point, d.arra ;
   "
  table_drias_rpg <- dbGetQuery(conn, requete)
  table_drias_rpg %>% head(10) %>% kbl() %>% kable_minimal()
  
  #Calcul d'indicateurs par type de culture ####
  
  # Récupération des libellés des codes culture
  culture_mapping <- s3read_using(
    FUN = read.csv,
    sep = ";",
    object = "2023/sujet2/diffusion/ign/rpg/CULTURE.csv",
    bucket = "projet-funathon",
    opts = list("region" = "")
  )
  
  #ajout du libellé des cultures
 drias_rpg_lib <- table_drias_rpg %>% 
   left_join(culture_mapping, by = c("code_cultu" = "Code"))

 #calcul indicateurs
 # On aggrège au niveau national par code culture et on calcule un écart
 # moyen du cumul par m2
   tab_indic <- drias_rpg_lib%>%
   group_by(code_cultu, Libellé)%>%
   summarise(ecart_volume_prec = round(sum(surface*arra),0),
             surface_tot = round(sum(surface),0))%>%
   mutate(ecart_cumul_moy = round(ecart_volume_prec/surface_tot,0))

   tab_indic %>%
     arrange(ecart_cumul_moy) %>%
     head(10) %>% kbl() %>% kable_classic()
 
  #identifier les parcelles de maïs doux
  #Requête sur la table RPG pour récupérer les parcelles de mais doux
   requete <- "SELECT id_parcel, geom
   FROM rpg.parcelles
   WHERE code_cultu LIKE 'MID'"
   
   rpg_mais<- st_read(conn, query = requete)
   
   
   # Frontières régionales de métropole
   region_sf <- st_read(
     conn, query = "SELECT * FROM adminexpress.region"
   )
   region_sf <- region_sf %>% st_transform(
     "EPSG:2154"
   )
   region_sf <- region_sf %>%
     dplyr::filter(!(insee_reg %in% c("03", "04", "06", "01", "02", "01_SBSM")))
   
   # Visualisation avec geom_sf
   fig <- ggplot()+
     geom_sf(data = region_sf) +
     geom_sf(data = st_buffer(rpg_mais,4000),
             fill = "yellow",
             color = NA)+
     theme_minimal()+
     ggtitle("Répartition des parcelles de maïs doux")
fig   
    