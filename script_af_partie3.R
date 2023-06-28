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
#install.packages("RODBC")
library(RODBC)


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
                 breaks = breaks)

  fig  
  