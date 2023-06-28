#Chargement des library ####
#install.packages("RPostgres")
library(RPostgres)
install.packages("dplyr")
library(dplyr)
library(knitr)
library(sf)
library(janitor)
library(aws.s3)
library(sf)
library(tidyverse)
library(leaflet)
#install.packages("leaflet")
library(paletteer)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(kableExtra)
#install.packages("kableExtra")  
#install.packages("ggplot2")
library(ggplot2)
library(htmlwidgets)

#Connexion bd ####
conn <- dbConnect(Postgres(),
                  user = "funathon4",
                  password = "postgre4",
                  host = "postgresql-758156.projet-funathon",
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

#vérifier la liste des tables
dbListTables(conn)

#Manipulations####
#Création d'un point
#création des variables
lat <- 43.844809
lon <- 1.108055
rayon <- 1000
point <- data.frame(lat, lon, rayon)
point

point31 <- st_as_sf(point, coords = c("lon", "lat"), crs = "EPSG:4326")%>%
  st_transform("EPSG:2154")%>%
  mutate(coord_pt_gps = st_as_text(geometry))
      
write_sf(point31,
         tbl = 'table_point',
         conn)
#création d'une requette pour sélectionner les parcelles autour de notre point
requete <- "SELECT row_number() OVER () AS row_id,  p.coord_pt_gps, p.rayon, r.*  
    FROM public.point31 p, rpg.parcelles r 
    WHERE ST_DWithin(p.geometry, r.geom, p.rayon); "

parc_prox <- st_read(conn, query = requete)

#Visualisation leaflet ####
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

#Préparation de la carte 

# Transformation de la projection car leaflet ne connait que le WGS 84
parc_prox <- parc_prox %>% st_transform(4326)

#création d'un marqueur
marqueur <- point31%>% st_transform(4326)

#Création de la palette de couleurs associée au groupe de culture
palette_couleurs <- c("#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939", "#8ca252", "#b5cf6b", "#cedb9c",
                      "#8c6d31", "#bd9e39", "#e7ba52", "#e7cb94", "#843c39", "#ad494a", "#d6616b", "#e7969c",
                      "#7b4173", "#a55194", "#ce6dbd", "#de9ed6")

couleurs_cult <- colorFactor(palette_couleurs, parc_prox$code_group)

#ajout du libellé des cultures
parc_prox_lib <- parc_prox %>% 
  left_join(lib_cult %>% select(-code_groupe_culture), by = c("code_cultu" = "code_culture")) 

# Création d'un label ad hoc à afficher en surbrillance au passage de la souris sur la carte
labels <- sprintf("<strong>id_parcel : </strong>%s<br/>
                  <strong>Groupe culture : </strong>%s<br/>
                  <strong>Culture : </strong>%s<br/>
                  <strong>Surface (ha) : </strong>%s<br/>
                  <strong>Département : </strong>%s<br/>
                  <strong>Commune : </strong>%s<br/>",
                  parc_prox$id_parcel,
                  parc_prox_lib$libelle_groupe_culture,
                  parc_prox_lib$libelle_culture,
                  parc_prox$surf_parc,
                  parc_prox$insee_dep,
                  parc_prox$nom_com) %>% 
  lapply(htmltools::HTML)

#Création de la carte
carte_parcelles <- leaflet(parc_prox)%>%
  addTiles("http://wxs.ign.fr/essentiels/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}")%>%
  addPolygons(fillColor = ~couleurs_cult(code_group),
              weight = 1,
              opacity = 1,
              color = "#CE6A6B",
              dashArray = "2",
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#E3CD8B",
                dashArray = "",
                fillOpacity = 0.0,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto",
                encoding="UTF-8"))%>%
  addMarkers(data= marqueur)

carte_parcelles

# Pour sauvegarder la carte
 saveWidget(widget = carte_parcelles, file = "carte_parc_prox.html")



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
                     


#Comparaison avec la répartition des cultures au niveau départemental et national ####

# Couche département pour récupérer le département du point
dep <- s3read_using(
  FUN = sf::read_sf,
  layer = "departement",
  object = "2023/sujet2/diffusion/ign/adminexpress_cog_simpl_000_2023.gpkg",
  bucket = "projet-funathon",
  opts = list("region" = "")) %>% 
  st_transform(2154)

#Jointure pour récupérer le département
  #retransformer le str d parc_prox
parc_prox_lib <- parc_prox_lib%>% st_transform(2154)
  #jointure
  dep_point <- st_join(parc_prox_lib,dep, join = st_intersects)%>%
    st_drop_geometry()%>%
    select(insee_dep.x)%>%
    unique()

  # Récupération des statistiques départementales
  stat_dep_pt <- s3read_using(
    FUN = readr::read_rds,
    object = "2023/sujet2/diffusion/resultats/stat_group_cult_by_dep.rds",
    bucket = "projet-funathon",
    opts = list("region" = ""))
  
  # Récupération des statistiques nationales
  stat_fm <- s3read_using(
    FUN = readr::read_csv,
    object = "2023/sujet2/diffusion/resultats/stat_group_cult_fm.csv",
    col_types = cols(code_group = col_character()),
    bucket = "projet-funathon",
    opts = list("region" = "")) %>% 
    select(code_group, libelle_groupe_culture, pct_surf) %>% 
    rename(pct_surf_fm = pct_surf)
  
  
  
  # Calcul des % surfaces autour du point
  stat_pt <- parc_prox %>%
    st_drop_geometry() %>% 
    count(code_group, wt = surf_parc) %>%
    add_tally(n) %>% 
    mutate(pct_surf_local = round(100 * n / nn, 1)) %>%
    select(code_group, pct_surf_local) 
  
  # Récupération des statistiques du département concerné
  stat_dep_pt <- stat_dep_pt %>% 
    filter(insee_dep %in% dep_point) %>% 
    select(insee_dep, code_group, libelle_groupe_culture, pct_surf) %>% 
    rename(pct_surf_dep = pct_surf)
  
  # Appariement des statistiques locale, départementale et nationale
  stat_compar <- stat_fm %>% 
    left_join(stat_dep_pt %>% select(code_group, pct_surf_dep), by = "code_group") %>% 
    left_join(stat_pt , by = "code_group") %>% 
    select(libelle_groupe_culture, pct_surf_local, pct_surf_dep, pct_surf_fm) %>% 
    arrange(desc(pct_surf_local)) %>%
    adorn_totals() 
  
  stat_compar %>% 
    setNames(c("Groupe de cultures","Surf. locales (%)", "Surf. départ. (%)","Surf. France m. (%)")) %>%
    kable(
      format="html",
      caption="<span style='font-size:medium'>Comparaison des surfaces locales, départementales et nationales</span>",
      format.args = list(decimal.mark = ",", big.mark = " "),
      booktabs = TRUE) %>%
    kable_styling(font_size = 15) %>% 
    gsub("font-size: initial !important;",
         "font-size: 20pt !important;",.)%>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>% 
    row_spec(nrow(stat_compar), bold = T, color = "white", background = "grey")
  
  #Graphique de comparaison des cultures au niveau local, départemental et national
  
 stat_compar_tidy <- stat_compar %>%
   filter(libelle_groupe_culture != "Total") %>%
   slice_head(n=10) %>% 
   pivot_longer(cols =  c(pct_surf_local, pct_surf_dep, pct_surf_fm),
                names_to = "echelle",
                values_to = "pc_surface")
    stat_compar_tidy 
 
   fig <- ggplot(stat_compar_tidy,
                aes(x = libelle_groupe_culture, y = pc_surface))+
     geom_col(aes(fill = echelle),
              position = position_dodge())+
     coord_flip()
   
  fig
  