#Fichier Interface Utilisateur UI
#Packages####
library(shiny)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Analyse du RPG"),
  leafletOutput("map"),
  numericInput(inputId = "buffer_radius", label = "Quel est le rayon d'étude en km?",
               value = 500),
  dataTableOutput("table")
)
ui

