#Fichier Interface Utilisateur UI
#Packages####
library(shiny)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Analyse du RPG"),
  leafletOutput("carte_parcelles"),
  numericInput(inputId = "rayon", label = "Quel est le rayon d'étude en m?",
               value = 500),
  dataTableOutput("table")
)
ui
