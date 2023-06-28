#Léna
library(shiny)

# Define UI
ui <- fluidPage(
  leafletOutput("map", height = 800),                                            #afficher la carte 
  numericInput(inputId="buffer_radius", label="Rayon (en km) :", value = 5),     #valeur à récupérer 
  tableOutput("table")                                                           #tableau de stat que l'on veut afficher j'imagine 
)

#inputId : id de l'input  
#label : label de l'input qui s'affiche pour expliquer la demande  mais non obligatoire 
#value : valeur initial 













