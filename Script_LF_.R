---
title: "Script Léna"
output: pdf_document
date: "2023-06-27"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Chargement des library 

```{r}
library(RPostgres)
library(dplyr)
library(knitr)
library(sf)
library(janitor)
library(aws.s3)
```



# Connection à la base 

```{r}
conn <- dbConnect(Postgres(),
                  user = "funathon4",
                  password = "postgre4",
                  host = "postgresql-758156.projet-funathon",
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)
```

# Partie 2 
## 2.1 Récupération d'un point 


Création d'un object spatial grâce au packages sf












