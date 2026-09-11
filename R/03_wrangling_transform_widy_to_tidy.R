library(readxl)
library(writexl)
library(here)
library(dplyr)
library(tidyr)
library(irr)
library(stringr)

#---------------
# read spreadsheet

data <- read_excel(here("file_name.xlsx)"))

#---------------
# rename the first col

names(data)[1:2] <- c("id_categorias", "categorias") # spreadsheet in pt-br

#---------------
# remove group titles and NAs

titles_remove <- c(                                     
  "Caracteristicas do/a terapeuta pré-intervenção (TA)",
  "Caracteristicas pré-sessão (TB)",
  "Manejo do fluxo da sessão (TC)",
  "Características da sessão (TD)",
  "Manejo de técnicas/habilidades terapeuticas em sessão (TE)",
  "Comportamento do/a terapeuta na interação com o/a cliente (TF)",
  "Recurso de ensino (TG)"
) # titles in pt-br

data <- data |> 
  filter(
    !is.na(categoria),
    !id_categoria  %in% titles_remove, # revision needed on the spreadsheet
    !categoria %in% titles_remove     # revision needed on the spreadsheet
    ) 

# -------------
# convert minutes (text to numbers)

minutes <- c(
  cinco = 5,
  dez = 10,
  quinze = 15,
  vinte = 20,
  vintecinco = 25,
  trinta = 30,
  trintacinco = 35,
  quarenta = 40,
  quarentacinco = 45,
  cinquenta = 50,
  cinquentacinco = 55,
  sessenta = 60,
  sessentacinco = 65,
  setenta = 70,
  setentacinco = 75,
  oitenta = 80,
  oitentacinco = 85,
  noventa = 90
) # numbers in pt-br

#------------
# convert to tidy

data_tidy <- data |> 
  pivot_longer(
    cols = -c(
      id_categoria, categoria
      ),
    names_to = c(
      "observador",
      "minuto",
      "registro"
    ),
    names_sep = "_",
    values_to = "valor"
  ) |> 
  mutate(
    minuto = minutes[minuto] |> as.numeric()
  ) |> 
  arrange(
    id_cetegoria,
    observador,
    minuto,
    registro
  )
  
#--------
# result

glimpse(data_tidy)

write_xlsx(data, "file_name.xlsx")











