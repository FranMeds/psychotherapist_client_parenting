library(readxl)
library(writexl)
library(here)
library(dplyr)
library(tidyr)
library(irr)
library(stringr)

#---------------
# read spreadsheet

data <- read_excel(here("file_name.xlsx)"), 
                   sheet = "name_worksheet", 
                   skip = 1)

#---------------
# renomear primeiras colunas 

names(data)[4:5] <- c("id_categoriai", "categoria")

#---------------
# remover titulos das sessões

remove_lines <- c(
  "CIT",
  "CIP",
  "CIOP",
  "CEPP"
)

#---------------
# criar versões para relato e para emissões

data_relato <- data |> 
  filter(!(id_categoria) %in% remove_lines) |> 
  mutate(across(T1:T90, ~ str_remove_all(.x, "e")))

data_emissao <- data |> 
  filter(!(id_categoria) %in% remove_lines) |> 
  mutate(across(T1:T90, ~ str_remove_all(.x, "r")))

#---------------
# contar as ocorrências de emissões

data_emissao <- data_emissao |> 
  mutate(across(T1:T90, ~ str_count(as.character(.), "e")))

data_emissao[is.na(data_emissao)] <- 0

#---------------
# contar as ocorrências de relato

data_relato <- data_relato |> 
  mutate(across(T1:T90, ~ str_count(as.numeric(.), "r")))

data_relato[is.na(data_relato)] <- 0

#---------------
# filtrar apenas categorias com registros para emissões

data_emissao_tidy <- data_emissao |> 
  pivot_longer(                         # transformar em tidy
    cols = starts_with("T"),
    names_to = "tempo_sessao",
    values_to = "ocorrencias"
  ) |> 
  filter(!if_all(ocorrencias, ~ . == 0))

#---------------
# organização dos data de emissão

data_emissao_agrupado <- data_emissao_tidy  |>
  mutate(
    tempo_num = as.numeric(str_remove(tempo_sessao, "T")),
    intervalo = paste0(
      "T",
      5 * ((tempo_num - 1) %/% 5) + 1,
      "-T",
      5 * ((tempo_num - 1) %/% 5) + 5
    )
  )  |>
  group_by(Analise, 
           Sessao, 
           id_categoria, 
           categoria, 
           intervalo, 
           Genero
           )  |>
  summarise(
    ocorrencias = sum(ocorrencias),
    .groups = "drop"
  ) |>
  arrange(intervalo)

#---------------
# filtrar apenas categorias com registros para relato

data_relato_tidy <- data_relato |> 
  pivot_longer(                         # transformar em tidy
    cols = starts_with("T"),
    names_to = "tempo_sessao",
    values_to = "ocorrencias"
  ) |> 
  filter(!if_all(ocorrencias, ~ . == 0))

#---------------
# organização dos data de relato

data_relato_agrupado <- data_relato_tidy  |>
  mutate(
    tempo_num = as.numeric(str_remove(tempo_sessao, "T")),
    intervalo = paste0(
      "T",
      5 * ((tempo_num - 1) %/% 5) + 1,
      "-T",
      5 * ((tempo_num - 1) %/% 5) + 5
    )
  )  |>
  group_by(Analise, 
           Sessao, 
           id_categoria, 
           categoria, 
           intervalo, 
           Genero
           )  |>
  summarise(
    ocorrencias = sum(ocorrencias),
    .groups = "drop"
  ) |>
  arrange(intervalo)


#---------------
# salvar em planilha com abas para emissão e relato

lista_abas <- list(
  "Emissão" = data_emissao_agrupado,
  "Relato" = data_relato_agrupado
)

write_xlsx(lista_abas, 
           "path_to_save\\file_name.xlsx")






