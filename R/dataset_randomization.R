# Define uma semente para tornar a randomização reproduzível
set.seed(123)

# Sorteia 13 sessões distintas 
sessoes_sorteadas <- sample(
  1:13,
  size = 13,
  replace = FALSE
)

# Soreteia os terços das sessões para análise
tercos_sorteados <- sample(
  c("T1", "T2", "T3"),
  size = 13,
  replace = TRUE
)

# Nomes das sessões
sessoes_incluir_analise <- paste0(
  "Sessão",
  sessoes_sorteadas,
  "_",
  tercos_sorteados
)

# Seleciona três sessões (20% da amostra) para a concordância entre juízes
sessoes_concordancia <- sample(
  sessoes_incluir_analise,
  size = 3,
  replace = FALSE
)

# Cria um data frame para melhor visualizar o resultado da randomização
dados_amostra <- data.frame(
  Sessão = sessoes_incluir_analise,
  Fase = ifelse(
    sessoes_incluir_analise %in% sessoes_concordancia,
    "Banco análise/Concordância",
    "Banco análise"
  )
)

# Colaca em ordem crecente o data frame 
dados_amostra_oredenada <- dados_amostra |> 
  dplyr::arrange(
    as.numeric(
      stringr::str_extract(
        Sessão, "(?<=Sessão)\\d+"
        )
      )
    )

dados_amostra_oredenada
