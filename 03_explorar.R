# ETAPA 3 — Análise exploratória (não escreve nada — só explora)
# Rodar após 02_compilar.R

library(tidyverse)

DIR_COMPILADO <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/compilados"
DIR_ANALISE <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/analise"
cjsg <- readRDS("dados/compilados/cjsg.rds")
cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── Volume geral ─────────────────────────────────────────────────────────────

#Acórdãos/decisões (2ª inst.)
nrow(cjsg)

# "Sentenças (1ª inst.)
nrow(cjpg)

# "Processos únicos 2ª inst."
n_distinct(cjsg$processo)

#Processos únicos 1ª inst
n_distinct(cjpg$processo)

# ── Por ano ───────────────────────────────────────────────────────────────────
# ACÓRDÃOS POR ANO 
cjsg |> mutate(ano = year(data_julgamento)) |> count(ano, sort = FALSE) 

# SENTENÇAS POR ANO
cjpg |> mutate(ano = year(disponibilizacao)) |> count(ano, sort = FALSE)

# ── Por comarca ───────────────────────────────────────────────────────────────
# TOP 15 COMARCAS (acórdãos) 
cjsg |> count(comarca, sort = TRUE) |> head(15)

# TOP 15 COMARCAS (sentenças)
cjpg |> count(comarca, sort = TRUE) |> head(15)

# ── Termos-chave nas ementas ──────────────────────────────────────────────────
# FREQUÊNCIA DE TERMOS-CHAVE NAS EMENTAS
termos <- c(
  "protesto indevido", "duplicata", "inexigibilidade",
  "lista telefônica", "publicidade", "danos morais",
  "inexistência de débito", "negativação", "serasa",
  "título sem causa", "contrato"
)
for (t in termos) {
  n <- sum(grepl(t, cjsg$ementa, ignore.case = TRUE), na.rm = TRUE)
  cat(sprintf("  %-30s : %d acórdãos\n", t, n))
}

for (t in termos) {
  n <- sum(grepl(t, cjpg$julgado, ignore.case = TRUE), na.rm = TRUE)
  cat(sprintf("  %-30s : %d julgados 1ª inst.\n", t, n))
}

# ── Flags temáticas (mutate) ──────────────────────────────────────────────────
# Colunas booleanas para filtrar julgados por tema
# TRUE = ementa/julgado menciona aquele tema
# Útil para selecionar precedentes mais similares ao caso FAM Ltda

cjsg <- cjsg |>
  mutate(
    tem_protesto  = grepl("protesto indevido|protesto irregular|protesto ilegal|apontamento indevido",
                          ementa, ignore.case = TRUE),
    tem_danos     = grepl("dano(s)? moral(is)?|indeniza",
                          ementa, ignore.case = TRUE),
    tem_duplicata = grepl("duplicata|título(s)? sem causa|título mercantil|duplicata simulada|duplicata fria",
                          ementa, ignore.case = TRUE),
    tem_contrato      = grepl("ausência de contrato|inexistência de contrato|contrato não celebrado|sem relação contratual",
                              ementa, ignore.case = TRUE),
    tem_rel_juridica  = grepl("relação jurídica inexistente|ausência de relação jurídica|inexistência de relação jurídica",
                              ementa, ignore.case = TRUE)
  )

# Flags temáticas (tem_protesto, tem_danos, etc.) agora vivem em 09_enriquecer.R

cat("\n=== FLAGS TEMÁTICAS — 2ª INSTÂNCIA (acórdãos) ===\n")
cjsg |>
  summarise(
    tem_protesto     = sum(tem_protesto,     na.rm = TRUE),
    tem_danos        = sum(tem_danos,        na.rm = TRUE),
    tem_duplicata    = sum(tem_duplicata,    na.rm = TRUE),
    tem_contrato     = sum(tem_contrato,     na.rm = TRUE),
    tem_rel_juridica = sum(tem_rel_juridica, na.rm = TRUE),
    total            = n()
  ) |>
  pivot_longer(-total, names_to = "flag", values_to = "n") |>
  mutate(pct = round(100 * n / total, 1)) 

#cat("\n=== FLAGS TEMÁTICAS — 1ª INSTÂNCIA (sentenças) ===\n")
cjpg |>
  summarise(
    tem_protesto     = sum(tem_protesto,     na.rm = TRUE),
    tem_danos        = sum(tem_danos,        na.rm = TRUE),
    tem_duplicata    = sum(tem_duplicata,    na.rm = TRUE),
    tem_contrato     = sum(tem_contrato,     na.rm = TRUE),
    tem_rel_juridica = sum(tem_rel_juridica, na.rm = TRUE),
    total            = n()
  ) |>
  pivot_longer(-total, names_to = "flag", values_to = "n") |>
  mutate(pct = round(100 * n / total, 1)) 

#cat("\n=== JULGADOS COM TODOS OS TEMAS (mais similares ao caso FAM) ===\n")
cjsg |>
  filter(tem_protesto & tem_danos & tem_duplicata) |>
#  select(processo, comarca, data_julgamento, ementa) |>
  print(n = 20)

cjpg |>
  filter(tem_protesto & tem_danos & tem_duplicata) |>
  #  select(processo, comarca, data_julgamento, ementa) |>
  print(n = 20)

## ── Velocidade de julgamento ──────────────────────────────────────────────────
#cat("\n=== VELOCIDADE DE JULGAMENTO — 1ª INSTÂNCIA ===\n")
# Dias entre distribuição e disponibilização da sentença
#velocidade_1a <- cjpg |>
 # filter(!is.na(data_distribuicao), !is.na(disponibilizacao)) |>
 # mutate(
  #  dias = as.numeric(difftime(disponibilizacao, data_distribuicao, units = "days"))
  #) |>
  #filter(dias >= 0, dias < 3650)   # exclui outliers > 10 anos (prováveis erros de data)

#velocidade_1a |>
 # summarise(
  #  n             = n(),
   # media_dias    = round(mean(dias),              1),
    #mediana_dias  = round(median(dias),            1),
    #p25_dias      = round(quantile(dias, 0.25),    1),
    #p75_dias      = round(quantile(dias, 0.75),    1),
    #max_dias      = max(dias)
  #) |>
  #mutate(
   # media_meses   = round(media_dias   / 30, 1),
  #  mediana_meses = round(mediana_dias / 30, 1)
  #) |>
#  print()

#  === VELOCIDADE DE JULGAMENTO — 2ª INSTÂNCIA (por ano) ===\n")
# Na 2ª instância não temos data de distribuição do recurso,
# então mostramos a distribuição dos julgamentos por ano
cjsg |>
  mutate(ano = year(data_julgamento)) |>
  count(ano, sort = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ── Valores de indenização ────────────────────────────────────────────────────
#=== VALORES DE INDENIZAÇÃO — 2ª INSTÂNCIA ===\n")

# Extrai valores no padrão "R$ X.XXX,XX" das ementas
valores_2a <- cjsg |>
  mutate(
    valor_raw = str_extract(
      ementa,
      "R\\$\\s?[\\d]{1,3}(?:\\.\\d{3})*(?:,\\d{2})?"
    ),
    valor_num = valor_raw |>
      str_remove_all("R\\$\\s?")           |>
      str_remove_all("\\.(?=\\d{3})")      |>   # remove ponto de milhar
      str_replace(",", ".")                |>   # troca vírgula decimal por ponto
      as.numeric()
  ) |>
  filter(!is.na(valor_num), valor_num >= 500, valor_num <= 100000)

cat("Processos com valor extraído:", nrow(valores_2a), "\n\n")

valores_2a |>
  summarise(
    n       = n(),
    minimo  = min(valor_num),
    p25     = quantile(valor_num, 0.25),
    mediana = median(valor_num),
    media   = round(mean(valor_num), 2),
    p75     = quantile(valor_num, 0.75),
    maximo  = max(valor_num)
  ) |>
  print()

cat("\n=== FAIXAS DE VALOR MAIS COMUNS ===\n")
valores_2a |>
  mutate(
    faixa = case_when(
      valor_num <  2000  ~ "até R$ 2.000",
      valor_num <  5000  ~ "R$ 2.001 a R$ 5.000",
      valor_num < 10000  ~ "R$ 5.001 a R$ 10.000",
      valor_num < 20000  ~ "R$ 10.001 a R$ 20.000",
      TRUE               ~ "acima de R$ 20.000"
    )
  ) |>
  count(faixa, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ── Salvar ────────────────────────────────────────────────────────────────────
# IMPORTANTE: este script é só exploratório, NÃO sobrescreve cjsg/cjpg.
# O enriquecimento (flags, papel, favorabilidade, valor, etc.) é feito por
# 09 → 10_11 → 14, que são os únicos a salvar os .rds principais.
# (cjsg.csv/cjpg.csv também não são reescritos aqui — saem do 14)

resumo <- list(
  total_acordaos      = nrow(cjsg),
  total_sentencas     = nrow(cjpg),
  processos_unicos    = n_distinct(c(cjsg$processo, cjpg$processo)),
  por_ano_cjsg        = cjsg |> mutate(ano = year(data_julgamento)) |> count(ano),
  por_comarca_cjsg    = cjsg |> count(comarca, sort = TRUE) |> head(20),
  por_ano_cjpg        = cjpg |> mutate(ano = year(disponibilizacao)) |> count(ano),
  por_comarca_cjpg    = cjpg |> count(comarca, sort = TRUE) |> head(20),
  flags_cjsg          = if (any(str_starts(names(cjsg), "tem_"))) {
                          cjsg |> summarise(across(starts_with("tem_"), \(x) sum(x, na.rm = TRUE)))
                        } else NULL,
  flags_cjpg          = if (any(str_starts(names(cjpg), "tem_"))) {
                          cjpg |> summarise(across(starts_with("tem_"), \(x) sum(x, na.rm = TRUE)))
                        } else NULL,
#  velocidade_1a       = velocidade_1a |> summarise(mediana_dias = median(dias), media_dias = mean(dias)),
  valores_indenizacao = valores_2a    |> summarise(mediana = median(valor_num), media = mean(valor_num), n = n())
)


saveRDS(resumo, file.path(DIR_ANALISE, "resumo_analise.rds"))
message("Etapa 3 concluída (exploratória — nada salvo).")
