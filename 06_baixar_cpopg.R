# ETAPA 6 — Download dos detalhes dos processos de 1ª instância (cpopg)
# Baixa o HTML completo de cada processo já identificado em cjpg
# Necessário para extrair PARTES (sentenças já foram capturadas na etapa 02)
#
# ⚠️ PRÉ-REQUISITO: rodar 04_autenticar.R nesta mesma sessão R
# ⚠️ Pode demorar (~1s por processo) — 243 processos ≈ 4 min

library(tjsp)
library(tidyverse)

DIR_COMPILADO <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/compilados"
DIR_CPOPG     <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cpopg_html"

# Carrega processos únicos identificados em cjpg
cjpg <- readRDS(file.path(DIR_COMPILADO, "cjpg.rds"))

processos_1a <- cjpg |>
  distinct(processo) |>
  pull(processo)

message("Processos de 1ª instância a baixar: ", length(processos_1a))

tjsp_baixar_cpopg(
  processos = processos_1a,
  diretorio = DIR_CPOPG
)

message("Etapa 6 concluída. HTMLs em: ", DIR_CPOPG)
