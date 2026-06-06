# ETAPA 5 — Download dos detalhes dos processos de 2ª instância (cposg)
# Baixa o HTML completo de cada processo já identificado em cjsg
# Necessário para extrair PARTES e DISPOSITIVO
#
# ⚠️ PRÉ-REQUISITO: rodar 04_autenticar.R nesta mesma sessão R
# ⚠️ Pode demorar (~1s por processo) — 271 processos ≈ 5 min

library(tjsp)
library(tidyverse)

DIR_CPOSG     <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cposg_html"

# Carrega processos únicos identificados em cjsg
cjsg <- readRDS(file.path(DIR_COMPILADO, "cjsg.rds"))

processos_2a <- cjsg |>
  distinct(processo) |>
  pull(processo)

message("Processos de 2ª instância a baixar: ", length(processos_2a))

tjsp_baixar_cposg(
  processos = processos_2a,
  diretorio = DIR_CPOSG
)

message("Etapa 5 concluída. HTMLs em: ", DIR_CPOSG)
