# ETAPA 2 — Leitura dos HTMLs e compilação em tabelas estruturadas
# Rodar após a etapa 01

library(tjsp)
library(tidyverse)

DIR_CJSG      <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cjsg_html"
DIR_CJPG      <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cjpg_html"
DIR_COMPILADO <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/compilados"

# ── 2ª instância ─────────────────────────────────────────────────────────────
message("Lendo acórdãos/decisões (cjsg) ...")

cjsg <- tjsp_ler_cjsg(diretorio = DIR_CJSG)

message("  Registros lidos: ", nrow(cjsg))
message("  Processos únicos: ", n_distinct(cjsg$processo))

saveRDS(cjsg, file.path(DIR_COMPILADO, "cjsg.rds"))
write.csv(cjsg, file.path(DIR_COMPILADO, "cjsg.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ── 1ª instância ─────────────────────────────────────────────────────────────
message("Lendo sentenças (cjpg) ...")

cjpg <- tjsp_ler_cjpg(diretorio = DIR_CJPG)

message("  Registros lidos: ", nrow(cjpg))
message("  Processos únicos: ", n_distinct(cjpg$processo))

saveRDS(cjpg, file.path(DIR_COMPILADO, "cjpg.rds"))
write.csv(cjpg, file.path(DIR_COMPILADO, "cjpg.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ── Base unificada ────────────────────────────────────────────────────────────
todos <- bind_rows(
  cjsg |> select(processo) |> mutate(instancia = "2a"),
  cjpg |> select(processo) |> mutate(instancia = "1a")
) |>
  distinct(processo, .keep_all = TRUE)

message("Total de processos únicos (1ª + 2ª): ", nrow(todos))

saveRDS(todos, file.path(DIR_COMPILADO, "processos_unicos.rds"))
write.csv(todos, file.path(DIR_COMPILADO, "processos_unicos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("Etapa 2 concluída. Arquivos em: ", DIR_COMPILADO)

