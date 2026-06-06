# ETAPA 8 — Dispositivo + Favorabilidade
#
# Enriquece cjsg e cjpg com novas colunas (não cria dfs separados):
#   cjsg$dispositivo           → texto do dispositivo do acórdão
#   cjsg$papel                 → papel da A R de Araújo no processo
#   cjsg$resultado_dispositivo → procedente / improcedente / parcial / indefinido
#   cjsg$favorabilidade        → consumidor_venceu / empresa_venceu / parcial_consumidor / indefinido
#
#   cjpg$papel                 → idem
#   cjpg$resultado_dispositivo → classificado a partir de cjpg$julgado
#   cjpg$favorabilidade        → idem
#
# Filtro rápido após rodar:
#   cjsg |> filter(papel == "re", favorabilidade == "consumidor_venceu")
#   cjpg |> filter(papel == "re", favorabilidade != "indefinido")

library(tjsp)
library(tidyverse)  # já inclui stringr, dplyr, etc.

DIR_COMPILADO <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/compilados"
DIR_PARTES    <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/partes"
DIR_CPOSG     <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cposg_html"
DIR_ANALISE   <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/analise"

source("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/_helpers.R")

# ── Carrega bases ─────────────────────────────────────────────────────────────
cjsg        <- readRDS(file.path(DIR_COMPILADO, "cjsg.rds"))
cjpg        <- readRDS(file.path(DIR_COMPILADO, "cjpg.rds"))
partes_todas <- readRDS(file.path(DIR_PARTES,   "partes_todas.rds"))

# ── Função de favorabilidade ──────────────────────────────────────────────────
# Classificação do resultado agora feita pelas funções nativas do pacote tjsp:
#   tjsp_classificar_recurso(x)   → para acórdãos  (cjsg$dispositivo)
#   tjsp_classificar_sentenca(x)  → para sentenças  (cjpg$julgado)
# Ambas retornam: "procedente" / "improcedente" / "parcial" / NA

calcular_favorabilidade <- function(papel, resultado) {
  case_when(
    # ── Empresa como ré ────────────────────────────────────────────────────────
    papel == "re" & resultado == "procedente"   ~ "consumidor_venceu",
    papel == "re" & resultado == "parcial"      ~ "consumidor_venceu",
    papel == "re" & resultado == "improcedente" ~ "empresa_venceu",
    papel == "re" & resultado == "extinto"      ~ "consumidor_venceu",   # art. 924 — empresa pagou
    papel == "re" & resultado == "homologacao"  ~ "consumidor_venceu",   # acordo homologado

    # ── Empresa como autora ────────────────────────────────────────────────────
    papel == "autora" & resultado == "procedente"   ~ "empresa_venceu",
    papel == "autora" & resultado == "improcedente" ~ "consumidor_venceu",
    papel == "autora" & resultado == "parcial"      ~ "consumidor_venceu",
    papel == "autora" & resultado == "extinto"      ~ "indefinido",
    papel == "autora" & resultado == "homologacao"  ~ "indefinido",

    TRUE ~ "indefinido"

    # ── LÓGICA PENDENTE — 2ª INSTÂNCIA (aguardando validação) ─────────────────
    # tjsp_classificar_recurso retorna: provido / improvido / parcial /
    #   nao conhecido / prejudicado/extinto / embargos rejeitados / duvida
    #
    # A classificação depende de QUEM recorreu (requerente do recurso):
    #
    #   requerente = consumidor & provido    → consumidor_venceu
    #   requerente = consumidor & improvido  → empresa_venceu
    #   requerente = empresa    & provido    → empresa_venceu
    #   requerente = empresa    & improvido  → consumidor_venceu
    #
    # "nao conhecido" e "prejudicado/extinto" → verificar caso a caso
    # "embargos rejeitados" → decisão acessória, não classifica o mérito
    #
    # Implementar em script separado após validação manual.
  )
}

# ── Extrai papel por processo e instância (de partes_todas) ───────────────────
# Pega apenas as linhas da A R de Araújo e resolve conflitos mantendo
# o papel mais informativo (re > autora > outro)
nivel_papel <- c("re" = 1, "autora" = 2, "outro" = 3)

papel_processo <- partes_todas |>
  filter(str_detect(parte, EMPRESA_RGX)) |>
  mutate(nivel = nivel_papel[papel]) |>
  group_by(processo, instancia) |>
  slice_min(nivel, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(processo, instancia, papel)

# ── Extrai dispositivo dos acórdãos (2ª instância) ───────────────────────────
message("Lendo dispositivos dos acordaos (2a inst.) ...")
dispositivos <- tjsp_ler_dispositivo(diretorio = DIR_CPOSG)
message("  Dispositivos extraidos: ", nrow(dispositivos))

# ── Enriquece cjsg ────────────────────────────────────────────────────────────
message("Enriquecendo cjsg ...")

# Remove colunas que possam existir de execuções anteriores (evita .x/.y)
cjsg <- cjsg |>
  select(-any_of(c("dispositivo", "papel", "resultado_dispositivo", "favorabilidade")))

cjsg <- cjsg |>
  left_join(dispositivos, by = "processo") |>
  left_join(
    papel_processo |> filter(instancia == "2a") |> select(processo, papel),
    by = "processo"
  ) |>
  mutate(
    resultado_dispositivo = tjsp_classificar_recurso(dispositivo),
    favorabilidade        = calcular_favorabilidade(papel, resultado_dispositivo)
  )

# Verificação: valores retornados por tjsp_classificar_recurso
cat("\nValores distintos em resultado_dispositivo (cjsg):\n")
print(table(cjsg$resultado_dispositivo, useNA = "ifany"))

# ── Enriquece cjpg ────────────────────────────────────────────────────────────
message("Enriquecendo cjpg ...")

# Remove colunas que possam existir de execuções anteriores (evita .x/.y)
cjpg <- cjpg |>
  select(-any_of(c("papel", "resultado_dispositivo", "favorabilidade")))

cjpg <- cjpg |>
  left_join(
    papel_processo |> filter(instancia == "1a") |> select(processo, papel),
    by = "processo"
  ) |>
  mutate(
    resultado_dispositivo = tjsp_classificar_sentenca(julgado),
    favorabilidade        = calcular_favorabilidade(papel, resultado_dispositivo)
  )

# Verificação: valores retornados por tjsp_classificar_sentenca
cat("\nValores distintos em resultado_dispositivo (cjpg):\n")
print(table(cjpg$resultado_dispositivo, useNA = "ifany"))

# ── Resumo — 2ª INSTÂNCIA (acórdãos / cjsg) ──────────────────────────────────
cat("\n╔══════════════════════════════════════════════╗\n")
cat("║       2ª INSTÂNCIA — ACÓRDÃOS (cjsg)        ║\n")
cat("╚══════════════════════════════════════════════╝\n")

cat("\n--- Papel da A R de Araújo ---\n")
cjsg |>
  filter(!is.na(papel)) |>
  count(papel, sort = TRUE) |>
  print()

cat("\n--- Resultado por papel ---\n")
cjsg |>
  count(papel, resultado_dispositivo, sort = TRUE) |>
  print()

cat("\n--- Favorabilidade por papel ---\n")
cjsg |>
  count(papel, favorabilidade, sort = TRUE) |>
  print()

cat("\n--- Recorte: empresa como ré — taxa de vitória do consumidor ---\n")
cjsg |>
  filter(papel == "re", favorabilidade != "indefinido") |>
  summarise(
    n_total                = n(),
    consumidor_venceu      = sum(favorabilidade == "consumidor_venceu"),
    parcial                = sum(favorabilidade == "parcial_consumidor"),
    empresa_venceu         = sum(favorabilidade == "empresa_venceu"),
    pct_vitoria_consumidor = round(100 * (consumidor_venceu + parcial) / n_total, 1)
  ) |>
  print()

# ── Resumo — 1ª INSTÂNCIA (sentenças / cjpg) ─────────────────────────────────
cat("\n╔══════════════════════════════════════════════╗\n")
cat("║       1ª INSTÂNCIA — SENTENÇAS (cjpg)       ║\n")
cat("╚══════════════════════════════════════════════╝\n")

cat("\n--- Papel da A R de Araújo ---\n")
cjpg |>
  filter(!is.na(papel)) |>
  count(papel, sort = TRUE) |>
  print()

cat("\n--- Resultado por papel ---\n")
cjpg |>
  count(papel, resultado_dispositivo, sort = TRUE) |>
  print()

cat("\n--- Favorabilidade por papel ---\n")
cjpg |>
  count( favorabilidade, sort = TRUE) |>
  print()

cat("\n--- Recorte: empresa como ré — taxa de vitória do consumidor ---\n")
cjpg |>
  filter(papel == "re", favorabilidade != "indefinido") |>
  summarise(
    n_total                = n(),
    consumidor_venceu      = sum(favorabilidade == "consumidor_venceu"),
    parcial                = sum(favorabilidade == "parcial_consumidor"),
    empresa_venceu         = sum(favorabilidade == "empresa_venceu"),
    pct_vitoria_consumidor = round(100 * (consumidor_venceu + parcial) / n_total, 1)
  ) |>
  print()

# ── Salva cjsg e cjpg enriquecidos ───────────────────────────────────────────
saveRDS(cjsg, file.path(DIR_COMPILADO, "cjsg.rds"))
saveRDS(cjpg, file.path(DIR_COMPILADO, "cjpg.rds"))
write.csv(cjsg, file.path(DIR_COMPILADO, "cjsg.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cjpg, file.path(DIR_COMPILADO, "cjpg.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("\nEtapa 8 concluida. cjsg e cjpg enriquecidos em: ", DIR_COMPILADO)
message("Colunas novas em cjsg: dispositivo, papel, resultado_dispositivo, favorabilidade")
message("Colunas novas em cjpg: papel, resultado_dispositivo, favorabilidade")
