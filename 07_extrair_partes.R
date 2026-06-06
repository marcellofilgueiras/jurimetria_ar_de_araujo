# ETAPA 7 — Extração das PARTES dos processos
# Lê os HTMLs baixados nas etapas 05 e 06 e produz tabela de partes
#
# Saída de tjsp_ler_partes:
#   - processo       : nº CNJ
#   - cd_processo    : código interno do TJSP
#   - tipo_parte     : Requerente, Requerido, Apelante, Apelado, Embargante, etc.
#   - parte          : nome da parte
#   - representante  : advogado/OAB
#
# Em seguida identificamos o PAPEL de "A R de Araújo Comunicações" em cada processo:
#   - autora  → quando aparece como Requerente / Apelante / Embargante
#   - ré      → quando aparece como Requerido / Apelado / Embargado

library(tjsp)
library(dplyr)
library(stringr)

DIR_CPOSG   <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cposg_html"
DIR_CPOPG   <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cpopg_html"
DIR_PARTES  <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/partes"

source("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/_helpers.R")

# ── Partes da 2ª instância ───────────────────────────────────────────────────
message("Lendo partes do cposg (2ª inst.) ...")
partes_2a <- tjsp_ler_partes(diretorio = DIR_CPOSG) |>
  mutate(instancia = "2a")
message("  Registros: ", nrow(partes_2a))

# ── Partes da 1ª instância ───────────────────────────────────────────────────
message("Lendo partes do cpopg (1ª inst.) ...")
partes_1a <- tjsp_ler_partes(diretorio = DIR_CPOPG) |>
  mutate(instancia = "1a")
message("  Registros: ", nrow(partes_1a))



# ── Identifica o papel da A R de Araújo em cada processo ─────────────────────

partes_todas <- bind_rows(partes_2a, partes_1a) |>
  mutate(
    papel = case_when(
      str_detect(tipo_parte, regex("requerente|reqte|apelante|embargante|embargte|exequente|exeqte|agravante|autora?", ignore_case = TRUE)) ~ "autora",
      str_detect(tipo_parte, regex("requerid[oa]|reqd[oa]|apelad[oa]|embargad[oa]|embargd[oa]|executad[oa]|exectd[oa]|agravad[oa]|r[eé]u?", ignore_case = TRUE)) ~ "re",
      TRUE ~ "outro"
    )
  )

message("\nPapel da A R de Araújo nos processos:")
partes_todas |>
  filter(str_detect(parte, EMPRESA_RGX)) |>
  count(papel, instancia, sort = TRUE) |>
  print()

# ── Salva ────────────────────────────────────────────────────────────────────
saveRDS(partes_todas, file.path(DIR_PARTES, "partes_todas.rds"))

write.csv(partes_todas, file.path(DIR_PARTES, "partes_todas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("Etapa 7 concluída. Arquivos em: ", DIR_PARTES)
