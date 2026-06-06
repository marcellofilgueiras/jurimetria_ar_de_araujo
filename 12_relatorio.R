# ETAPA 12 — Relatório Jurimétrico para Petição Inicial
#
# Objeto: A R de Araújo Comunicações ME (Guia Plus / Lista Regional Brasil)
# Parte autora: Sociedade Comercial FAM Ltda
# Finalidade: embasar pedido de declaração de inexigibilidade de duplicata
#             e indenização por danos morais (art. 186 CC, CDC)
#
# Fonte: TJSP — CJSG (acórdãos) e CJPG (sentenças) coletados via pacote {tjsp}

library(tidyverse)
library(scales)

DIR_COMPILADO <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/compilados"
DIR_ANALISE   <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/analise"

cjsg <- readRDS(file.path(DIR_COMPILADO, "cjsg.rds"))
cjpg <- readRDS(file.path(DIR_COMPILADO, "cjpg.rds"))

sep  <- paste0(rep("=", 60), collapse = "")
sep2 <- paste0(rep("-", 60), collapse = "")

cat("\n")
cat(sep, "\n")
cat("  RELATÓRIO JURIMÉTRICO\n")
cat("  A R DE ARAÚJO COMUNICAÇÕES ME\n")
cat("  Tribunal de Justiça do Estado de São Paulo\n")
cat(sep, "\n\n")

# ── 1. VOLUME DE PROCESSOS ────────────────────────────────────────────────────
cat(sep2, "\n")
cat("1. VOLUME DE PROCESSOS\n")
cat(sep2, "\n")

n_2a      <- n_distinct(cjsg$processo)
n_1a      <- n_distinct(cjpg$processo)
n_total   <- n_distinct(c(cjsg$processo, cjpg$processo))

cat(sprintf("  Acórdãos coletados (2ª instância) : %d processos\n", n_2a))
cat(sprintf("  Sentenças coletadas (1ª instância): %d processos\n", n_1a))
cat(sprintf("  Total de processos únicos          : %d processos\n\n", n_total))

# ── 2. DISTRIBUIÇÃO ANUAL ─────────────────────────────────────────────────────
cat(sep2, "\n")
cat("2. ANO DE DISTRIBUIÇÃO / JULGAMENTO\n")
cat(sep2, "\n")

cat("  2ª instância (ano do acórdão):\n")
cjsg |>
  mutate(ano = year(data_julgamento)) |>
  filter(!is.na(ano)) |>
  count(ano, sort = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  mutate(linha = sprintf("    %d : %3d acórdãos (%s%%)", ano, n, pct)) |>
  pull(linha) |> cat(sep = "\n")

cat("\n  1ª instância (ano da sentença):\n")
cjpg |>
  mutate(ano = year(disponibilizacao)) |>
  filter(!is.na(ano)) |>
  count(ano, sort = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  mutate(linha = sprintf("    %d : %3d sentenças (%s%%)", ano, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 3. LOCALIZAÇÃO (COMARCAS) ─────────────────────────────────────────────────
cat(sep2, "\n")
cat("3. DISTRIBUIÇÃO GEOGRÁFICA — TOP 15 COMARCAS\n")
cat(sep2, "\n")

cat("  2ª instância:\n")
cjsg |>
  count(comarca, sort = TRUE) |>
  head(15) |>
  mutate(pct = round(100 * n / sum(cjsg$comarca |> length()), 1)) |>
  mutate(linha = sprintf("    %-30s : %3d (%s%%)", comarca, n, pct)) |>
  pull(linha) |> cat(sep = "\n")

cat("\n  1ª instância:\n")
cjpg |>
  count(comarca, sort = TRUE) |>
  head(15) |>
  mutate(pct = round(100 * n / nrow(cjpg), 1)) |>
  mutate(linha = sprintf("    %-30s : %3d (%s%%)", comarca, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 4. TAXA DE PROCEDÊNCIA — 1ª INSTÂNCIA ────────────────────────────────────
cat(sep2, "\n")
cat("4. TAXA DE PROCEDÊNCIA — 1ª INSTÂNCIA\n")
cat(sep2, "\n")

res_1a <- cjpg |>
  filter(!is.na(resultado_dispositivo)) |>
  count(resultado_dispositivo, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

res_1a |>
  mutate(linha = sprintf("    %-20s : %3d (%s%%)",
                         resultado_dispositivo, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 5. PAPEL DA EMPRESA (consumidor como autor) ───────────────────────────────
cat(sep2, "\n")
cat("5. POLO DA AÇÃO — EMPRESA vs CONSUMIDOR\n")
cat(sep2, "\n")

papel_1a <- cjpg |> filter(!is.na(papel)) |> count(papel, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))
papel_2a <- cjsg |> filter(!is.na(papel)) |> count(papel, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

cat("  1ª instância:\n")
papel_1a |>
  mutate(linha = sprintf("    %-10s : %3d (%s%%)", papel, n, pct)) |>
  pull(linha) |> cat(sep = "\n")

cat("\n  2ª instância:\n")
papel_2a |>
  mutate(linha = sprintf("    %-10s : %3d (%s%%)", papel, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 6. TAXA DE VITÓRIA DO CONSUMIDOR — 1ª INSTÂNCIA ──────────────────────────
cat(sep2, "\n")
cat("6. TAXA DE VITÓRIA DO CONSUMIDOR — 1ª INSTÂNCIA\n")
cat("   (recorte: empresa como ré, resultado definido)\n")
cat(sep2, "\n")

fav_1a <- cjpg |>
  filter(papel == "re", favorabilidade != "indefinido") |>
  count(favorabilidade, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

fav_1a |>
  mutate(linha = sprintf("    %-25s : %3d (%s%%)", favorabilidade, n, pct)) |>
  pull(linha) |> cat(sep = "\n")

n_def <- sum(fav_1a$n)
n_cv  <- fav_1a |> filter(favorabilidade == "consumidor_venceu") |> pull(n) |> sum()
cat(sprintf("\n    Em %d de %d processos com resultado definido (%.1f%%),\n",
            n_cv, n_def, 100 * n_cv / n_def))
cat("    o consumidor obteve decisão favorável.\n\n")

# ── 7. VALORES DE INDENIZAÇÃO ─────────────────────────────────────────────────
cat(sep2, "\n")
cat("7. VALORES DE INDENIZAÇÃO POR DANOS MORAIS\n")
cat("   (extraídos das ementas de acórdãos — 2ª instância)\n")
cat(sep2, "\n")

valores <- cjsg |>
  mutate(
    valor_raw = str_extract(ementa,
                            "R\\$\\s?[\\d]{1,3}(?:\\.\\d{3})*(?:,\\d{2})?"),
    valor_num = valor_raw |>
      str_remove_all("R\\$\\s?") |>
      str_remove_all("\\.(?=\\d{3})") |>
      str_replace(",", ".") |>
      as.numeric()
  ) |>
  filter(!is.na(valor_num), valor_num >= 500, valor_num <= 100000)

if (nrow(valores) > 0) {
  cat(sprintf("  Processos com valor identificado : %d\n", nrow(valores)))
  cat(sprintf("  Valor mínimo                     : R$ %s\n",
              format(min(valores$valor_num),   big.mark = ".", decimal.mark = ",")))
  cat(sprintf("  Mediana                          : R$ %s\n",
              format(median(valores$valor_num), big.mark = ".", decimal.mark = ",")))
  cat(sprintf("  Média                            : R$ %s\n",
              format(round(mean(valores$valor_num)), big.mark = ".", decimal.mark = ",")))
  cat(sprintf("  Valor máximo                     : R$ %s\n\n",
              format(max(valores$valor_num),   big.mark = ".", decimal.mark = ",")))

  cat("  Faixas de valor:\n")
  valores |>
    mutate(faixa = case_when(
      valor_num <  2000  ~ "até R$ 2.000",
      valor_num <  5000  ~ "R$ 2.001 a R$ 5.000",
      valor_num < 10000  ~ "R$ 5.001 a R$ 10.000",
      valor_num < 20000  ~ "R$ 10.001 a R$ 20.000",
      TRUE               ~ "acima de R$ 20.000"
    )) |>
    count(faixa, sort = TRUE) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    mutate(linha = sprintf("    %-25s : %3d (%s%%)", faixa, n, pct)) |>
    pull(linha) |> cat(sep = "\n")
} else {
  cat("  Nenhum valor identificado nas ementas.\n")
}
cat("\n")

# ── 8. TEMAS NOS JULGADOS ─────────────────────────────────────────────────────
cat(sep2, "\n")
cat("8. TEMAS IDENTIFICADOS NOS JULGADOS\n")
cat(sep2, "\n")

temas <- tibble(
  tema = c("Protesto indevido", "Danos morais",
           "Duplicata", "Ausência de contrato",
           "Relação jurídica inexistente"),
  flag = c("tem_protesto", "tem_danos",
           "tem_duplicata", "tem_contrato",
           "tem_rel_juridica")
)

cat("  2ª instância (acórdãos):\n")
for (i in seq_len(nrow(temas))) {
  f <- temas$flag[i]
  if (f %in% names(cjsg)) {
    n <- sum(cjsg[[f]], na.rm = TRUE)
    cat(sprintf("    %-30s : %3d acórdãos (%s%%)\n",
                temas$tema[i], n, round(100 * n / nrow(cjsg), 1)))
  }
}

cat("\n  1ª instância (sentenças):\n")
for (i in seq_len(nrow(temas))) {
  f <- temas$flag[i]
  if (f %in% names(cjpg)) {
    n <- sum(cjpg[[f]], na.rm = TRUE)
    cat(sprintf("    %-30s : %3d sentenças (%s%%)\n",
                temas$tema[i], n, round(100 * n / nrow(cjpg), 1)))
  }
}
cat("\n")

cat(sep, "\n")
cat("  Relatório gerado em:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n")
cat("  Fonte: TJSP — ESAJ (dados públicos)\n")
cat("  Metodologia: coleta automatizada via pacote {tjsp} (R)\n")
cat(sep, "\n\n")

# ── Salva output em texto ─────────────────────────────────────────────────────
sink(file.path(DIR_ANALISE, "relatorio_jurimetrico.txt"))
source(sys.frame(1)$ofile %||% "12_relatorio.R")
sink()

message("Etapa 12 concluída. Relatório em: ", DIR_ANALISE)
