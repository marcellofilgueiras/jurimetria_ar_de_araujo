# ETAPA 13 — Relatório Jurimétrico para Petição Inicial (+ word clouds)
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

# Bases analíticas: só sentenças canônicas (descarta duplicatas)
cjpg_can <- cjpg |> filter(decisao_canonica)

# Salva relatório em .txt + também imprime no console (split = TRUE)
.relat_file <- file(file.path(DIR_ANALISE, "relatorio_jurimetrico.txt"), open = "w")
sink(.relat_file, split = TRUE)
on.exit({ try(sink(), silent = TRUE); try(close(.relat_file), silent = TRUE) }, add = TRUE)

# Helpers de formatação
fmt_brl <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

sep  <- paste0(rep("=", 70), collapse = "")
sep2 <- paste0(rep("-", 70), collapse = "")

cat("\n")
cat(sep, "\n")
cat("  RELATÓRIO JURIMÉTRICO\n")
cat("  A R DE ARAÚJO COMUNICAÇÕES ME (GUIA PLUS / LISTA REGIONAL BRASIL)\n")
cat("  Tribunal de Justiça do Estado de São Paulo — 1ª instância\n")
cat(sep, "\n")
cat("  Gerado em:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n")
cat(sep, "\n\n")

# ── 0. SUMÁRIO EXECUTIVO ────────────────────────────────────────────────────
cat(sep2, "\n")
cat("0. SUMÁRIO EXECUTIVO\n")
cat(sep2, "\n")

n_total_pg     <- nrow(cjpg_can)
n_re           <- sum(cjpg_can$papel == "re", na.rm = TRUE)
n_merito       <- sum(cjpg_can$tipo_decisao_cpc == "merito_487_I", na.rm = TRUE)

merito_re <- cjpg_can |>
  filter(papel == "re",
         tipo_decisao_cpc %in% c("merito_487_I", "homologacao_487_III"),
         resultado_dispositivo %in% c("procedente","parcial","improcedente","homologacao"))

n_pro     <- sum(merito_re$resultado_dispositivo %in% c("procedente","parcial","homologacao"))
n_contra  <- sum(merito_re$resultado_dispositivo == "improcedente")
pct_pro   <- 100 * n_pro / nrow(merito_re)

med_val   <- median(cjpg_can$valor_indenizacao, na.rm = TRUE)
mean_val  <- mean(cjpg_can$valor_indenizacao,   na.rm = TRUE)

med_tempo <- median(cjpg_can$tempo_tramitacao_dias, na.rm = TRUE)

n_tutela  <- sum(cjpg_can$tem_tutela_deferida, na.rm = TRUE)
pct_tut   <- 100 * n_tutela / n_total_pg

cat(sprintf("  • Base: %d sentenças únicas de 1ª instância (2017–2026)\n", n_total_pg))
cat(sprintf("  • Em %d sentenças, a empresa figura como RÉ (%.1f%%)\n",
            n_re, 100 * n_re / n_total_pg))
cat(sprintf("  • Em %d de %d decisões de mérito, a solução foi PRÓ-CONSUMIDOR (%.1f%%)\n",
            n_pro, nrow(merito_re), pct_pro))
cat(sprintf("  • Indenização: mediana R$ %s (média R$ %s)\n",
            fmt_brl(med_val), fmt_brl(mean_val)))
cat(sprintf("  • Tempo entre ajuizamento e sentença: mediana %d dias (~%.1f meses)\n",
            med_tempo, med_tempo / 30))
cat(sprintf("  • Tutela de urgência deferida: %d sentenças (%.1f%%)\n\n",
            n_tutela, pct_tut))

# ── 1. VOLUME DE PROCESSOS ────────────────────────────────────────────────────
cat(sep2, "\n")
cat("1. VOLUME DE PROCESSOS\n")
cat(sep2, "\n")

n_2a    <- n_distinct(cjsg$processo)
n_1a    <- n_distinct(cjpg$processo)
n_total <- n_distinct(c(cjsg$processo, cjpg$processo))

cat(sprintf("  Sentenças 1ª inst. coletadas       : %d (todas as linhas)\n",   nrow(cjpg)))
cat(sprintf("  Sentenças canônicas (sem duplicat.) : %d\n",                     n_total_pg))
cat(sprintf("  Acórdãos 2ª inst. coletados         : %d\n",                     n_2a))
cat(sprintf("  Processos únicos (todos)            : %d\n\n",                   n_total))

# ── 2. DISTRIBUIÇÃO ANUAL ─────────────────────────────────────────────────────
cat(sep2, "\n")
cat("2. ANO DA SENTENÇA — 1ª INSTÂNCIA (canônicas)\n")
cat(sep2, "\n")

cjpg_can |>
  mutate(ano = year(disponibilizacao)) |>
  filter(!is.na(ano)) |>
  count(ano, sort = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  mutate(linha = sprintf("    %d : %3d sentenças (%4.1f%%)", ano, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n  (2026 em curso — ano incompleto)\n\n")

# ── 3. LOCALIZAÇÃO ────────────────────────────────────────────────────────────
cat(sep2, "\n")
cat("3. DISTRIBUIÇÃO GEOGRÁFICA — TOP 15 COMARCAS (1ª inst.)\n")
cat(sep2, "\n")

cjpg_can |>
  count(comarca, sort = TRUE) |>
  head(15) |>
  mutate(pct = round(100 * n / n_total_pg, 1)) |>
  mutate(linha = sprintf("    %-30s : %3d (%4.1f%%)", comarca, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 4. PAPEL DA EMPRESA ───────────────────────────────────────────────────────
cat(sep2, "\n")
cat("4. POLO DA AÇÃO — A R de Araújo é RÉ em quase todos os processos\n")
cat(sep2, "\n")

papel_t <- cjpg_can |>
  filter(!is.na(papel)) |>
  count(papel, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

papel_t |>
  mutate(linha = sprintf("    %-10s : %3d (%4.1f%%)", papel, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat(sprintf("    (%d sentenças sem papel identificado)\n\n",
            sum(is.na(cjpg_can$papel))))

# ── 5. TIPO DE DECISÃO (CPC) ─────────────────────────────────────────────────
cat(sep2, "\n")
cat("5. NATUREZA DAS DECISÕES (CPC)\n")
cat(sep2, "\n")

cjpg_can |>
  count(tipo_decisao_cpc, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  mutate(linha = sprintf("    %-25s : %3d (%4.1f%%)",
                         tipo_decisao_cpc, n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n  Legenda:\n")
cat("    merito_487_I            → sentença de mérito (art. 487, I, CPC)\n")
cat("    homologacao_487_III     → homologação de acordo / transação (487, III)\n")
cat("    extincao_execucao_924   → cumprimento de sentença extinto pelo pagamento (924)\n")
cat("    extincao_sem_merito_485 → extinção sem mérito (art. 485)\n\n")

# ── 6. PROCEDÊNCIA POR PAPEL DA EMPRESA ──────────────────────────────────────
cat(sep2, "\n")
cat("6. RESULTADO POR PAPEL DA EMPRESA — 1ª INSTÂNCIA (decisões de mérito)\n")
cat(sep2, "\n")

df_proc <- cjpg_can |>
  filter(!is.na(papel),
         tipo_decisao_cpc %in% c("merito_487_I", "homologacao_487_III")) |>
  count(papel, resultado_dispositivo) |>
  group_by(papel) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup()

for (p in c("re", "autora")) {
  sub <- df_proc |> filter(papel == p)
  if (nrow(sub) == 0) next
  cat(sprintf("\n  Empresa como %s (n=%d):\n", toupper(p), sum(sub$n)))
  sub |>
    mutate(linha = sprintf("    %-15s : %3d (%4.1f%%)",
                           resultado_dispositivo, n, pct)) |>
    pull(linha) |> cat(sep = "\n")
}

cat(sprintf("\n\n  >>> TAXA PRÓ-CONSUMIDOR (decisões de mérito): %.1f%% (%d de %d)\n\n",
            pct_pro, n_pro, nrow(merito_re)))

# ── 7. VALORES DE INDENIZAÇÃO ─────────────────────────────────────────────────
cat(sep2, "\n")
cat("7. VALORES DE INDENIZAÇÃO — 1ª INSTÂNCIA\n")
cat("   (extraídos do DISPOSITIVO de cada sentença canônica)\n")
cat(sep2, "\n")

valores <- cjpg_can |> filter(!is.na(valor_indenizacao))

if (nrow(valores) > 0) {
  cat(sprintf("  Sentenças com valor identificado : %d\n", nrow(valores)))
  cat(sprintf("  Valor mínimo                     : R$ %s\n",
              fmt_brl(min(valores$valor_indenizacao))))
  cat(sprintf("  P25                              : R$ %s\n",
              fmt_brl(quantile(valores$valor_indenizacao, 0.25))))
  cat(sprintf("  Mediana                          : R$ %s\n",
              fmt_brl(median(valores$valor_indenizacao))))
  cat(sprintf("  Média                            : R$ %s\n",
              fmt_brl(mean(valores$valor_indenizacao))))
  cat(sprintf("  P75                              : R$ %s\n",
              fmt_brl(quantile(valores$valor_indenizacao, 0.75))))
  cat(sprintf("  Valor máximo                     : R$ %s\n\n",
              fmt_brl(max(valores$valor_indenizacao))))

  cat("  Faixas de valor:\n")
  valores |>
    mutate(faixa = case_when(
      valor_indenizacao <  2000 ~ "até R$ 2.000",
      valor_indenizacao <  5000 ~ "R$ 2.001 a R$ 5.000",
      valor_indenizacao < 10000 ~ "R$ 5.001 a R$ 10.000",
      valor_indenizacao < 20000 ~ "R$ 10.001 a R$ 20.000",
      TRUE                      ~ "acima de R$ 20.000"
    )) |>
    count(faixa, sort = TRUE) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    mutate(linha = sprintf("    %-25s : %3d (%4.1f%%)", faixa, n, pct)) |>
    pull(linha) |> cat(sep = "\n")
} else {
  cat("  Nenhum valor identificado.\n")
}
cat("\n")

# ── 8. TEMPO DE TRAMITAÇÃO ───────────────────────────────────────────────────
cat(sep2, "\n")
cat("8. TEMPO ENTRE AJUIZAMENTO E SENTENÇA\n")
cat("   (decisões de mérito, empresa ré, processos com data calculável)\n")
cat(sep2, "\n")

tempo_base <- cjpg_can |>
  filter(papel == "re",
         tipo_decisao_cpc == "merito_487_I",
         !is.na(tempo_tramitacao_dias),
         tempo_tramitacao_dias >= 0,
         tempo_tramitacao_dias <= 3650)

cat(sprintf("  Sentenças analisadas      : %d\n", nrow(tempo_base)))
cat(sprintf("  Tempo MÍNIMO              : %d dias\n", min(tempo_base$tempo_tramitacao_dias)))
cat(sprintf("  P25                       : %d dias\n",
            as.integer(quantile(tempo_base$tempo_tramitacao_dias, 0.25))))
cat(sprintf("  MEDIANA                   : %d dias (~%.1f meses)\n",
            median(tempo_base$tempo_tramitacao_dias),
            median(tempo_base$tempo_tramitacao_dias) / 30))
cat(sprintf("  MÉDIA                     : %d dias (~%.1f meses)\n",
            round(mean(tempo_base$tempo_tramitacao_dias)),
            mean(tempo_base$tempo_tramitacao_dias) / 30))
cat(sprintf("  P75                       : %d dias\n",
            as.integer(quantile(tempo_base$tempo_tramitacao_dias, 0.75))))
cat(sprintf("  Tempo MÁXIMO              : %d dias\n\n",
            max(tempo_base$tempo_tramitacao_dias)))

cat("  Faixas de tempo:\n")
tempo_base |>
  mutate(faixa = case_when(
    tempo_tramitacao_dias <  180 ~ "Até 6 meses",
    tempo_tramitacao_dias <  365 ~ "6 meses a 1 ano",
    tempo_tramitacao_dias <  730 ~ "1 a 2 anos",
    tempo_tramitacao_dias < 1095 ~ "2 a 3 anos",
    TRUE                         ~ "Mais de 3 anos"
  ),
  faixa = factor(faixa, levels = c("Até 6 meses","6 meses a 1 ano",
                                   "1 a 2 anos","2 a 3 anos","Mais de 3 anos"))) |>
  count(faixa) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  mutate(linha = sprintf("    %-20s : %3d (%4.1f%%)", as.character(faixa), n, pct)) |>
  pull(linha) |> cat(sep = "\n")
cat("\n")

# ── 9. TUTELAS DE URGÊNCIA E REVELIA ─────────────────────────────────────────
cat(sep2, "\n")
cat("9. INDICADORES PROCESSUAIS\n")
cat(sep2, "\n")

cat(sprintf("  Tutela de urgência deferida      : %3d sentenças (%4.1f%%)\n",
            n_tutela, pct_tut))
n_rev <- sum(cjpg_can$revelia, na.rm = TRUE)
cat(sprintf("  Revelia declarada                : %3d sentenças (%4.1f%%)\n\n",
            n_rev, 100 * n_rev / n_total_pg))

# ── 10. TEMAS NOS JULGADOS ──────────────────────────────────────────────────
cat(sep2, "\n")
cat("10. TEMAS IDENTIFICADOS NAS SENTENÇAS\n")
cat(sep2, "\n")

temas <- tibble(
  tema = c("Protesto indevido", "Danos morais",
           "Duplicata", "Ausência de contrato",
           "Relação jurídica inexistente"),
  flag = c("tem_protesto", "tem_danos",
           "tem_duplicata", "tem_contrato",
           "tem_rel_juridica")
)

for (i in seq_len(nrow(temas))) {
  f <- temas$flag[i]
  if (f %in% names(cjpg_can)) {
    n <- sum(cjpg_can[[f]], na.rm = TRUE)
    cat(sprintf("    %-30s : %3d (%4.1f%%)\n",
                temas$tema[i], n, 100 * n / n_total_pg))
  }
}
cat("\n")

# ── 11. ANEXOS GERADOS ──────────────────────────────────────────────────────
cat(sep2, "\n")
cat("11. ANEXOS GERADOS NESTA PESQUISA\n")
cat(sep2, "\n")
cat("  Em analise/graficos/:\n")
cat("    01_serie_temporal.png             — sentenças por ano\n")
cat("    02_top_comarcas.png               — top 12 comarcas\n")
cat("    03_valor_mediana_por_ano.png      — evolução do quantum\n")
cat("    04_histograma_valores.png         — distribuição dos valores\n")
cat("    05_facet_procedencia_por_papel.png — resultado por papel\n")
cat("    06_pro_contra_consumidor.png      — síntese pró/contra consumidor\n")
cat("    07_tempo_tramitacao.png           — tempo entre ajuizamento e sentença\n\n")
cat("  Em analise/:\n")
cat("    validacao_julgados_1a.html        — DT navegável com todas as 270 sentenças\n")
cat("    precedentes_campeoes.html         — top 30 precedentes mais citáveis\n")
cat("    precedentes_campeoes.csv          — idem em CSV (Excel)\n")
cat("    wordcloud_julgados_1a.html        — nuvem de todos os julgados\n")
cat("    wordcloud_consumidor_venceu.html  — nuvem só dos casos pró-consumidor\n\n")

cat(sep, "\n")
cat("  Fonte: TJSP — ESAJ (dados públicos)\n")
cat("  Metodologia: coleta automatizada via pacote {tjsp} (R)\n")
cat("  Pipeline: scripts 01–13 em jurimetria_thierry/\n")
cat(sep, "\n\n")

# Fecha sink antes das word clouds (htmlwidgets escreve fora do .txt)
sink(); close(.relat_file)
on.exit()  # cancela o handler já que fechamos manualmente

# ── WORD CLOUDS (anexos do relatório) ───────────────────────────────────────
library(tidytext)
library(wordcloud2)
library(htmlwidgets)

stop_pt <- stopwords::stopwords("pt", source = "stopwords-iso")
stop_extra <- c(
  "processo", "autor", "autora", "ré", "réu", "parte", "partes",
  "sentença", "ação", "autos", "juiz", "juíza", "mm", "dr", "dra",
  "fls", "art", "lei", "código", "cpc", "cdc", "inc",
  "ante", "exposto", "julgo", "procedente", "improcedente",
  "condeno", "condena", "posto", "assim", "portanto", "diante",
  "vez", "ainda", "bem", "já", "tal", "caso", "forma", "modo", "data",
  "requerente", "requerida", "requerido", "presente"
)
stop_todos <- unique(c(stop_pt, stop_extra))

tokenizar <- function(df) {
  df |>
    filter(!is.na(julgado)) |>
    select(processo, julgado) |>
    unnest_tokens(word, julgado) |>
    filter(
      !word %in% stop_todos,
      str_length(word) > 3,
      !str_detect(word, "^[0-9]+$")
    ) |>
    count(word, sort = TRUE)
}

# Nuvem 1: todos os julgados canônicos
freq <- tokenizar(cjpg_can)
wc_geral <- wordcloud2(head(freq, 150), size = 0.6,
                       color = "random-dark", backgroundColor = "white")
saveWidget(wc_geral,
           file = normalizePath(file.path(DIR_ANALISE, "wordcloud_julgados_1a.html"),
                                mustWork = FALSE),
           selfcontained = FALSE)

# Nuvem 2: só processos em que o consumidor venceu
freq_venceu <- tokenizar(cjpg_can |> filter(favorabilidade == "consumidor_venceu"))
wc_venceu <- wordcloud2(head(freq_venceu, 150), size = 0.6,
                        color = "#155724", backgroundColor = "white")
saveWidget(wc_venceu,
           file = normalizePath(file.path(DIR_ANALISE, "wordcloud_consumidor_venceu.html"),
                                mustWork = FALSE),
           selfcontained = FALSE)

message("Etapa 13 concluída.")
message("  Relatório: ", file.path(DIR_ANALISE, "relatorio_jurimetrico.txt"))
message("  Word clouds: ", DIR_ANALISE)
