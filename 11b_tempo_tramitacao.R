# ETAPA 11b — Tempo de tramitação dos processos (1ª inst.)
#
# Usa tjsp::tjsp_ler_movimentacao() para ler todas as movimentações dos
# HTMLs em dados/cpopg_html/ e calcular:
#
#   data_distribuicao = MIN(dt_mov) por processo
#   data_sentenca     = cjpg$disponibilizacao
#   tempo_dias        = data_sentenca - data_distribuicao
#
# Outputs:
#   PNG  → analise/graficos/07_tempo_tramitacao.png
#   Coluna nova no cjpg.rds: tempo_tramitacao_dias

library(tjsp)
library(tidyverse)
library(scales)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── 1. Lê todas as movimentações dos cpopg ────────────────────────────────
message("Lendo movimentacoes de dados/cpopg_html/ ...")
mov <- tjsp_ler_movimentacao(diretorio = "dados/cpopg_html")
message("  Movimentações lidas: ", nrow(mov))

# ── 2. Data de distribuição por processo ──────────────────────────────────
# min(dt_mov) é o primeiro ato → distribuição/protocolo
data_dist <- mov |>
  filter(!is.na(dt_mov)) |>
  group_by(processo) |>
  summarise(data_distribuicao = min(dt_mov), .groups = "drop")

cat("Processos com data de distribuição:", nrow(data_dist), "\n")

# ── 3. Junta no cjpg e calcula tempo ──────────────────────────────────────
cjpg <- cjpg |>
  select(-any_of(c("data_distribuicao", "tempo_tramitacao_dias"))) |>
  left_join(data_dist, by = "processo") |>
  mutate(
    tempo_tramitacao_dias = as.integer(
      as.Date(disponibilizacao) - data_distribuicao
    )
  )

# Sanidade: tempos negativos ou enormes
cat("\n=== Sanidade do tempo_tramitacao_dias ===\n")
cat("Com valor calculado:", sum(!is.na(cjpg$tempo_tramitacao_dias)), "\n")
cat("Negativos (problema):", sum(cjpg$tempo_tramitacao_dias < 0, na.rm = TRUE), "\n")
cat("Acima de 10 anos    :", sum(cjpg$tempo_tramitacao_dias > 3650, na.rm = TRUE), "\n")

# ── 4. Estatísticas — só sentenças canônicas, papel = ré ────────────────
base <- cjpg |>
  filter(
    decisao_canonica,
    papel == "re",
    tipo_decisao_cpc == "merito_487_I",
    !is.na(tempo_tramitacao_dias),
    tempo_tramitacao_dias >= 0,
    tempo_tramitacao_dias <= 3650
  )

cat("\n=== Tempo de tramitação — sentenças canônicas (1ª inst., mérito, ré) ===\n")
base |>
  summarise(
    n           = n(),
    media_dias  = round(mean(tempo_tramitacao_dias)),
    mediana_dias= median(tempo_tramitacao_dias),
    p25         = quantile(tempo_tramitacao_dias, 0.25),
    p75         = quantile(tempo_tramitacao_dias, 0.75),
    max         = max(tempo_tramitacao_dias)
  ) |>
  mutate(
    media_meses   = round(media_dias / 30, 1),
    mediana_meses = round(mediana_dias / 30, 1),
    mediana_anos  = round(mediana_dias / 365, 1)
  ) |>
  print()

# Faixas
cat("\n=== Faixas de tempo (sentenças de mérito) ===\n")
base |>
  mutate(
    faixa = case_when(
      tempo_tramitacao_dias <  180 ~ "Até 6 meses",
      tempo_tramitacao_dias <  365 ~ "6 meses a 1 ano",
      tempo_tramitacao_dias <  730 ~ "1 a 2 anos",
      tempo_tramitacao_dias < 1095 ~ "2 a 3 anos",
      tempo_tramitacao_dias < 1825 ~ "3 a 5 anos",
      TRUE                         ~ "Mais de 5 anos"
    ),
    faixa = factor(faixa, levels = c("Até 6 meses","6 meses a 1 ano","1 a 2 anos",
                                     "2 a 3 anos","3 a 5 anos","Mais de 5 anos"))
  ) |>
  count(faixa) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print(n = Inf)

# ── 5. GRÁFICO ────────────────────────────────────────────────────────────
DIR_GRAF <- "analise/graficos"
dir.create(DIR_GRAF, showWarnings = FALSE, recursive = TRUE)

tema <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption  = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank()
  )

med  <- median(base$tempo_tramitacao_dias)
mean_ <- mean(base$tempo_tramitacao_dias)

g7 <- ggplot(base, aes(tempo_tramitacao_dias)) +
  geom_histogram(binwidth = 90, fill = "#6b46c1", color = "white", boundary = 0) +
  geom_vline(xintercept = med,   linetype = "dashed", color = "grey20", linewidth = 0.7) +
  geom_vline(xintercept = mean_, linetype = "dotted", color = "#1f4e79", linewidth = 0.7) +
  annotate("text", x = med,   y = Inf, vjust = 1.5, hjust = -0.05,
           label = sprintf("Mediana: %d dias (~%.1f meses)", med, med/30),
           size = 3.5, color = "grey20") +
  annotate("text", x = mean_, y = Inf, vjust = 3.5, hjust = -0.05,
           label = sprintf("Média: %d dias (~%.1f meses)", round(mean_), mean_/30),
           size = 3.5, color = "#1f4e79") +
  scale_x_continuous(breaks = seq(0, 3650, 365),
                     labels = function(x) paste0(round(x/365, 1), "a")) +
  labs(
    title    = "Tempo entre ajuizamento e sentença",
    subtitle = paste0(nrow(base),
                      " sentenças de mérito (1ª inst., empresa ré) — eixo X em anos, bins de 3 meses"),
    x = "Anos de tramitação", y = "Sentenças",
    caption = "Fonte: TJSP — ESAJ. data_distribuicao = MIN(dt_mov) por processo."
  ) +
  tema

ggsave(file.path(DIR_GRAF, "07_tempo_tramitacao.png"), g7,
       width = 9, height = 5, dpi = 150)

message("Gráfico salvo: ", file.path(DIR_GRAF, "07_tempo_tramitacao.png"))

# ── 6. Salva no cjpg.rds ─────────────────────────────────────────────────
saveRDS(cjpg, "dados/compilados/cjpg.rds")
write.csv(cjpg, "dados/compilados/cjpg.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("Coluna 'tempo_tramitacao_dias' e 'data_distribuicao' adicionadas ao cjpg.rds")
