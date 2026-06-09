# ETAPA 11 — Gráficos para a petição inicial (1ª instância)
# Salva PNGs em analise/graficos/
#
# 1. Série temporal — sentenças por ano
# 2. Top comarcas — barras horizontais
# 3. Evolução do valor mediano de indenização por ano
# 4. Histograma de valores de indenização
# 5. Facet wrap — taxa de procedência por papel (ré vs autora)

library(tidyverse)
library(scales)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

DIR_GRAF <- "analise/graficos"
dir.create(DIR_GRAF, showWarnings = FALSE, recursive = TRUE)

cjpg <- readRDS("dados/compilados/cjpg.rds") |>
  filter(decisao_canonica)                       # base limpa, sem duplicatas

cat("Base de análise:", nrow(cjpg), "sentenças canônicas\n\n")

# ── Tema padrão ──────────────────────────────────────────────────────────────
tema <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption  = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank()
  )

CAPTION <- "Fonte: TJSP — ESAJ (1ª inst.). Coleta: pacote {tjsp}."

# ── 1. SÉRIE TEMPORAL (linha) ────────────────────────────────────────────────
serie <- cjpg |>
  mutate(ano = year(disponibilizacao)) |>
  count(ano)

g1 <- ggplot(serie, aes(ano, n)) +
  geom_line(color = "#2c5282", linewidth = 1) +
  geom_point(color = "#2c5282", size = 3) +
  geom_text(aes(label = n), vjust = -1.1, size = 3.5, color = "grey30") +
  scale_x_continuous(breaks = seq(2017, 2026, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Sentenças por ano contra A R de Araújo Comunicações",
    subtitle = paste0("1ª instância TJSP — total: ", nrow(cjpg),
                      " sentenças únicas (2026 em andamento)"),
    x = NULL, y = "Sentenças",
    caption = CAPTION
  ) +
  tema

ggsave(file.path(DIR_GRAF, "01_serie_temporal.png"), g1,
       width = 9, height = 5, dpi = 150)

# ── 2. TOP COMARCAS ──────────────────────────────────────────────────────────
g2 <- cjpg |>
  count(comarca, sort = TRUE) |>
  slice_head(n = 12) |>
  mutate(comarca = fct_reorder(comarca, n)) |>
  ggplot(aes(n, comarca)) +
  geom_col(fill = "#2f855a") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Dispersão geográfica das ações",
    subtitle = "Top 12 comarcas com mais sentenças contra a empresa (1ª inst.)",
    x = "Sentenças", y = NULL,
    caption = CAPTION
  ) +
  tema

ggsave(file.path(DIR_GRAF, "02_top_comarcas.png"), g2,
       width = 9, height = 6, dpi = 150)

# ── 3. EVOLUÇÃO DO VALOR MEDIANO POR ANO ────────────────────────────────────
val_por_ano <- cjpg |>
  filter(!is.na(valor_morais)) |>
  mutate(ano = year(disponibilizacao)) |>
  group_by(ano) |>
  summarise(
    n       = n(),
    mediana = median(valor_morais),
    p25     = quantile(valor_morais, 0.25),
    p75     = quantile(valor_morais, 0.75),
    .groups = "drop"
  )

g3 <- ggplot(val_por_ano, aes(ano, mediana)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#fed7aa", alpha = 0.6) +
  geom_line(color = "#c2410c", linewidth = 1) +
  geom_point(color = "#c2410c", size = 3) +
  geom_text(aes(label = paste0("R$ ", format(mediana, big.mark = "."))),
            vjust = -1.2, size = 3.2, color = "grey30") +
  scale_x_continuous(breaks = seq(2017, 2026, 1)) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".")) +
  labs(
    title    = "Indenização por danos morais — mediana por ano",
    subtitle = "Área sombreada = intervalo interquartil (P25–P75) — 1ª inst.",
    x = NULL, y = "Mediana (R$)",
    caption = CAPTION
  ) +
  tema

ggsave(file.path(DIR_GRAF, "03_valor_mediana_por_ano.png"), g3,
       width = 9, height = 5, dpi = 150)

# ── 4. HISTOGRAMA DE VALORES ────────────────────────────────────────────────
med_val <- median(cjpg$valor_morais, na.rm = TRUE)
mean_val <- mean(cjpg$valor_morais, na.rm = TRUE)

g4 <- cjpg |>
  filter(!is.na(valor_morais)) |>
  ggplot(aes(valor_morais)) +
  geom_histogram(binwidth = 2500, fill = "#c2410c", color = "white", boundary = 0) +
  geom_vline(xintercept = med_val,  linetype = "dashed", color = "grey20", linewidth = 0.7) +
  geom_vline(xintercept = mean_val, linetype = "dotted", color = "#1f4e79", linewidth = 0.7) +
  annotate("text", x = med_val,  y = Inf, vjust = 1.5, hjust = -0.05,
           label = paste0("Mediana: R$ ", format(round(med_val), big.mark = ".")),
           size = 3.5, color = "grey20") +
  annotate("text", x = mean_val, y = Inf, vjust = 3.5, hjust = -0.05,
           label = paste0("Média: R$ ",  format(round(mean_val), big.mark = ".")),
           size = 3.5, color = "#1f4e79") +
  scale_x_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".")) +
  labs(
    title    = "Distribuição do quantum de dano moral",
    subtitle = paste0(sum(!is.na(cjpg$valor_morais)),
                      " sentenças com valor extraído do dispositivo (1ª inst.)"),
    x = "Valor (R$)", y = "Sentenças",
    caption = CAPTION
  ) +
  tema

ggsave(file.path(DIR_GRAF, "04_histograma_valores.png"), g4,
       width = 9, height = 5, dpi = 150)

# ── 5. FACET WRAP — taxa de procedência por papel (Procedente integral + Parcial empilhados) ──
df_merito <- cjpg |>
  filter(!is.na(papel),
         tipo_decisao_cpc %in% c("merito_487_I", "homologacao_487_III")) |>
  mutate(
    papel = factor(papel,
                   levels = c("re", "autora"),
                   labels = c("Empresa como RÉ", "Empresa como AUTORA")),
    # Categoria agrupadora (eixo X) — Procedente e Parcial ficam na mesma coluna
    categoria = case_when(
      resultado_dispositivo %in% c("procedente", "parcial") ~ "Procedente",
      resultado_dispositivo == "improcedente"               ~ "Improcedente",
      resultado_dispositivo == "homologacao"                ~ "Homologação\n(acordo)",
      TRUE                                                  ~ "Outro"
    ),
    categoria = factor(categoria,
                       levels = c("Procedente", "Homologação\n(acordo)",
                                  "Improcedente", "Outro")),
    # Subcategoria (fill stacked dentro da coluna)
    subtipo = case_when(
      resultado_dispositivo == "procedente"   ~ "Integral",
      resultado_dispositivo == "parcial"      ~ "Parcial",
      resultado_dispositivo == "improcedente" ~ "Improcedente",
      resultado_dispositivo == "homologacao"  ~ "Homologação",
      TRUE                                    ~ "Outro"
    ),
    subtipo = factor(subtipo,
                     levels = c("Integral", "Parcial", "Homologação",
                                "Improcedente", "Outro"))
  )

df_facet <- df_merito |>
  count(papel, categoria, subtipo) |>
  group_by(papel) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

# Total por (papel, categoria) para o rótulo no topo
totais_cat <- df_facet |>
  group_by(papel, categoria) |>
  summarise(pct_total = sum(pct), n_total = sum(n), .groups = "drop")

g5 <- ggplot(df_facet, aes(categoria, pct, fill = subtipo)) +
  geom_col() +
  geom_text(
    data = totais_cat,
    aes(x = categoria, y = pct_total,
        label = paste0(round(pct_total, 1), "%\n(n=", n_total, ")"),
        fill = NULL),
    vjust = -0.2, size = 3.4, color = "grey20", lineheight = 0.9
  ) +
  facet_wrap(~papel, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = label_percent(scale = 1)) +
  scale_fill_manual(values = c(
    "Integral"     = "#2f855a",     # verde escuro
    "Parcial"      = "#9ae6b4",     # verde claro
    "Homologação"  = "#90cdf4",     # azul
    "Improcedente" = "#c53030",     # vermelho
    "Outro"        = "#a0aec0"
  )) +
  labs(
    title    = "Resultado das sentenças por papel da A R de Araújo",
    subtitle = "Decisões de mérito (art. 487, I e III) — 1ª inst.\n'Procedente' empilha integral (verde escuro) + parcial (verde claro)",
    x = NULL, y = "% das sentenças",
    fill = NULL,
    caption = CAPTION
  ) +
  tema +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(DIR_GRAF, "05_facet_procedencia_por_papel.png"), g5,
       width = 11, height = 6, dpi = 150)

# ── 6. PRÓ vs CONTRA CONSUMIDOR ─────────────────────────────────────────────
# Agrega resultado em "pró consumidor" / "contra consumidor" considerando o papel
# da empresa:
#   - Empresa ré + procedente/parcial/homologação → pró-consumidor
#   - Empresa ré + improcedente                    → contra-consumidor
#   - Empresa autora + procedente/parcial          → contra-consumidor
#   - Empresa autora + improcedente                → pró-consumidor

df_pro_contra <- cjpg |>
  filter(!is.na(papel),
         tipo_decisao_cpc %in% c("merito_487_I", "homologacao_487_III"),
         resultado_dispositivo %in% c("procedente","parcial","improcedente","homologacao")) |>
  mutate(
    lado = case_when(
      papel == "re"     & resultado_dispositivo %in% c("procedente","parcial","homologacao") ~ "Pró-consumidor",
      papel == "re"     & resultado_dispositivo == "improcedente"                            ~ "Contra-consumidor",
      papel == "autora" & resultado_dispositivo %in% c("procedente","parcial")               ~ "Contra-consumidor",
      papel == "autora" & resultado_dispositivo == "improcedente"                            ~ "Pró-consumidor",
      papel == "autora" & resultado_dispositivo == "homologacao"                             ~ "Pró-consumidor"
    ),
    lado = factor(lado, levels = c("Pró-consumidor", "Contra-consumidor"))
  ) |>
  count(lado) |>
  mutate(pct = 100 * n / sum(n))

g6 <- ggplot(df_pro_contra, aes(lado, pct, fill = lado)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            vjust = -0.2, size = 4.5, color = "grey20", lineheight = 0.9,
            fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = label_percent(scale = 1)) +
  scale_fill_manual(values = c(
    "Pró-consumidor"    = "#2f855a",
    "Contra-consumidor" = "#c53030"
  )) +
  labs(
    title    = "Resultado a favor / contra o consumidor",
    subtitle = paste0("Decisões de mérito 1ª inst. — n total: ", sum(df_pro_contra$n)),
    x = NULL, y = "% das sentenças",
    caption = CAPTION
  ) +
  tema +
  theme(legend.position = "none")

ggsave(file.path(DIR_GRAF, "06_pro_contra_consumidor.png"), g6,
       width = 7, height = 5.5, dpi = 150)

# ── Resumo no console ────────────────────────────────────────────────────────
cat("✓ 5 gráficos salvos em:", DIR_GRAF, "\n")
list.files(DIR_GRAF) |> walk(\(f) cat("   -", f, "\n"))
