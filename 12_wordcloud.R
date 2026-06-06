# ETAPA 12 — Word Cloud dos julgados (1ª instância)
# Pacotes: wordcloud2, tidytext, stopwords
# Instalar se necessário:
#   install.packages(c("wordcloud2", "tidytext", "stopwords", "htmlwidgets"))

library(tidyverse)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── Stopwords em português ────────────────────────────────────────────────────
stop_pt <- stopwords::stopwords("pt", source = "stopwords-iso")

# Palavras técnicas/processuais genéricas que não agregam — ajuste à vontade
stop_extra <- c(
  "processo", "autor", "autora", "ré", "réu", "parte", "partes",
  "sentença", "ação", "autos", "juiz", "juíza", "mm", "dr", "dra",
  "fls", "art", "lei", "código", "cpc", "cdc", "inc",
  "ante", "exposto", "julgo", "procedente", "improcedente",
  "condeno", "condena", "nos", "nos", "ante", "posto",
  "assim", "portanto", "diante", "vez", "ainda", "bem",
  "vez", "já", "tal", "caso", "forma", "modo", "data",
  "requerente", "requerida", "requerido", "presente"
)

stop_todos <- unique(c(stop_pt, stop_extra))

# ── Tokeniza e conta ──────────────────────────────────────────────────────────
freq <- cjpg |>
  filter(duplicado == FALSE, !is.na(julgado)) |>
  select(processo, julgado) |>
  unnest_tokens(word, julgado) |>
  filter(
    !word %in% stop_todos,
    str_length(word) > 3,          # remove palavras curtas
    !str_detect(word, "^[0-9]+$")  # remove tokens só numéricos
  ) |>
  count(word, sort = TRUE)

cat("Top 30 palavras:\n")
print(head(freq, 30))

# ── Word cloud principal (todos os julgados) ──────────────────────────────────
wc_geral <- wordcloud2(
  head(freq, 150),
  size        = 0.6,
  color       = "random-dark",
  backgroundColor = "white",
  fontFamily  = "sans-serif"
)

out_geral <- normalizePath("analise/wordcloud_julgados_1a.html", mustWork = FALSE)
saveWidget(wc_geral, file = out_geral, selfcontained = FALSE)
message("Word cloud geral salvo: ", out_geral)
browseURL(out_geral)

# ── Word cloud — só processos em que consumidor venceu ───────────────────────
freq_venceu <- cjpg |>
  filter(duplicado == FALSE, favorabilidade == "consumidor_venceu", !is.na(julgado)) |>
  select(processo, julgado) |>
  unnest_tokens(word, julgado) |>
  filter(
    !word %in% stop_todos,
    str_length(word) > 3,
    !str_detect(word, "^[0-9]+$")
  ) |>
  count(word, sort = TRUE)

wc_venceu <- wordcloud2(
  head(freq_venceu, 150),
  size        = 0.6,
  color       = "#155724",
  backgroundColor = "white",
  fontFamily  = "sans-serif"
)

out_venceu <- normalizePath("analise/wordcloud_consumidor_venceu.html", mustWork = FALSE)
saveWidget(wc_venceu, file = out_venceu, selfcontained = FALSE)
message("Word cloud (consumidor venceu) salvo: ", out_venceu)
browseURL(out_venceu)
