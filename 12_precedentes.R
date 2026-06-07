# ETAPA 12 — Precedentes campeões (sentenças mais citáveis para a inicial)
#
# Critérios de força:
#   + flags temáticas (sem usar revelia/danos):
#       tem_protesto, tem_duplicata, tem_rel_juridica, tem_contrato
#   + tem_tutela_deferida → ganha ponto (liminar, item do pedido da FAM)
#   + resultado_dispositivo == "procedente" (integral) → mais forte que parcial
#   + valor_indenizacao >= mediana (R$ 4.200) → robustece o quantum
#
# Saídas:
#   analise/precedentes_campeoes.csv
#   analise/precedentes_campeoes.html  (DT navegável)

library(tidyverse)
library(DT)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── 1. Universo elegível ──────────────────────────────────────────────────
# Sentenças canônicas, empresa como ré, mérito decidido pró-consumidor
elegiveis <- cjpg |>
  filter(
    decisao_canonica,
    papel == "re",
    tipo_decisao_cpc == "merito_487_I",
    favorabilidade   == "consumidor_venceu"
  )

cat("Universo de sentenças elegíveis:", nrow(elegiveis), "\n")

# ── 2. Score de força para citação ───────────────────────────────────────
mediana_val <- median(cjpg$valor_indenizacao, na.rm = TRUE)

scored <- elegiveis |>
  mutate(
    score_temas = as.integer(tem_protesto) +
                  as.integer(tem_duplicata) +
                  as.integer(tem_rel_juridica) +
                  as.integer(tem_contrato),                  # 0–4
    score_proced = if_else(resultado_dispositivo == "procedente", 1L, 0L),
    score_tutela = if_else(coalesce(tem_tutela_deferida, FALSE), 1L, 0L),
    score_valor  = if_else(!is.na(valor_indenizacao) & valor_indenizacao >= mediana_val,
                           1L, 0L),
    score_total  = score_temas + score_proced + score_tutela + score_valor
  ) |>
  arrange(desc(score_total), desc(valor_indenizacao), desc(disponibilizacao))

cat("\nDistribuição do score_total:\n")
print(table(scored$score_total))

# ── 3. Curadoria — top precedentes ───────────────────────────────────────
TOP_N <- 30
top <- scored |>
  slice_head(n = TOP_N) |>
  mutate(
    ano  = year(disponibilizacao),
    data = format(disponibilizacao, "%d/%m/%Y"),
    # trecho do dispositivo (primeiros 600 chars)
    dispositivo_trecho = str_sub(dispositivo_txt, 1, 600)
  ) |>
  select(
    score_total, score_temas, score_proced, score_tutela, score_valor,
    processo, data, ano, comarca, vara, magistrado,
    resultado_dispositivo, valor_indenizacao,
    tem_protesto, tem_duplicata, tem_rel_juridica, tem_contrato, tem_tutela_deferida,
    dispositivo_trecho, dispositivo_txt
  )

# ── 4. CSV ────────────────────────────────────────────────────────────────
csv_out <- "analise/precedentes_campeoes.csv"
write.csv(top, csv_out, row.names = FALSE, fileEncoding = "UTF-8")
message("CSV salvo: ", csv_out)

# ── 5. DT navegável ──────────────────────────────────────────────────────
tabela <- datatable(
  top,
  extensions = c("Buttons"),
  filter     = "top",
  rownames   = FALSE,
  escape     = FALSE,
  options    = list(
    pageLength = 30,
    scrollX    = TRUE,
    autoWidth  = FALSE,
    dom        = "Blfrtip",
    buttons    = c("csv", "excel"),
    order      = list(list(0, "desc")),                    # score desc
    columnDefs = list(
      list(width = "500px",
           targets = which(names(top) %in% c("dispositivo_trecho","dispositivo_txt")) - 1),
      list(className = "dt-center",
           targets = which(names(top) %in%
                           c("score_total","score_temas","score_proced",
                             "score_tutela","score_valor")) - 1),
      list(className = "dt-left", targets = "_all")
    ),
    language   = list(
      search     = "Buscar:",
      lengthMenu = "Mostrar _MENU_",
      info       = "_START_ a _END_ de _TOTAL_",
      paginate   = list(previous = "<", `next` = ">")
    )
  ),
  caption = htmltools::tags$caption(
    style = "caption-side: top; font-size: 1.1em; font-weight: bold;",
    paste0("Top ", TOP_N,
           " precedentes mais citáveis — A R de Araújo como ré (1ª inst., consumidor venceu)")
  )
) |>
  formatStyle(
    "score_total",
    background = styleColorBar(c(0, max(top$score_total)), "#9ae6b4"),
    fontWeight = "bold"
  ) |>
  formatStyle(
    "resultado_dispositivo",
    backgroundColor = styleEqual(c("procedente", "parcial"),
                                 c("#d4edda",   "#fff3cd"))
  ) |>
  formatCurrency("valor_indenizacao", currency = "R$ ",
                 mark = ".", dec.mark = ",", digits = 0)

html_out <- normalizePath("analise/precedentes_campeoes.html", mustWork = FALSE)
htmlwidgets::saveWidget(tabela, file = html_out, selfcontained = FALSE)
message("HTML salvo: ", html_out)
browseURL(html_out)

# ── 6. Resumo ─────────────────────────────────────────────────────────────
cat("\n=== TOP 10 precedentes (score) ===\n")
top |>
  slice_head(n = 10) |>
  select(processo, comarca, magistrado, resultado_dispositivo,
         valor_indenizacao, score_total) |>
  print(n = Inf)
