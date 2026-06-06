# ETAPA 11 — DT só dos processos com múltiplas decisões na base
# Mostra lado a lado as 52 linhas dos 25 processos duplicados

library(tidyverse)
library(DT)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── Classifica cada processo duplicado em tipo ───────────────────────────────
# tipo_duplicata e decisao_canonica já vêm do cjpg.rds (criadas no 14)
df_view <- cjpg |>
  filter(tem_multiplas_decisoes) |>
  mutate(data = format(disponibilizacao, "%d/%m/%Y")) |>
  arrange(processo, disponibilizacao) |>
  select(
    processo,
    tipo_duplicata,
    decisao_canonica,
    n_decisoes,
    data,
    cd_doc,
    classe,
    assunto,
    resultado_dispositivo,
    tipo_decisao_cpc,
    favorabilidade,
    valor_indenizacao,
    revelia,
    tem_tutela_deferida,
    relatorio_txt,
    fundamentacao_txt,
    dispositivo_txt
  )

cat("Linhas mostradas:", nrow(df_view), "\n")
cat("Processos:", n_distinct(df_view$processo), "\n")
cat("\nTipos:\n")
print(table(df_view$tipo_duplicata))

# ── Tabela ────────────────────────────────────────────────────────────────────
tabela <- datatable(
  df_view,
  extensions = c("Buttons"),
  filter     = "top",
  rownames   = FALSE,
  escape     = FALSE,
  options    = list(
    pageLength = 50,
    scrollX    = TRUE,
    autoWidth  = FALSE,
    dom        = "Blfrtip",
    buttons    = c("csv", "excel"),
    order      = list(list(0, "asc"), list(3, "asc")),    # processo, data
    columnDefs = list(
      list(width = "500px",
           targets = which(names(df_view) %in% c("julgado","dispositivo_txt")) - 1),
      list(width = "200px", targets = which(names(df_view) == "cd_doc") - 1),
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
    paste0("Processos com múltiplas decisões na base — ",
           n_distinct(df_view$processo), " processos / ",
           nrow(df_view), " linhas")
  )
) |>
  formatStyle(
    "tipo_duplicata",
    backgroundColor = styleEqual(
      c("BUG_cd_doc_igual",
        "MESMA_DATA_cd_doc_diferente",
        "DATAS_DIFERENTES",
        "MERITO_vs_EXECUCAO"),
      c("#f8d7da",                # vermelho (bug)
        "#fff3cd",                # amarelo (suspeito)
        "#d4edda",                # verde (datas reais)
        "#d1ecf1")                # azul (mérito vs execução)
    ),
    fontWeight = "bold"
  ) |>
  formatStyle(
    "decisao_canonica",
    backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
  ) |>
  formatStyle(
    "processo",
    target = "row",
    backgroundColor = styleEqual(
      unique(df_view$processo),
      rep(c("#ffffff", "#f5f5f5"),
          length.out = n_distinct(df_view$processo))
    )
  )

out_html <- normalizePath("analise/duplicatas_1a.html", mustWork = FALSE)
htmlwidgets::saveWidget(tabela, file = out_html, selfcontained = FALSE)
message("Salvo em: ", out_html)
browseURL(out_html)
