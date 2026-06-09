# ETAPA 10 — Tabela interativa para validação manual dos julgados (1ª instância)
# Abre HTML com DT::datatable — permite leitura do inteiro teor, filtros e Ctrl+F
# Rodar no RStudio: source("10_dt_validacao.R")

library(tidyverse)
library(DT)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg        <- readRDS("dados/compilados/cjpg.rds")
partes_todas <- readRDS("dados/partes/partes_todas.rds")

# ── Monta tabela de visualização ──────────────────────────────────────────────
# Inclui apenas colunas úteis para validação; julgado vai por último (texto longo)
df_view <- cjpg |>
  # mostra TODAS as linhas (inclusive duplicatas) — uso n_decisoes p/ flagar
  mutate(
    ano  = lubridate::year(disponibilizacao),
    data = format(disponibilizacao, "%d/%m/%Y")
  ) |>
  arrange(desc(tem_multiplas_decisoes), processo, data) |>  # agrupa duplicatas no topo
  select(
    processo,
    decisao_canonica,
    tipo_duplicata,
    n_decisoes,
    data,
    cd_doc,
    classe,
    assunto,
    papel,
    resultado_dispositivo,
    tipo_decisao_cpc,
    favorabilidade,
    valor_morais,
    valor_materiais,
    valor_multa,
    tem_tutela_deferida,
    revelia,
    tem_protesto,
    tem_danos,
    tem_duplicata,
    tem_contrato,
    tem_rel_juridica,
    relatorio_txt,
    fundamentacao_txt,
    dispositivo_txt,
  )

# ── Tabela interativa ─────────────────────────────────────────────────────────
tabela <- datatable(
  df_view,
  extensions  = c("Buttons"),          # sem Responsive — todas as colunas sempre visíveis
  filter      = "top",                              # filtro por coluna no topo
  rownames    = FALSE,
  escape      = FALSE,
  options     = list(
    pageLength  = 25,
    scrollX     = TRUE,
    autoWidth   = FALSE,
    dom         = "Blfrtip",
    buttons     = c("csv", "excel"),                # exportar seleção
    columnDefs  = list(
      list(width = "500px", targets = which(names(df_view) %in%
            c("relatorio_txt","fundamentacao_txt","dispositivo_txt")) - 1),
      list(className = "dt-left", targets = "_all")
    ),
    language    = list(
      search      = "Buscar (Ctrl+F nativo ou aqui):",
      lengthMenu  = "Mostrar _MENU_ registros",
      info        = "Mostrando _START_ a _END_ de _TOTAL_ processos",
      paginate    = list(previous = "Anterior", `next` = "Próximo")
    )
  ),
  caption = htmltools::tags$caption(
    style = "caption-side: top; font-size: 1.1em; font-weight: bold;",
    "1ª Instância TJSP — A R de Araújo Comunicações"
  )
) |>
  formatStyle(
    "decisao_canonica",
    backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da")),
    fontWeight      = "bold"
  ) |>
  formatStyle(
    "tipo_duplicata",
    backgroundColor = styleEqual(
      c("UNICO", "BUG_cd_doc_igual", "DATAS_DIFERENTES", "MESMA_DATA_cd_doc_diferente"),
      c("#ffffff", "#f8d7da",        "#d1ecf1",          "#fff3cd")
    )
  ) |>
  formatStyle(
    "tipo_decisao_cpc",
    backgroundColor = styleEqual(
      c("merito_487_I", "homologacao_487_III", "extincao_sem_merito_485",
        "extincao_execucao_924", "execucao_outros", "indefinido", "nulo"),
      c("#d4edda",      "#d1ecf1",            "#fff3cd",
        "#e2d6f0",      "#e9ecef",         "#f8f9fa",     "#f8d7da")
    ),
    fontWeight = "bold"
  ) |>
  formatStyle(
    "favorabilidade",
    backgroundColor = styleEqual(
      c("consumidor_venceu", "empresa_venceu", "indefinido"),
      c("#d4edda",           "#f8d7da",        "#fff3cd")    # verde / vermelho / amarelo
    )
  ) |>
  # Zebra discreta por processo — útil para visualizar duplicatas agrupadas
  formatStyle(
    "processo",
    target = "row",
    backgroundColor = styleEqual(
      unique(df_view$processo),
      rep(c("#ffffff", "#fafafa"),
          length.out = n_distinct(df_view$processo))
    )
  ) |>
  formatStyle(
    "resultado_dispositivo",
    color = styleEqual(
      c("procedente", "parcial", "improcedente", "extinto", "homologacao", "nulo"),
      c("#155724",    "#856404", "#721c24",       "#383d41",  "#0c5460",    "#6c757d")
    ),
    fontWeight = "bold"
  )

# ── Salva e abre no browser ───────────────────────────────────────────────────
out_html <- normalizePath("analise/validacao_julgados_1a.html", mustWork = FALSE)
htmlwidgets::saveWidget(tabela, file = out_html, selfcontained = FALSE)
message("Salvo em: ", out_html)
browseURL(out_html)

tabela
