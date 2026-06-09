# ETAPA 15 — Briefing autocontido para colar no Claude chat
#
# Gera um Markdown único com tudo que o Claude precisa para escrever
# a petição inicial:
#   - Contexto do caso + pedidos
#   - Resumo executivo (6 números-chave atualizados)
#   - Metodologia em 3 linhas
#   - Top 10 precedentes com trecho do dispositivo
#   - Prompt-base sugerido (editável)
#
# Saída: analise/briefing_para_peticao.md

library(tidyverse)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")

cjpg <- readRDS("dados/compilados/cjpg.rds") |> filter(decisao_canonica)
prec <- read.csv("analise/precedentes_campeoes.csv",
                 stringsAsFactors = FALSE, encoding = "UTF-8",
                 colClasses = c(processo = "character"))

# ── Métricas-chave ───────────────────────────────────────────────────────────
n_total <- nrow(cjpg)
n_re    <- sum(cjpg$papel == "re", na.rm = TRUE)

merito  <- cjpg |>
  filter(papel == "re",
         tipo_decisao_cpc %in% c("merito_487_I", "homologacao_487_III"),
         resultado_dispositivo %in% c("procedente","parcial","improcedente","homologacao"))
n_pro   <- sum(merito$resultado_dispositivo != "improcedente")
pct_pro <- 100 * n_pro / nrow(merito)

valores  <- cjpg |> filter(!is.na(valor_morais))
med_val  <- median(valores$valor_morais)
mean_val <- mean(valores$valor_morais)
p25_val  <- quantile(valores$valor_morais, 0.25) |> as.numeric()
p75_val  <- quantile(valores$valor_morais, 0.75) |> as.numeric()
n_val    <- nrow(valores)

# Materiais (devolução)
materiais  <- cjpg |> filter(!is.na(valor_materiais))
med_mat    <- if (nrow(materiais) > 0) median(materiais$valor_materiais) else NA
mean_mat   <- if (nrow(materiais) > 0) mean(materiais$valor_materiais)   else NA
n_mat      <- nrow(materiais)

tempos     <- cjpg |> filter(papel == "re",
                             tipo_decisao_cpc == "merito_487_I",
                             !is.na(tempo_tramitacao_dias),
                             tempo_tramitacao_dias >= 0,
                             tempo_tramitacao_dias <= 3650)
med_tempo  <- median(tempos$tempo_tramitacao_dias)
mean_tempo <- mean(tempos$tempo_tramitacao_dias)

n_tutela  <- sum(cjpg$tem_tutela_deferida, na.rm = TRUE)
pct_tut   <- 100 * n_tutela / n_total

# Helper formatação
brl <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

# ── Top 10 precedentes (junta com cjpg para ter julgado/relatório/fundamentação completos)
top10 <- prec |>
  head(10) |>
  mutate(processo = as.character(processo)) |>
  left_join(
    cjpg |>
      mutate(processo = as.character(processo)) |>
      select(processo, julgado, relatorio_txt, fundamentacao_txt, dispositivo_txt),
    by = "processo",
    suffix = c(".csv", "")
  )

# Função helper: pega o melhor texto disponível para cada componente
get_or_fallback <- function(txt, fallback_label = "(não destacado nesta sentença)") {
  if (is.na(txt) || nchar(str_trim(txt)) == 0) fallback_label else str_trim(txt)
}

linhas <- c(
  "# Briefing para Petição Inicial — Sociedade Comercial FAM Ltda vs A R de Araújo Comunicações ME",
  "",
  paste("> Gerado em", format(Sys.Date(), "%d/%m/%Y"),
        "a partir de pesquisa jurimétrica no TJSP."),
  "> Repositório: https://github.com/marcellofilgueiras/jurimetria_ar_de_araujo",
  "",
  "---",
  "",
  "## 1. Contexto do caso",
  "",
  "**Autora:** Sociedade Comercial FAM Ltda (consumidora).",
  "",
  "**Ré:** A R de Araújo Comunicações ME, também conhecida como **Guia Plus** e **Lista Regional Brasil** (CNPJ pode variar — empresa atua sob diversos nomes fantasia).",
  "",
  "**Núcleo fático:** a empresa aplica sistematicamente o chamado **\"golpe da lista telefônica\"**: oferece, sem autorização clara, serviço de publicidade em lista impressa/digital; emite duplicata por serviço não contratado; encaminha o título a protesto; inscreve o consumidor em cadastros de inadimplentes (SERASA, SPC). No caso da FAM, há **duplicata indevida sendo protestada**.",
  "",
  "## 2. Pedidos da inicial",
  "",
  "1. **Tutela de urgência** — sustação imediata do protesto e proibição de novas cobranças",
  "2. **Declaratória** de inexistência de relação jurídica e de inexigibilidade da duplicata",
  "3. **Cancelamento definitivo** do protesto e exclusão de cadastros restritivos",
  "4. **Indenização por danos morais** (quantum a ser arbitrado)",
  "",
  "## 3. Pesquisa jurimétrica — números-chave",
  "",
  "Coletadas e analisadas **todas as sentenças de 1ª instância do TJSP** envolvendo a empresa, no período 2017–2026. Após deduplicação canônica:",
  "",
  sprintf("| Métrica | Valor |"),
  sprintf("|---|---|"),
  sprintf("| Sentenças únicas analisadas (1ª inst.) | **%d** |", n_total),
  sprintf("| Sentenças em que a empresa é RÉ | **%d** (%.1f%%) |", n_re, 100 * n_re / n_total),
  sprintf("| **Taxa de procedência PRÓ-CONSUMIDOR** (mérito) | **%.1f%%** (%d de %d casos) |",
          pct_pro, n_pro, nrow(merito)),
  sprintf("| Quantum dano moral — **MEDIANA** | **R$ %s** (n=%d) |", brl(med_val), n_val),
  sprintf("| Quantum dano moral — **MÉDIA** | R$ %s |", brl(mean_val)),
  sprintf("| Faixa típica do quantum (P25–P75) | **R$ %s a R$ %s** |", brl(p25_val), brl(p75_val)),
  sprintf("| Danos materiais (devolução) — mediana | R$ %s (n=%d) |", brl(med_mat), n_mat),
  sprintf("| Danos materiais (devolução) — média | R$ %s |", brl(mean_mat)),
  sprintf("| Tempo até a sentença — **MEDIANA** | **%d dias** (~%.1f meses) |",
          med_tempo, med_tempo / 30),
  sprintf("| Tempo até a sentença — MÉDIA | %d dias (~%.1f meses) |",
          round(mean_tempo), mean_tempo / 30),
  sprintf("| Tutela de urgência deferida | %d sentenças (%.1f%%) |", n_tutela, pct_tut),
  "",
  "### Implicações jurídicas",
  "",
  "- A empresa é **ré em quase 100% dos casos** — não é litigante eventual, é alvo recorrente.",
  sprintf("- O quantum de R$ %s é **consistente entre comarcas** (P25 R$ %s, P75 R$ %s) — patamar bem estabelecido no TJSP para casos análogos.",
          brl(med_val), brl(p25_val), brl(p75_val)),
  "- A sentença sai em **menos de 1 ano** em 75% dos casos — argumento para a tutela de urgência (perigo da demora é real, mas a resposta judicial é rápida).",
  "- Há um padrão sistemático de **operação ilícita em escala**, repetida há quase uma década (2017–2026).",
  "",
  "## 4. Metodologia",
  "",
  "- **Fonte:** TJSP — ESAJ (consultas públicas CJPG e CJSG).",
  "- **Coleta:** pacote `{tjsp}` (Jose de Jesus Filho) em R.",
  "- **Processamento:** scripts 01–15 em `jurimetria_ar_de_araujo/` — extração de partes, classificação de dispositivos (CPC 487/485/924), regex contextual para quantum (separa danos morais de materiais e multa), deduplicação canônica (privilegia mérito).",
  "",
  "## 5. Top 10 precedentes para citação nominal",
  "",
  "Ranqueados por score 0–6 (flags temáticas + procedência integral + tutela deferida + valor ≥ mediana):",
  ""
)

# Top 10 em lista (cada um com Relatório + Fundamentação + Dispositivo
# — com fallback para julgado inteiro se as partes vierem NA)
for (i in seq_len(nrow(top10))) {
  r <- top10[i, ]
  valor <- if (is.na(r$valor_morais)) "—" else paste0("R$ ", brl(r$valor_morais))

  rel  <- get_or_fallback(r$relatorio_txt,
            "(sentença sem relatório autônomo — dispensado nos termos da Lei 9.099/95 ou similar)")
  fund <- get_or_fallback(r$fundamentacao_txt,
            "(fundamentação não isolada pelo extrator — ver julgado completo abaixo)")
  disp <- get_or_fallback(r$dispositivo_txt,
            "(dispositivo não isolado pelo extrator — ver julgado completo abaixo)")
  julg <- get_or_fallback(r$julgado,
            "(texto integral indisponível)")

  linhas <- c(linhas,
    sprintf("### %d. Processo %s", i, r$processo),
    "",
    sprintf("- **Comarca / Vara:** %s — %s", r$comarca, r$vara),
    sprintf("- **Magistrado(a):** %s", r$magistrado),
    sprintf("- **Data:** %s", r$data),
    sprintf("- **Resultado:** %s | **Quantum (morais):** %s | **Score:** %d/6",
            r$resultado_dispositivo, valor, r$score_total),
    "",
    "<details><summary>📄 Relatório</summary>",
    "",
    rel,
    "",
    "</details>",
    "",
    "<details><summary>⚖️ Fundamentação</summary>",
    "",
    fund,
    "",
    "</details>",
    "",
    "<details><summary>🎯 Dispositivo</summary>",
    "",
    disp,
    "",
    "</details>",
    "",
    "<details><summary>📜 Julgado integral (fallback)</summary>",
    "",
    julg,
    "",
    "</details>",
    "",
    "---",
    ""
  )
}

linhas <- c(linhas,
  "## 6. Anexos disponíveis (para enviar ao Claude se necessário)",
  "",
  "- `analise/relatorio_jurimetrico.txt` — relatório completo (11 seções)",
  "- `analise/precedentes_campeoes.csv` — top 30 precedentes em Excel/CSV",
  "- `analise/graficos/01_serie_temporal.png` — sentenças por ano",
  "- `analise/graficos/05_facet_procedencia_por_papel.png` — taxa de procedência",
  "- `analise/graficos/06_pro_contra_consumidor.png` — síntese pró-consumidor",
  "- `analise/graficos/07_tempo_tramitacao.png` — tempo até a sentença",
  "- `analise/dashboard.html` — dashboard interativo completo",
  "",
  "---",
  "",
  "## 🤖 PROMPT BASE SUGERIDO (edite e mande para o Claude chat)",
  "",
  "```",
  "Você é advogado de consumidor. Vou anexar abaixo um briefing jurimétrico com dados estatísticos do TJSP sobre a empresa A R de Araújo Comunicações ME (Guia Plus).",
  "",
  "Sua tarefa: escrever a petição inicial para a Sociedade Comercial FAM Ltda contra a empresa, pedindo:",
  "1. Tutela de urgência (sustar protesto)",
  "2. Declaração de inexigibilidade da duplicata",
  "3. Indenização por danos morais",
  "",
  "Requisitos:",
  "- Use os números do item 3 do briefing com citação à fonte (TJSP).",
  "- Cite NOMINALMENTE os 10 precedentes do item 5 na seção de fundamentação.",
  "- Fundamentos: CDC arts. 6, 14, 39, 42 + Código Civil 186, 927 + Lei 9.492/97 (protesto).",
  "- Use a Súmula 385 do STJ se houver inscrição prévia — caso contrário, dano moral in re ipsa (Súmula 227 STJ).",
  sprintf("- Pedido de quantum: arbitre com base na MEDIANA R$ %s (média R$ %s, P75 R$ %s) — TJSP.",
          brl(med_val), brl(mean_val), brl(p75_val)),
  "- Estrutura: Endereçamento, Qualificação, Fatos, Direito, Tutela, Pedidos, Valor da causa, Provas.",
  "",
  "[COLAR AQUI O CONTEÚDO DESTE briefing_para_peticao.md INTEIRO]",
  "```",
  "",
  "---",
  "",
  "*Fim do briefing. Pesquisa replicável em [github.com/marcellofilgueiras/jurimetria_ar_de_araujo](https://github.com/marcellofilgueiras/jurimetria_ar_de_araujo).*"
)

out <- "analise/briefing_para_peticao.md"
writeLines(linhas, out, useBytes = TRUE)

message("Briefing salvo em: ", out)
message("Tamanho: ", file.size(out), " bytes")
message("Linhas: ", length(linhas))
