# ETAPA 9 — Enriquecimento do cjpg (1ª instância)
# Adiciona ao cjpg:
#   flags temáticas (tem_protesto, tem_danos, ...)
#   relatorio_txt, fundamentacao_txt, dispositivo_txt
#   valor_indenizacao, tem_tutela_deferida, revelia
#   tipo_decisao_cpc, tipo_duplicata, decisao_canonica
# Rodar após 08_favorabilidade.R

library(tidyverse)

setwd("C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry")
source("_helpers.R")

cjpg <- readRDS("dados/compilados/cjpg.rds")

# ── 0. FLAGS TEMÁTICAS ────────────────────────────────────────────────────────
# (antes ficavam no script de exploração; centralizadas aqui para evitar
#  dependência de ordem dos scripts)
cjpg <- cjpg |>
  mutate(
    tem_protesto = grepl("protesto indevido|protesto irregular|protesto ilegal|apontamento indevido",
                         julgado, ignore.case = TRUE),
    tem_danos    = grepl("dano(s)? moral(is)?|indeniza",
                         julgado, ignore.case = TRUE),
    tem_duplicata = grepl("duplicata|título(s)? sem causa|título mercantil|duplicata simulada|duplicata fria",
                          julgado, ignore.case = TRUE),
    tem_contrato = grepl("ausência de contrato|inexistência de contrato|contrato não celebrado|sem relação contratual",
                         julgado, ignore.case = TRUE),
    tem_rel_juridica = grepl("relação jurídica inexistente|ausência de relação jurídica|inexistência de relação jurídica",
                             julgado, ignore.case = TRUE)
  )

# ── 1. VALOR DE INDENIZAÇÃO ───────────────────────────────────────────────────
# PROBLEMA: extrair o primeiro R$ do julgado capturava valores do RELATÓRIO
# (ex.: dívida mencionada na inicial), não da CONDENAÇÃO.
#
# SOLUÇÃO: isolar o DISPOSITIVO antes de buscar valores.
# Âncoras típicas de início do dispositivo:
#   "Ante o exposto" / "Diante do exposto" / "Pelo exposto" / "Posto isso" /
#   "Isso posto" / "DISPOSITIVO" / "JULGO PROCEDENTE/IMPROCEDENTE/PARCIALMENTE"
# Âncoras de fim (opcional): P.R.I. / P.R.I.C. / Publique-se / Intimem-se

extrair_dispositivo <- function(txt) {
  if (is.na(txt)) return(NA_character_)

  # Estratégia: PEGAR A ÚLTIMA ocorrência de âncora — sentenças citam
  # jurisprudência na fundamentação que também usa "Ante o exposto",
  # "JULGO PROCEDENTE" etc. O dispositivo real é sempre o último bloco.

  rgx_inicio <- regex(
    "(ante\\s+o\\s+exposto|diante\\s+do\\s+exposto|pelo\\s+exposto|
      posto\\s+isso|isso\\s+posto|do\\s+exposto|
      \\bDISPOSITIVO\\b|
      JULGO\\s+(PROCEDENTE|IMPROCEDENTE|PARCIALMENTE|EXTINT))",
    ignore_case = TRUE, comments = TRUE
  )

  # str_locate_all → todas ocorrências; ficamos com a última
  ocorrencias <- str_locate_all(txt, rgx_inicio)[[1]]
  if (nrow(ocorrencias) == 0) return(NA_character_)
  pos <- ocorrencias[nrow(ocorrencias), 1]      # inicio da última

  trecho <- str_sub(txt, pos)

  # corta no marcador de encerramento, se existir
  fim <- str_locate(trecho, regex(
    "P\\.?\\s?R\\.?\\s?I\\.?\\s?(C\\.?)?|Publique-se|Intimem-se",
    ignore_case = TRUE
  ))[1, 1]
  if (!is.na(fim)) trecho <- str_sub(trecho, 1, fim - 1)

  trecho
}

# Extrai TODOS os R$ do dispositivo e fica com o primeiro vinculado a
# "dano(s) moral(is)" / "condeno" / "indeniza" / "importe de"
# Regex de valor monetário aceita:
#   R$ 1.000,00  (com ponto de milhar e centavos)
#   R$ 1000,00   (sem ponto de milhar — bug do escrivão)
#   R$ 1.000     (sem centavos)
#   R$ 1000      (idem)
# Captura grupos: [valor_inteiro]
RGX_DINHEIRO <- "R\\$\\s?([0-9]+(?:\\.[0-9]{3})*(?:,[0-9]{2})?)"

parse_brl <- function(s) {
  if (is.na(s)) return(NA_real_)
  s |>
    str_remove_all("\\.(?=[0-9]{3})") |>   # tira pontos de milhar
    str_replace(",", ".") |>               # vírgula decimal → ponto
    as.numeric()
}

# Classifica CADA R$ encontrado no dispositivo segundo seu contexto e
# distribui em 3 colunas: valor_morais, valor_materiais, valor_multa.
#
# Para cada R$, olha janela de 120 chars antes + 40 depois e classifica:
#   - DESCARTAR  → R$ próximo de "duplicata", "boleto", "título", "débito",
#                  "levantamento", "depósito de fls", "custas", "honorários",
#                  "UFESPs". Não vai para nenhuma coluna.
#   - MULTA      → "multa", "astreintes", "pena de", "cominatória"
#   - MORAL      → "dano(s) moral(is)", "morais"
#   - MATERIAL   → "dano(s) material(is)", "devolução", "restitu...",
#                  "valor(es) pago(s)", "devolver", "restituir"
#
# Regras dos múltiplos R$ na mesma categoria:
#   - morais  → primeiro encontrado (geralmente é único)
#   - materiais → o MAIOR (sentenças listam várias parcelas)
#   - multa   → primeiro
#
# Retorna lista nomeada (morais, materiais, multa).

RGX_DINHEIRO_LOC <- "R\\$\\s?[0-9]+(?:\\.[0-9]{3})*(?:,[0-9]{2})?"

extrair_valores_dispositivo <- function(disp) {
  vazio <- list(morais = NA_real_, materiais = NA_real_, multa = NA_real_)
  if (is.na(disp) || nchar(disp) == 0) return(vazio)

  locs <- str_locate_all(disp, RGX_DINHEIRO_LOC)[[1]]
  if (nrow(locs) == 0) return(vazio)

  morais <- materiais <- multa <- NA_real_

  for (i in seq_len(nrow(locs))) {
    inicio <- locs[i, 1]; fim <- locs[i, 2]
    raw    <- str_sub(disp, inicio, fim)
    valor  <- parse_brl(str_remove(raw, "R\\$\\s?"))
    if (is.na(valor)) next

    antes  <- str_sub(disp, max(1, inicio - 120), inicio - 1)
    depois <- str_sub(disp, fim + 1, min(nchar(disp), fim + 40))
    ctx    <- paste(antes, depois)

    # 1) DESCARTAR — débitos, títulos, boletos, levantamentos, custas
    if (str_detect(ctx, regex(
      "duplicata|t[íi]tulo(s)?\\s+(protestad|sem\\s+causa|mercantil)|
       boleto|d[ée]bito|valor\\s+protestad|
       levantamento|dep[óo]sito\\s+de\\s+fls|alvar[áa]|
       custas|honor[áa]rios|UFESPs?|ufesp",
      ignore_case = TRUE, comments = TRUE))) {
      next
    }

    # 2) MULTA
    if (str_detect(ctx, regex(
      "multa|astreintes|cominat[óo]ria|pena\\s+de", ignore_case = TRUE))) {
      if (is.na(multa)) multa <- valor
      next
    }

    # 3) MORAL
    if (str_detect(ctx, regex(
      "dano(s)?\\s+moral(is)?|\\bmorais\\b", ignore_case = TRUE))) {
      if (is.na(morais)) morais <- valor
      next
    }

    # 4) MATERIAL / devolução / restituição
    if (str_detect(ctx, regex(
      "dano(s)?\\s+materia(is|l)|devolu[çc][ãa]o|restitu[íi]|
       devolver|restituir|valor(es)?\\s+pago(s)?|reembols",
      ignore_case = TRUE, comments = TRUE))) {
      if (is.na(materiais) || valor > materiais) materiais <- valor
      next
    }

    # 5) Caso o contexto contenha "condeno...indeniza..." sem especificar
    #    morais/materiais, presumimos MORAL (caso comum em sentenças sucintas)
    if (str_detect(ctx, regex(
      "condeno?[^.]*indeniza[çc][ãa]o|a\\s+t[íi]tulo\\s+de\\s+indeniza",
      ignore_case = TRUE))) {
      if (is.na(morais)) morais <- valor
      next
    }

    # 6) FALLBACK GENÉRICO — "condeno/condenar...R$ X" sem categoria explícita
    #    Já passaram pelos descartes (custas/multa/débito/duplicata/etc.).
    #    Quando a sentença só diz "condenar a pagar R$ X" sem dizer
    #    moral/material, no contexto desta empresa (98% danos morais a
    #    consumidores), atribuímos a valor_morais.
    if (str_detect(ctx, regex(
      "condeno?|condenar.{0,40}(a\\s+)?pag",
      ignore_case = TRUE))) {
      if (is.na(morais)) morais <- valor
      next
    }
  }

  list(morais = morais, materiais = materiais, multa = multa)
}

# ── Relatório e Fundamentação ────────────────────────────────────────────────
# Estrutura da sentença:
#   [RELATÓRIO] → "É o relatório." / "Decido." / "Passo a decidir." → [FUNDAMENTAÇÃO]
#   [FUNDAMENTAÇÃO] → âncora de dispositivo (última) → [DISPOSITIVO]
# Juizado: pode começar com "Dispensado o relatório" — nesse caso relatorio_txt = NA

# Marcador de FIM do relatório / INÍCIO da fundamentação
RGX_FIM_RELATORIO <- regex(
  "(é\\s+o\\s+relat[óo]rio\\.?\\s*(decido|passo\\s+a\\s+decidir|fundamento\\s+e\\s+decido)?|
    relatados[,.]?\\s*decido|
    fundamento\\s+e\\s+decido|
    passo\\s+a\\s+decidir|
    dispensado\\s+o\\s+relat[óo]rio)",
  ignore_case = TRUE, comments = TRUE
)

# Marcador de INÍCIO do dispositivo (mesma lógica de extrair_dispositivo:
# pegar a ÚLTIMA ocorrência, ignorando citações na fundamentação)
RGX_INICIO_DISPOSITIVO <- regex(
  "(ante\\s+o\\s+exposto|diante\\s+do\\s+exposto|pelo\\s+exposto|
    posto\\s+isso|isso\\s+posto|do\\s+exposto|
    \\bDISPOSITIVO\\b|
    JULGO\\s+(PROCEDENTE|IMPROCEDENTE|PARCIALMENTE|EXTINT))",
  ignore_case = TRUE, comments = TRUE
)

extrair_relatorio <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  fim <- str_locate(txt, RGX_FIM_RELATORIO)
  if (is.na(fim[1, 1])) return(NA_character_)

  marcador <- str_sub(txt, fim[1, 1], fim[1, 2])
  # se for "Dispensado o relatório", não há relatório
  if (str_detect(marcador, regex("dispensado", ignore_case = TRUE))) {
    return(NA_character_)
  }
  str_sub(txt, 1, fim[1, 1] - 1) |> str_trim()
}

extrair_fundamentacao <- function(txt) {
  if (is.na(txt)) return(NA_character_)

  # início = fim do relatório (se houver) ou começo do texto
  fim_rel <- str_locate(txt, RGX_FIM_RELATORIO)
  inicio <- if (is.na(fim_rel[1, 1])) 1L else (fim_rel[1, 2] + 1L)

  # fim = última âncora de dispositivo
  ocorrencias <- str_locate_all(txt, RGX_INICIO_DISPOSITIVO)[[1]]
  if (nrow(ocorrencias) == 0) return(NA_character_)
  fim_fund <- ocorrencias[nrow(ocorrencias), 1] - 1L

  if (fim_fund <= inicio) return(NA_character_)
  str_sub(txt, inicio, fim_fund) |> str_trim()
}

# ── Decisão canônica ─────────────────────────────────────────────────────────
# CLASSES_MERITO e CLASSES_EXECUCAO definidos em _helpers.R

# ── Tipo de decisão segundo o CPC ────────────────────────────────────────────
# Categorias:
#   merito_487_I            → procedente/improcedente/parcial (mérito do art. 487, I)
#   homologacao_487_III     → acordo, transação, renúncia, reconhecimento (art. 487, III)
#   extincao_sem_merito_485 → sem resolução de mérito (art. 485 — desistência, abandono, etc.)
#   extincao_execucao_924   → satisfação da obrigação na execução (art. 924)
#   execucao_outros         → cumprimento de sentença sem decisão extintiva clara
#   indefinido              → não classificável

classificar_tipo_decisao <- function(classe, resultado, dispositivo) {
  is_execucao <- !is.na(classe) & classe == "Cumprimento de sentença"
  disp <- ifelse(is.na(dispositivo), "", dispositivo)

  # Pistas textuais no dispositivo
  pista_924    <- str_detect(disp, regex(
    "art(igo)?\\.?\\s*924|satisfa[çc][ãa]o\\s+da\\s+obriga[çc][ãa]o|
     pagamento\\s+integral|quita[çc][ãa]o\\s+(da\\s+)?d[ií]vida|
     extin[çc][ãa]o\\s+da\\s+execu[çc][ãa]o",
    ignore_case = TRUE, comments = TRUE))
  pista_485    <- str_detect(disp, regex(
    "art(igo)?\\.?\\s*485|sem\\s+resolu[çc][ãa]o\\s+do\\s+m[ée]rito|
     desist[êe]ncia|abandono\\s+da\\s+causa|
     ileg[íi]timidade|aus[êe]ncia\\s+de\\s+(pressuposto|interesse)",
    ignore_case = TRUE, comments = TRUE))
  pista_487III <- str_detect(disp, regex(
    "art(igo)?\\.?\\s*487[,.\\s]*(inciso)?[\\s]*III|
     homolog[ou]\\s+(o\\s+)?(acordo|a\\s+transa[çc][ãa]o)|
     reconhe[çc]o\\s+(da\\s+proced[êe]ncia|do\\s+pedido)|
     renunci[ao]u?\\s+ao\\s+direito",
    ignore_case = TRUE, comments = TRUE))

  case_when(
    # Execução
    is_execucao & (pista_924 | resultado %in% c("homologacao","extinto")) ~ "extincao_execucao_924",
    is_execucao                                                            ~ "execucao_outros",

    # Pistas textuais explícitas (sobrepõem o classificador)
    pista_487III                                                           ~ "homologacao_487_III",
    pista_485 & !resultado %in% c("procedente","improcedente","parcial")   ~ "extincao_sem_merito_485",

    # Fallback pelo resultado do classificador tjsp
    resultado %in% c("procedente","improcedente","parcial")                ~ "merito_487_I",
    resultado == "homologacao"                                             ~ "homologacao_487_III",
    resultado == "extinto"                                                 ~ "extincao_sem_merito_485",
    resultado == "nulo"                                                    ~ "nulo",
    TRUE                                                                   ~ "indefinido"
  )
}

cjpg <- cjpg |>
  mutate(
    eh_merito = classe %in% CLASSES_MERITO
  ) |>
  group_by(processo) |>
  mutate(
    n_decisoes             = n(),
    tem_multiplas_decisoes = n_decisoes > 1,
    n_cd_doc_unicos        = n_distinct(cd_doc),
    n_datas_unicas         = n_distinct(disponibilizacao),
    n_classes              = n_distinct(classe),
    tem_merito_no_grupo    = any(eh_merito)
  ) |>
  mutate(
    # Tipologia da duplicata
    tipo_duplicata = case_when(
      !tem_multiplas_decisoes                       ~ "UNICO",
      n_cd_doc_unicos == 1                          ~ "BUG_cd_doc_igual",
      n_classes > 1 & tem_merito_no_grupo           ~ "MERITO_vs_EXECUCAO",
      n_datas_unicas > 1                            ~ "DATAS_DIFERENTES",
      TRUE                                          ~ "MESMA_DATA_cd_doc_diferente"
    ),
    # Regra de decisão canônica:
    #   - UNICO                       → canônica
    #   - BUG_cd_doc_igual            → 1ª linha (são iguais)
    #   - MESMA_DATA_cd_doc_diferente → 1ª linha (provável bug)
    #   - MERITO_vs_EXECUCAO          → privilegia MÉRITO (art. 487);
    #                                   entre méritos, a mais recente
    #   - DATAS_DIFERENTES (sem misto)→ mais recente
    #
    # Implementado com rank composto:
    #   rank_merito = 0 se eh_merito (quando grupo tem mérito), senão 1
    #   tie-break   = -data (mais recente primeiro)
    rank_merito = case_when(
      tipo_duplicata == "MERITO_vs_EXECUCAO" & eh_merito ~ 0L,
      tipo_duplicata == "MERITO_vs_EXECUCAO"             ~ 1L,
      TRUE                                               ~ 0L
    ),
    rank_data = case_when(
      tipo_duplicata %in% c("DATAS_DIFERENTES", "MERITO_vs_EXECUCAO") ~
        rank(-as.numeric(disponibilizacao), ties.method = "first"),
      TRUE ~ as.numeric(row_number())
    ),
    ordem_canonica = rank(rank_merito * 1000 + rank_data, ties.method = "first"),
    decisao_canonica = ordem_canonica == 1
  ) |>
  ungroup() |>
  select(-rank_merito, -rank_data, -ordem_canonica, -tem_merito_no_grupo) |>
  mutate(
    relatorio_txt     = map_chr(julgado, extrair_relatorio),
    fundamentacao_txt = map_chr(julgado, extrair_fundamentacao),
    dispositivo_txt   = map_chr(julgado, extrair_dispositivo),
    .valores          = map(dispositivo_txt, extrair_valores_dispositivo),
    valor_morais      = map_dbl(.valores, "morais"),
    valor_materiais   = map_dbl(.valores, "materiais"),
    valor_multa       = map_dbl(.valores, "multa"),
    valor_total       = pmap_dbl(
      list(valor_morais, valor_materiais),
      \(m, mat) {
        s <- sum(c(m, mat), na.rm = TRUE)
        if (s == 0 && is.na(m) && is.na(mat)) NA_real_ else s
      }
    )
  ) |>
  select(-.valores) |>
  mutate(
    tipo_decisao_cpc = classificar_tipo_decisao(
      classe, resultado_dispositivo, dispositivo_txt
    )
  )

cat("Dispositivo localizado em:", sum(!is.na(cjpg$dispositivo_txt)),
    "de", nrow(cjpg), "julgados\n")

cat("\n=== TIPO DE DECISÃO (CPC) ===\n")
print(table(cjpg$tipo_decisao_cpc, useNA = "ifany"))

cat("\n--- Cruzamento tipo_decisao_cpc x classe ---\n")
cjpg |> count(tipo_decisao_cpc, classe) |> arrange(tipo_decisao_cpc, desc(n)) |>
  print(n = Inf)

cat("\n=== DECISÃO CANÔNICA ===\n")
cat("Total de linhas:           ", nrow(cjpg), "\n")
cat("Marcadas como canônica:    ", sum(cjpg$decisao_canonica), "\n")
cat("Marcadas como descartáveis:", sum(!cjpg$decisao_canonica), "\n")
cat("\nQuebra por tipo:\n")
cjpg |>
  count(tipo_duplicata, decisao_canonica) |>
  pivot_wider(names_from = decisao_canonica, values_from = n,
              names_prefix = "canonica_") |>
  print()

cat("\n--- Impacto nas estatísticas (favorabilidade) ---\n")
cat("ANTES (todas as linhas):\n")
print(table(cjpg$favorabilidade, useNA = "ifany"))
cat("\nDEPOIS (só canônicas):\n")
print(table(cjpg$favorabilidade[cjpg$decisao_canonica], useNA = "ifany"))

cat("\n=== PROCESSOS COM MÚLTIPLAS DECISÕES NA BASE ===\n")
multi <- cjpg |>
  filter(tem_multiplas_decisoes) |>
  distinct(processo, n_decisoes) |>
  arrange(desc(n_decisoes))
cat("Processos com >1 linha:", nrow(multi), "\n")
cat("Linhas envolvidas:", sum(multi$n_decisoes), "\n")
print(head(multi, 15))

cat("=== VALORES DE CONDENAÇÃO — 1ª INSTÂNCIA ===\n")
cat("Com valor_morais   :", sum(!is.na(cjpg$valor_morais)),    "\n")
cat("Com valor_materiais:", sum(!is.na(cjpg$valor_materiais)), "\n")
cat("Com valor_multa    :", sum(!is.na(cjpg$valor_multa)),     "\n\n")

cat("--- valor_morais ---\n")
cjpg |>
  filter(!is.na(valor_morais)) |>
  summarise(
    n = n(), min = min(valor_morais),
    p25 = quantile(valor_morais, 0.25),
    mediana = median(valor_morais),
    media = round(mean(valor_morais), 2),
    p75 = quantile(valor_morais, 0.75),
    max = max(valor_morais)
  ) |> print()

cat("\n--- valor_materiais ---\n")
cjpg |>
  filter(!is.na(valor_materiais)) |>
  summarise(
    n = n(), min = min(valor_materiais),
    p25 = quantile(valor_materiais, 0.25),
    mediana = median(valor_materiais),
    media = round(mean(valor_materiais), 2),
    p75 = quantile(valor_materiais, 0.75),
    max = max(valor_materiais)
  ) |> print()

cat("\n=== FAIXAS DE VALOR — dano moral (1ª inst.) ===\n")
cjpg |>
  filter(!is.na(valor_morais)) |>
  mutate(
    faixa = case_when(
      valor_morais <  2000  ~ "até R$ 2.000",
      valor_morais <  5000  ~ "R$ 2.001 a R$ 5.000",
      valor_morais < 10000  ~ "R$ 5.001 a R$ 10.000",
      valor_morais < 20000  ~ "R$ 10.001 a R$ 20.000",
      TRUE                  ~ "acima de R$ 20.000"
    )
  ) |>
  count(faixa, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ── 2. TUTELA / LIMINAR DEFERIDA ──────────────────────────────────────────────
# TRUE se o julgado menciona deferimento de tutela/liminar para sustar protesto
# ou cancelar negativação (pedido típico do caso FAM)

cjpg <- cjpg |>
  mutate(
    tem_tutela_deferida = str_detect(
      julgado,
      regex(
        "defiro\\s+a\\s+tutela|tutela\\s+(de urgência|antecipada|cautelar)?\\s*(deferida|concedida|concedo)|
         liminar\\s+deferida|defiro\\s+a\\s+liminar|
         sustar\\s+o\\s+protesto|cancelar\\s+a\\s+negativação|
         retirar\\s+o\\s+nome|excluir\\s+o\\s+nome",
        ignore_case = TRUE
      )
    )
  )

cat("\n=== TUTELA / LIMINAR DEFERIDA ===\n")
print(table(cjpg$tem_tutela_deferida, useNA = "ifany"))
cat("(% dos processos com tutela deferida mencionada no julgado)\n")
cat(round(100 * mean(cjpg$tem_tutela_deferida, na.rm = TRUE), 1), "%\n")

# ── 3. REVELIA ────────────────────────────────────────────────────────────────
# TRUE se o julgado menciona revelia / ausência de contestação da empresa

cjpg <- cjpg |>
  mutate(
    revelia = str_detect(
      julgado,
      regex(
        "revel(ia|e)?|deixou\\s+de\\s+contestar|ausência\\s+de\\s+contestação|
         não\\s+contestou|sem\\s+contestação|não\\s+apresentou\\s+contestação",
        ignore_case = TRUE
      )
    )
  )

cat("\n=== REVELIA ===\n")
print(table(cjpg$revelia, useNA = "ifany"))
cat("(% dos processos com revelia declarada)\n")
cat(round(100 * mean(cjpg$revelia, na.rm = TRUE), 1), "%\n")

# ── Cruzamento revelia × favorabilidade ──────────────────────────────────────
cat("\n=== REVELIA x FAVORABILIDADE ===\n")
cjpg |>
  filter(duplicado == FALSE) |>
  count(revelia, favorabilidade) |>
  arrange(revelia, favorabilidade) |>
  print()

# ── Salva ─────────────────────────────────────────────────────────────────────
saveRDS(cjpg, "dados/compilados/cjpg.rds")
write.csv(cjpg, "dados/compilados/cjpg.csv", row.names = FALSE, fileEncoding = "UTF-8")

message("\nEtapa 9 concluida. Novas colunas em cjpg: valor_morais, valor_materiais, valor_multa, valor_total, tem_tutela_deferida, revelia, tipo_decisao_cpc, tipo_duplicata, decisao_canonica")
