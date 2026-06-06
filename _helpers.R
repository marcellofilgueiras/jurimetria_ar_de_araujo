# Helpers compartilhados entre scripts
# Carregar com: source("_helpers.R")

# ── Regex da empresa A R de Araújo ───────────────────────────────────────────
# Captura: A.R. de Araujo / Ar de Araújo / AR de Araujo /
#          Guia Plus / Guia Mais / Lista Regional (71 variações observadas)
EMPRESA_RGX <- "(?i)(A[.\\s]?R[.\\s]*\\s*de\\s+Ara[uú]jo|guia[\\s.]*plus|guia[\\s.]*mais|lista[\\s.]*regional)"

# ── Classes de MÉRITO (art. 487 CPC — processo de conhecimento) ─────────────
CLASSES_MERITO <- c(
  "Procedimento Comum Cível",
  "Procedimento do Juizado Especial Cível",
  "Reclamação Pré-processual",
  "Tutela Antecipada Antecedente",
  "Tutela Cautelar Antecedente",
  "Petição Cível",
  "Protesto",
  "Homologação da Transação Extrajudicial"
)

# ── Classes de EXECUÇÃO / CUMPRIMENTO (art. 924 CPC) ────────────────────────
CLASSES_EXECUCAO <- c(
  "Cumprimento de sentença",
  "Embargos de Terceiro Cível",
  "Incidente de Desconsideração de Personalidade Jurídica"
)
