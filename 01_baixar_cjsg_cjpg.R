# ETAPA 1 — Download de acórdãos e decisões monocráticas (2ª instância)
# Fonte: TJSP / ESAJ — cjsg (Consulta de Julgados de Segundo Grau)
# Empresa pesquisada: A R de Araújo Comunicações ME

library(tjsp)

# ── Acórdãos ────────────────────────────────────────────────────────────────


tjsp_baixar_cjsg(
  livre     = "A R de Araújo Comunicações",
  aspas     = TRUE,
  tipo      = "A",
  diretorio =  "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cjsg_html"
)



# Sentenças ---------------------------------------------------------------


# ETAPA 2 — Download de sentenças de 1ª instância
# Fonte: TJSP / ESAJ — cjpg (Consulta de Julgados de Primeiro Grau)
# Empresa pesquisada: A R de Araújo Comunicações ME


DIR_CJPG <- "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cjpg_html"

message("Baixando sentenças de 1ª instância (cjpg) ...")

tjsp_baixar_cjpg(
  livre     = "A R de Araújo Comunicações",
  aspas     = TRUE,
  diretorio = "C:/Users/marce/OneDrive/Documents/R2/jurimetria_thierry/dados/cjpg_html"
)
