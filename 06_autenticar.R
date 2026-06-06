# ETAPA 6 — Autenticação no ESAJ (login 2FA via Outlook pessoal)
#
# Pré-requisitos (rodar UMA VEZ, na ordem):
#   1) 06a_setup_outlook.R  → autoriza Microsoft365R a ler seu email
#   2) .Renviron contém LOGINADV (CPF) — PASSWORDADV é digitada aqui
#
# Cada nova sessão R precisa rodar este script antes de 07/08.
# ⚠️ Este script PEDE SUA SENHA INTERATIVAMENTE — ela não fica salva em arquivo

library(tjsp)

# ── Verifica se getPass está disponível ──────────────────────────────────────
if (!requireNamespace("getPass", quietly = TRUE)) {
  install.packages("getPass")
}
library(getPass)

# ── Lê CPF do .Renviron (seguro — não contém senha) ────────────────────────
LOGINADV <- Sys.getenv("LOGINADV")
stopifnot(
  "LOGINADV não encontrado no .Renviron" = LOGINADV != ""
)

# ── Pede SENHA interativamente (fica só na memória, não salva em arquivo) ────
cat("\n┌─────────────────────────────────────────────────────────────┐\n")
cat("│ AUTENTICAÇÃO ESAJ                                           │\n")
cat("│                                                             │\n")
cat("│ Sua senha será digitada apenas nesta sessão R              │\n")
cat("│ e não será salva em arquivo nenhum.                        │\n")
cat("└─────────────────────────────────────────────────────────────┘\n\n")

PASSWORDADV <- getPass::getPass(
  msg = sprintf("Digite sua senha ESAJ (CPF: %s): ", LOGINADV)
)

# ── Armazena temporariamente na memória (não em disco) ──────────────────────
Sys.setenv(PASSWORDADV = PASSWORDADV)

# ── Autentica no ESAJ ────────────────────────────────────────────────────────

tjsp_autenticar(login = LOGINADV,
                password = PASSWORDADV,
                email_provider= "outlook",
                outlook = "personal"
                )



# ✓ Você pode agora rodar:\n")
# - 07_baixar_cposg.R  (2ª instância, ~5 min)\n")
# - 08_baixar_cpopg.R  (1ª instância, ~4 min)\n\n")
