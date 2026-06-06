# ETAPA 6a — Setup ÚNICO do OAuth do Outlook pessoal (rodar 1x interativamente)
#
# ⚠️ Este script abre o navegador. Rode no RStudio (não no terminal) e:
#   1) Faça login no Outlook pessoal (hotmail/outlook/live)
#   2) Autorize o aplicativo "Microsoft365R" a ler seus emails
#   3) O token fica cacheado em ~/AzureR/ — não precisa repetir
#
# Depois disso, 06_autenticar.R encontra o token automaticamente.

if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
  install.packages("Microsoft365R")
}

library(Microsoft365R)

# Triggers OAuth flow no navegador:
outlook <- get_personal_outlook()

# Testa acesso à caixa de entrada
inbox <- outlook$get_inbox()
emails_recentes <- inbox$list_emails(n = 3)

cat("\n✓ Outlook conectado com sucesso.\n")
cat("Últimos 3 emails na sua inbox:\n")
print(emails_recentes)

cat("\nVocê já pode rodar 06_autenticar.R.\n")
