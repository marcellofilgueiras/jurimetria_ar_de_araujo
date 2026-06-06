# ETAPA 6b — Setup de Credenciais Seguras
#
# Este script explica como manter suas credenciais ESAJ de forma segura.
# Não precisa rodar — é só referência/documentação.

# ────────────────────────────────────────────────────────────────────────────
# ARQUITETURA DE SEGURANÇA
# ────────────────────────────────────────────────────────────────────────────

# 1. .Renviron → armazena LOGINADV (CPF) — PÚBLICO, OK no GitHub
#    - Arquivo: C:\Users\marce\OneDrive\Documents\R2\jurimetria_thierry\.Renviron
#    - Conteúdo: LOGINADV=11184318638
#    - Risco: Nenhum (CPF é semi-público)
#    - Status: ✅ Seguro, pode commitar

# 2. PASSWORDADV → digitada interativamente no script 06_autenticar.R
#    - Nunca salva em arquivo
#    - Armazenada só na memória da sessão R
#    - Quando fecha R: memória é limpa automaticamente
#    - Status: ✅ Máxima segurança

# ────────────────────────────────────────────────────────────────────────────
# FLUXO DE AUTENTICAÇÃO (a cada nova sessão R)
# ────────────────────────────────────────────────────────────────────────────

# 1. Abra RStudio (ou R)
# 2. Rode este comando:
#    source("06_autenticar.R")
# 3. Script pede: "Digite sua senha ESAJ (CPF: 11184318638): "
# 4. Digite sua senha (não aparece na tela — está protegida)
# 5. Script autentica automaticamente usando:
#    - Microsoft365R: busca token 2FA no email (Gmail/Outlook)
#    - ESAJ: faz login usando CPF + senha que você digitou
# 6. Se sucesso: ✓ mensagem de confirmação
# 7. Pode agora rodar 07_baixar_cposg.R e 08_baixar_cpopg.R

# ────────────────────────────────────────────────────────────────────────────
# SEGURANÇA: Por que é seguro?
# ────────────────────────────────────────────────────────────────────────────

# ✅ Não fica em arquivo
#    - .Renviron não contém senha
#    - Nenhum arquivo .R contém senha
#    - Nenhum arquivo .rds/.csv contém senha

# ✅ Não vai para GitHub
#    - .gitignore já protege dados sensíveis
#    - Você pode agora fazer `git add .` sem risco

# ✅ Memória volátil
#    - Senha existe só durante a sessão R
#    - Quando fecha R: apagada da memória automaticamente
#    - Nenhum trace em disco

# ✅ Isolada por usuário Windows
#    - Outro usuário do PC não consegue acessar a memória do seu R
#    - Credenciais ficam privadas ao seu login Windows

# ────────────────────────────────────────────────────────────────────────────
# SE SUA SENHA MUDAR NO ESAJ
# ────────────────────────────────────────────────────────────────────────────

# Não precisa fazer nada!
# Na próxima vez que rodar 06_autenticar.R, ele pede a nova senha interativamente.
# Nenhum arquivo precisa ser atualizado.

# ────────────────────────────────────────────────────────────────────────────
# TROUBLESHOOTING
# ────────────────────────────────────────────────────────────────────────────

# P: "O script não pede senha"
# R: Verifique que LOGINADV está em .Renviron
#    Rode: Sys.getenv("LOGINADV")

# P: "Erro ao autenticar"
# R: Verifique que rodou 06a_setup_outlook.R antes
#    Verifique que seu email Outlook (marcellofilgueiras@outlook.com)
#    tem acesso aos tokens ESAJ

# P: "Quero ver a senha sendo digitada?"
# R: Não recomendado por segurança. getPass::getPass() esconde por motivo.
#    Mas você pode modificar a linha:
#      PASSWORDADV <- getPass::getPass(...)
#    para:
#      PASSWORDADV <- readline("Digite sua senha: ")
#    ⚠️ Isso exibe a senha — use só em ambiente seguro

message(
  "\nℹ️  Este é um arquivo de referência.\n",
  "Para autenticar, rode: source('06_autenticar.R')\n"
)
