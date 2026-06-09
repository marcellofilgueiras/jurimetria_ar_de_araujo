# Projeto: jurimetria_thierry
# Repositório: https://github.com/marcellofilgueiras/jurimetria_ar_de_araujo
# Local: C:\Users\marce\OneDrive\Documents\R2\jurimetria_thierry\

## CONTEXTO
Pesquisa jurimétrica sobre A R de Araújo Comunicações ME (Guia Plus / Lista Regional Brasil).
Empresa aplica "golpe da lista telefônica" — cobra por publicidade não contratada,
protesta títulos sem causa e inscreve consumidores em cadastros de inadimplentes.
Objetivo: fundamentar petição inicial da Sociedade Comercial FAM Ltda vs. A R de Araújo
(ação de inexigibilidade de duplicata + danos morais).

## RESULTADOS OBTIDOS
- 290 processos únicos no TJSP (2017–2026)
- 62 acórdãos (2ª inst.) + 243 sentenças (1ª inst.)
- Empresa é ré em 97,6% das sentenças
- Taxa de vitória do consumidor (1ª inst.): 90,2%
- Indenização mediana: R$ 5.000

## SCRIPTS (rodar nesta ordem)
_helpers.R                → EMPRESA_RGX, CLASSES_MERITO, CLASSES_EXECUCAO (source comum)
01_baixar_cjsg_cjpg.R     → baixa acórdãos e sentenças do TJSP
02_compilar.R             → compila HTMLs em tabelas cjsg.rds / cjpg.rds
03_explorar.R             → análise exploratória (não escreve nada)
04_autenticar.R           → autenticação ESAJ (pede senha interativamente)
04a_setup_outlook.R       → setup OAuth do Outlook (rodar 1x)
04b_setup_credenciais.R   → documentação do fluxo de credenciais
05_baixar_cposg.R         → detalhes 2ª inst. (requer 04 autenticado)
06_baixar_cpopg.R         → detalhes 1ª inst. (requer 04 autenticado)
07_extrair_partes.R       → extrai partes, classifica papel da empresa
08_favorabilidade.R       → dispositivo + papel + favorabilidade
09_enriquecer.R           → flags temáticas, valor, tipo_decisao_cpc, decisão canônica
10_dt_validacao.R         → DT interativa (todas as sentenças, com zebra por processo)
11_graficos.R             → 7 PNGs em analise/graficos/
11b_tempo_tramitacao.R    → tjsp_ler_movimentacao() + coluna tempo_tramitacao_dias
12_precedentes.R          → top 30 precedentes campeões (CSV + DT navegável)
13_relatorio.R            → relatório jurimétrico (.txt) + 2 word clouds (.html)
14_dashboard.Rmd          → dashboard flexdashboard único — fecha o pacote
                            Render: rmarkdown::render("14_dashboard.Rmd",
                                                      output_file="analise/dashboard.html")

## ARQUIVOS DE DADOS
dados/compilados/cjsg.rds        → acórdãos (papel, favorabilidade, flags temáticas)
dados/compilados/cjpg.rds        → sentenças (idem)
dados/partes/partes_todas.rds    → partes com coluna papel (sem papel_araujo.rds separado)
dados/dispositivos/dispositivos_2a.rds
analise/relatorio_juriometrico.docx

## DECISÕES TÉCNICAS IMPORTANTES
- partes_todas.rds já contém coluna papel (não existe papel_araujo.rds)
- EMPRESA_RGX ampliado: "(?i)(A[.\\s]?R[.\\s]*\\s*de\\s+Ara[uú]jo|guia[\\s.]*plus|guia[\\s.]*mais|lista[\\s.]*regional)"
  captura 71 variações do nome (A.R., Ar de Araujo, Guia Plus, Lista Regional etc.)
- tjsp_classificar_sentenca() → classifica 1ª inst. (retorna: procedente/improcedente/parcial/extinto/homologacao)
- tjsp_classificar_recurso()  → classifica 2ª inst. (retorna: provido/improvido/parcial/nao conhecido/prejudicado/extinto)
- extinto + homologacao (papel==re) → consumidor_venceu (art. 924 CPC — empresa pagou)
- Autenticação ESAJ: tjsp_autenticar() não suporta 2FA. Solução atual: cookies manuais.

## PENDÊNCIAS (próximas etapas)
1. Favorabilidade da 2ª instância (script separado a criar)
   Lógica a implementar (salva em comentário no 08_favorabilidade.R):
     requerente=consumidor & provido   → consumidor_venceu
     requerente=consumidor & improvido → empresa_venceu
     requerente=empresa    & provido   → empresa_venceu
     requerente=empresa    & improvido → consumidor_venceu
     "nao conhecido" / "prejudicado"   → verificar caso a caso
   Requer cruzar resultado com tipo_parte (apelante/apelado) para saber quem recorreu.

2. Many-to-many em dispositivos (2ª inst.)
   tjsp_ler_dispositivo() retorna múltiplas linhas por processo
   (acórdão principal + embargos + agravos). Ainda sem solução.

3. Ampliar extração de valores de indenização
   Apenas 6 valores extraídos das ementas. Usar dispositivos completos.

4. Relatório Word (11_relatorio.R)
   officer + flextable instalados. Script criado mas docx ainda básico.
   Formatar melhor para protocolo judicial.

## AUTENTICAÇÃO ESAJ
.Renviron contém: LOGINADV=11184318638 (CPF)
Senha digitada interativamente via getPass — não salva em arquivo.
Se falhar: usar 06c_autenticar_cookies.R (F12 → Application → Cookies → esaj.tjsp.jus.br)
  Cookies necessários: JSESSIONID (/sajcas), K-JSESSIONID-hdhockji (/esaj-layout),
                       K-JSESSIONID-knbbofpc (/cpopg), K-JSESSIONID-knbbphhf (/cposg)

## PACOTE TJSP
remotes::install_github("jjesusfilho/tjsp")  # versão 2.6.0.9000
Funções-chave: tjsp_baixar_cjsg/cjpg, tjsp_baixar_cposg/cpopg,
               tjsp_ler_partes, tjsp_ler_dispositivo,
               tjsp_classificar_sentenca, tjsp_classificar_recurso
