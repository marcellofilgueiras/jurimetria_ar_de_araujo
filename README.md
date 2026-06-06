# Jurimetria — A R de Araújo Comunicações ME

Pesquisa jurimétrica sobre processos judiciais envolvendo a empresa **A R de Araújo Comunicações ME** (também conhecida como *Guia Plus* e *Lista Regional Brasil*) no Tribunal de Justiça do Estado de São Paulo (TJSP).

## Contexto

A empresa é investigada pelo chamado **"golpe da lista telefônica"**: cobra por serviços de publicidade não contratados, protesta títulos sem causa e inscreve consumidores em cadastros de inadimplentes (Serasa/SPC).

Este repositório reúne os scripts de coleta, processamento e análise de **290 processos judiciais** localizados no TJSP entre 2017 e 2026, com o objetivo de construir base empírica para fundamentar petição inicial de ação de declaração de inexigibilidade de duplicata e indenização por danos morais.

---

## Resultados Principais

| Indicador | Resultado |
|-----------|-----------|
| Processos únicos coletados | 290 |
| Acórdãos (2ª instância) | 62 |
| Sentenças (1ª instância) | 243 |
| Período | 2017–2026 |
| Empresa como ré | 97,6% das sentenças |
| **Taxa de vitória do consumidor (1ª inst.)** | **90,2%** |
| Indenização — mediana | R$ 5.000 |
| Comarcas identificadas | 10+ |

---

## Estrutura do Projeto

```
jurimetria_thierry/
│
├── dados/
│   ├── cjsg_html/          # HTMLs dos acórdãos (2ª inst.) — não versionado
│   ├── cjpg_html/          # HTMLs das sentenças (1ª inst.) — não versionado
│   ├── cposg_html/         # HTMLs dos processos de 2ª inst. — não versionado
│   ├── cpopg_html/         # HTMLs dos processos de 1ª inst. — não versionado
│   ├── compilados/         # Tabelas estruturadas (cjsg.rds, cjpg.rds)
│   ├── partes/             # Partes processuais (partes_todas.rds)
│   └── dispositivos/       # Dispositivos dos acórdãos
│
├── analise/                # Outputs da análise
│   └── relatorio_jurimetrico.docx
│
├── 01_baixar_cjsg_cjpg.R   # Download de acórdãos e sentenças
├── 03_compilar.R           # Compilação dos HTMLs em tabelas
├── 04_analisar.R           # Análise exploratória + flags temáticas
├── 06_autenticar.R         # Autenticação no ESAJ
├── 06c_autenticar_cookies.R# Autenticação via cookies (contingência)
├── 07_baixar_cposg.R       # Download de detalhes da 2ª instância
├── 08_baixar_cpopg.R       # Download de detalhes da 1ª instância
├── 09_extrair_partes.R     # Extração e classificação das partes
├── 10_11_favorabilidade.R  # Dispositivo + classificação de favorabilidade
├── 12_relatorio.R          # Relatório jurimétrico final
│
├── .Renviron               # Credenciais — NÃO versionado
├── .gitignore
└── jurimetria_thierry.Rproj
```

---

## Metodologia

### Coleta
- **Fonte**: TJSP/ESAJ — sistema público de consulta de julgados
- **Pacote R**: [`tjsp`](https://github.com/jjesusfilho/tjsp) (jjesusfilho)
- **Termos de busca**: `"A R de Araújo Comunicações"` (busca por frase exata, `aspas = TRUE`)
- **Consultas utilizadas**: CJSG (acórdãos), CJPG (sentenças), CPOSG e CPOPG (detalhes processuais)

### Processamento
- Extração de partes processuais via `tjsp_ler_partes()`
- Classificação do papel da empresa: **ré** (Requerida/Apelada) ou **autora** (Requerente/Apelante)
- Classificação dos resultados:
  - 1ª instância: `tjsp_classificar_sentenca()` → procedente / improcedente / parcial / extinto / homologação
  - 2ª instância: `tjsp_classificar_recurso()` → provido / improvido / não conhecido / prejudicado (⚠️ pendente de validação)

### Flags Temáticas
Cada julgado recebeu colunas booleanas para filtros rápidos:

| Flag | Detecta |
|------|---------|
| `tem_protesto` | protesto indevido / apontamento indevido |
| `tem_danos` | danos morais / indenização |
| `tem_duplicata` | duplicata / título sem causa |
| `tem_contrato` | ausência / inexistência de contrato |
| `tem_rel_juridica` | relação jurídica inexistente |

### Lógica de Favorabilidade (1ª instância)

| Papel da empresa | Resultado | Favorabilidade |
|-----------------|-----------|----------------|
| Ré | procedente / parcial | consumidor_venceu |
| Ré | improcedente | empresa_venceu |
| Ré | extinto (art. 924 CPC) | consumidor_venceu |
| Ré | homologação | consumidor_venceu |
| Autora | procedente | empresa_venceu |
| Autora | improcedente / parcial | consumidor_venceu |

---

## Como Reproduzir

### Pré-requisitos

```r
install.packages(c("tidyverse", "officer", "flextable"))
remotes::install_github("jjesusfilho/tjsp")
```

### Variáveis de Ambiente

Crie um arquivo `.Renviron` na raiz do projeto com seu CPF de cadastro no ESAJ:

```
LOGINADV=seu_cpf_aqui
```

A senha é solicitada interativamente ao rodar `06_autenticar.R` — nunca é salva em arquivo.

### Execução

Execute os scripts na ordem numérica a partir do RStudio (abra `jurimetria_thierry.Rproj`):

```r
source("01_baixar_cjsg_cjpg.R")   # ~10 min
source("03_compilar.R")
source("04_analisar.R")
# Autenticar no ESAJ antes dos próximos passos:
source("06_autenticar.R")          # senha solicitada interativamente
source("07_baixar_cposg.R")        # ~5 min
source("08_baixar_cpopg.R")        # ~4 min
source("09_extrair_partes.R")
source("10_11_favorabilidade.R")
source("12_relatorio.R")
```

> ⚠️ **Atenção**: o ESAJ exige autenticação em duas etapas (2FA por e-mail). Caso `06_autenticar.R` falhe, use `06c_autenticar_cookies.R` como contingência (requer cópia manual dos cookies do navegador).

---

## Pendências

- [ ] **Etapa 13**: classificação de favorabilidade da 2ª instância (`provido`/`improvido` — requer cruzar com quem recorreu)
- [ ] **Valores de indenização**: ampliar extração para além das ementas (usar dispositivos completos)
- [ ] **Autenticação**: solução robusta para o 2FA do ESAJ (issue aberta no pacote `tjsp` ou RSelenium)

---

## Dados Sensíveis e Privacidade

Os dados coletados são **públicos** — disponíveis no portal ESAJ do TJSP. Nenhum dado pessoal sensível está versionado neste repositório. O arquivo `.Renviron` (credenciais de acesso) está protegido pelo `.gitignore`.

---

## Referências

- TJSP — [esaj.tjsp.jus.br](https://esaj.tjsp.jus.br)
- Pacote `tjsp`: [github.com/jjesusfilho/tjsp](https://github.com/jjesusfilho/tjsp)
- Azevedo, R. B., & Castelar Pinheiro, A. (2020). *Introdução à Jurimetria*. Saraiva.

---

*Pesquisa conduzida com fins jurídicos — dados de acesso público do TJSP.*
