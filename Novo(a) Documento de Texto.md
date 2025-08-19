# PIPELINE — Análise AVC (DataSUS) em R


## 0) Premissas (Windows)

- R ≥ 4.3 (RStudio opcional)  
---

## 1) Estrutura de pastas (idempotente)

**PowerShell** (copie e cole):

```powershell
$WS="D:/workspace"
$PROJ="$WS/avc_datasus"
$DATA="$WS/data/datasus"
$OUT ="$PROJ/results"
$FIG ="$OUT/figs"
$LOG ="$PROJ/logs"
$RPT ="$OUT/relatorios"
$APP ="$PROJ/dashboard"

mkdir $PROJ,$OUT,$FIG,$LOG,$RPT,$APP -Force | Out-Null
```

---

## 2) Bootstrap de ambiente em R (pacotes + renv opcional)

**R** (Console ou `Rscript`):

```r
# ── Config ─────────────────────────────────────────────────────────────────────
WORKSPACE <- "D:/workspace"
PROJ_DIR  <- file.path(WORKSPACE, "avc_datasus")
DATA_DIR  <- file.path(WORKSPACE, "data", "datasus")
OUT_DIR   <- file.path(PROJ_DIR, "results")
FIG_DIR   <- file.path(OUT_DIR, "figs")
LOG_DIR   <- file.path(PROJ_DIR, "logs")
RPT_DIR   <- file.path(OUT_DIR, "relatorios")
APP_DIR   <- file.path(PROJ_DIR, "dashboard")

dirs <- c(PROJ_DIR, OUT_DIR, FIG_DIR, LOG_DIR, RPT_DIR, APP_DIR)
invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))

# ── Pacotes ────────────────────────────────────────────────────────────────────
ensure <- function(pkgs){
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(to_install)) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure(c(
  "tidyverse","readr","readxl","arrow","janitor","lubridate","stringr",
  "DBI","fst","data.table",
  "gt","gtsummary","knitr","rmarkdown",
  "ggplot2","plotly",
  "glue","here","fs",
  "logger","cli",
  "shiny"           # dashboard (opcional)
))

# (Opcional) Reprodutibilidade total com renv
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists(file.path(PROJ_DIR,"renv.lock"))) {
  renv::init(project = PROJ_DIR, bare = TRUE)
  renv::snapshot(project = PROJ_DIR, prompt = FALSE)
}
```

---

## 3) Runner canônico (`run.R`)

Crie **`D:/workspace/avc_datasus/run.R`**:

```r
# run.R — executor canônico do projeto
options(warn = 1)

# Carrega o script analítico (não altere a lógica dele aqui)
source("D:/workspace/analise_avc_datasus.R", encoding = "UTF-8")

WORKSPACE <- "D:/workspace"
PROJ_DIR  <- file.path(WORKSPACE, "avc_datasus")
DATA_DIR  <- file.path(WORKSPACE, "data", "datasus")
OUT_DIR   <- file.path(PROJ_DIR, "results")
FIG_DIR   <- file.path(OUT_DIR, "figs")
LOG_DIR   <- file.path(PROJ_DIR, "logs")
RPT_DIR   <- file.path(OUT_DIR, "relatorios")
APP_DIR   <- file.path(PROJ_DIR, "dashboard")
for(d in c(OUT_DIR, FIG_DIR, LOG_DIR, RPT_DIR, APP_DIR)) dir.create(d, TRUE, TRUE)

# Log para arquivo + console
logfile <- file.path(LOG_DIR, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
con <- file(logfile, open = "wt")
sink(con, type = "output"); sink(con, type = "message")
on.exit({ sink(type = "message"); sink(type = "output"); close(con) }, add = TRUE)

cli::cli_h1("Análise AVC — DataSUS")
logger::log_info("Workspace: {WORKSPACE}")
logger::log_info("Dados em: {DATA_DIR}")
logger::log_info("Resultados: {OUT_DIR}")

stopifnot(dir.exists(DATA_DIR))   # falha cedo

# === Chamadas esperadas ao seu script analítico ===
df  <- carregar_datasus(DATA_DIR)
validar_df(df)                   # gate mínimo
res <- pipeline_avc(df)

salvar_figuras(res, FIG_DIR)
salvar_relatorio_md(res, RPT_DIR)

# Renderiza relatório RMarkdown (se existir)
rmd <- file.path(PROJ_DIR, "relatorio_avc.Rmd")
if (file.exists(rmd)) {
  rmarkdown::render(
    rmd,
    output_format = "html_document",
    output_file = file.path(RPT_DIR, "relatorio_avc.html"),
    params = list(data_dir = DATA_DIR, fig_dir = FIG_DIR, out_dir = OUT_DIR)
  )
  logger::log_success("Relatório gerado em {file.path(RPT_DIR,'relatorio_avc.html')}")
} else {
  logger::log_warn("relatorio_avc.Rmd não encontrado. Pulei renderização.")
}

logger::log_success("Execução concluída sem erros.")
```

> **Regra:** `run.R` só orquestra. Não codifique lógica analítica aqui.

---

## 4) Contrato mínimo no `analise_avc_datasus.R`

No topo do seu script, exponha funções esperadas:

```r
# analise_avc_datasus.R — contrato de funções

carregar_datasus <- function(data_dir){
  # 1) Carregar (CSV/DBF/Parquet)
  # 2) Limpar (janitor::clean_names)
  # 3) Tipos (as.numeric/as.integer/lubridate)
  # 4) Retornar tibble/data.table
  tibble::tibble()  # Substitua pela sua implementação real
}

validar_df <- function(df){
  req_cols <- c("ano","uf")  # ajuste às suas colunas
  stopifnot(nrow(df) > 100)  # gate mínimo
  stopifnot(all(req_cols %in% names(df)))
  invisible(TRUE)
}

pipeline_avc <- function(df){
  # EDA -> derivação -> agregações -> indicadores (ex.: taxas/100k, letalidade)
  # gráficos ggplot2; tabelas gt/gtsummary
  list(
    tabela_resumo = tibble::tibble(),
    fig_taxa_uf   = ggplot2::ggplot()
  )
}

salvar_figuras <- function(obj, fig_dir){
  if(inherits(obj$fig_taxa_uf, "ggplot")){
    ggplot2::ggsave(
      filename = file.path(fig_dir, "fig_taxa_uf.png"),
      plot = obj$fig_taxa_uf, dpi = 300, width = 10, height = 6
    )
  }
  invisible(TRUE)
}

salvar_relatorio_md <- function(obj, rpt_dir){
  md <- c(
    "# Sumário — AVC (DataSUS)",
    "",
    "- Período analisado: <preencher>",
    "- Nível geográfico: UF/Município",
    "- Métricas: incidência, letalidade, custos, tempo médio de internação",
    "",
    "## Tabela-Resumo",
    "",
    "*(insira snapshot da tabela ou exporte .csv)*"
  )
  readr::write_lines(md, file.path(rpt_dir, "sumario.md"))
  invisible(TRUE)
}

# **Atenção:** dplyr::across() deve estar **dentro** de mutate()/summarise()/filter() etc.
```

---

## 5) Relatório `relatorio_avc.Rmd` (opcional, recomendado)

Crie **`D:/workspace/avc_datasus/relatorio_avc.Rmd`**:

```yaml
---
title: "Relatório — AVC (DataSUS)"
output: html_document
params:
  data_dir: "D:/workspace/data/datasus"
  fig_dir:  "D:/workspace/avc_datasus/results/figs"
  out_dir:  "D:/workspace/avc_datasus/results"
---
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(tidyverse); library(glue); library(gt)
data_dir <- params$data_dir; fig_dir <- params$fig_dir; out_dir <- params$out_dir
```

## Sumário Executivo

- Pergunta de saúde pública.
- Critérios de inclusão/exclusão, período, nível geográfico.
- Métricas primárias: incidência, letalidade, custos, tempo de internação.

## Principais Figuras

```{r figs}
pngs <- list.files(fig_dir, pattern="\\.png$", full.names = TRUE)
if (length(pngs)) lapply(pngs, knitr::include_graphics)
```

## Tabelas

```{r tabs}
# Exemplo: gt::gt(tabela_resumo)
```

---

## 6) Dashboard Shiny (smoke test)

Crie **`D:/workspace/avc_datasus/dashboard/app.R`**:

```r
library(shiny); library(tidyverse); library(glue)
OUT_DIR <- "D:/workspace/avc_datasus/results"
FIG_DIR <- file.path(OUT_DIR, "figs")

ui <- fluidPage(
  titlePanel("AVC — DataSUS (Smoke Test)"),
  sidebarLayout(
    sidebarPanel(helpText("Consome figuras/tabelas já geradas pelo pipeline.")),
    mainPanel(h3("Figuras"), uiOutput("galeria"), h3("Observações"), verbatimTextOutput("obs"))
  )
)

server <- function(input, output, session){
  output$galeria <- renderUI({
    pngs <- list.files(FIG_DIR, pattern="\\.png$", full.names=TRUE)
    if (!length(pngs)) return(div("Sem figuras. Rode o pipeline."))
    tagList(lapply(pngs, function(p) tags$img(src = basename(p), width="80%")))
  })
  output$obs <- renderText(glue("Resultados em: {OUT_DIR}"))
}

shinyApp(ui, server)
```

Executar:

```r
shiny::runApp("D:/workspace/avc_datasus/dashboard", launch.browser = TRUE)
```

---

## 7) Execução canônica (linha de comando)

**PowerShell:**

```powershell
Rscript "D:/workspace/avc_datasus/run.R"
```

- Logs: `D:/workspace/avc_datasus/logs/run_YYYYMMDD_HHMMSS.log`  
- Relatórios: `D:/workspace/avc_datasus/results/relatorios/`  
- Figuras: `D:/workspace/avc_datasus/results/figs/`

---

## 8) Quality Gates (objetivos e simples)

```r
# Exemplo para rodar dentro do seu script após carregar df:
validar_df <- function(df){
  req_cols <- c("ano","uf","internacoes","obitos","custo_total")
  stopifnot(nrow(df) > 1000)
  stopifnot(all(req_cols %in% names(df)))
  invisible(TRUE)
}
```

> **Opinião forte:** se não atender o mínimo, **não** publique artefatos. Corrija dados, regras de negócio ou filtros.

---

## 9) Export para README/portfólio (Git)

**PowerShell:**

```powershell
cd D:/workspace/avc_datasus
Copy-Item .\results\relatorios\relatorio_avc.html . -Force
git init
git add .
git commit -m "AVC DataSUS em R: pipeline reprodutível, relatórios e dashboard (smoke)"
# git remote add origin <url_do_repo>
# git push -u origin main
```

---

## 10) Troubleshooting

- **`across()` fora de `mutate()/summarise()`** → mova para dentro de um verbo do dplyr.  
- **Pastas não criadas** → este pipeline usa `dir.create(..., recursive=TRUE)` no bootstrap e no runner.  
- **Arquivo não encontrado** → pare de usar caminhos relativos mágicos; **todos** os caminhos são definidos no topo.  
- **OneDrive** → marque “Sempre manter neste dispositivo”.

---

## 11) Roadmap (elevar o nível rápido)

1. `targets`/`drake` para pipeline declarativo + cache.  
2. Testes unitários com `testthat` e validações com `assertr`.  
3. Empacotar o código analítico (NAMESPACE limpo) e `renv::snapshot()` fixando versões.  
4. Dockerfile minimal (R + system libs + renv) para portabilidade total.
