# analise_avc_datasus_pipeline.R
# ─────────────────────────────────────────────────────────────────────────────
# Pipeline clínico (AVC) alinhado ao seu script R:
# - Leitura Excel "sujo" (cabeçalho na 3ª linha)
# - Limpeza com janitor::clean_names
# - Normalização de SEXO/OBITO
# - Datas (DATA_ICTUS/DATA_CHEGADA/...) + janela 2018-01-01 a 2019-12-31
# - Flags de comorbidades (HAS/DM/TABAG/AVC prévio/FA/DAC)
# - EDA: resumos, séries mensais, correlações Spearman
# - Plots (ggplot2) e export de CSVs
# - Dashboard Shiny (opcional via RUN_DASHBOARD)
# ─────────────────────────────────────────────────────────────────────────────

# ========================== 1) PACOTES =======================================
req <- c(
  "tidyverse","readxl","janitor","lubridate","ggplot2","ggcorrplot","scales",
  "tsibble","fable","feasts","glue","DT","shiny","shinydashboard","stringr"
)
new <- setdiff(req, rownames(installed.packages()))
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# ========================== 2) CONFIG ========================================
# Caminho do Excel e aba
INPUT_XLSX  <- "D:/workspace/Analitico_AVC.xlsx"
INPUT_SHEET <- "Analitico - 21-11-2019 14h"

# Saídas
OUT_DIR    <- "outputs_avc"
PLOT_DIR   <- file.path(OUT_DIR, "plots")
TAB_DIR    <- file.path(OUT_DIR, "tables")
REPORT_TXT <- file.path(OUT_DIR, "resumo_insights.txt")

# Toggle do dashboard
RUN_DASHBOARD <- FALSE  # mude para TRUE se quiser abrir o Shiny ao final

# Colunas-alvo (ajuste se suas colunas tiverem outros nomes)
COL_IDADE      <- "IDADE"
COL_SEXO       <- "SEXO"
COL_OBITO      <- "OBITO"
COL_TEMPO_INT  <- "TEMPO_INTERNACAO"
COL_TOAST      <- "TOAST"
COL_DEST_ALTA  <- "DESTINO_ALTA"

CANDIDATAS_DATAS <- c("DATA_ICTUS","DATA_CHEGADA","DATA_ADMISSAO","DATA_ADMISÃO","DATA_ADMISSAO_")

# Janela temporal (seguindo seu script)
WIN_START <- as.Date("2018-01-01")
WIN_END   <- as.Date("2019-12-31")

# ========================== 3) FUNÇÕES =======================================
ensure_dir <- function(...) {
  dir.create(file.path(...), recursive = TRUE, showWarnings = FALSE)
}
msg <- function(...) cat(glue::glue(...), "\n")

normalize_obito <- function(x) {
  y <- toupper(trimws(as.character(x)))
  y <- dplyr::recode(y, "ÓBITO"="SIM","OBITO"="SIM","NÃO"="NAO","NA"="","<NA>"="")
  y[y %in% c("","NA","NAN")] <- "NAO"  # tratar vazio como NAO (subestimado)
  y
}
normalize_sexo <- function(x) {
  y <- toupper(trimws(as.character(x)))
  y <- dplyr::recode(y, "MASCULINO"="M","FEMININO"="F")
  y
}
parse_date_best <- function(x) {
  a <- suppressWarnings(lubridate::dmy(x))
  b <- suppressWarnings(lubridate::ymd(x))
  c <- suppressWarnings(lubridate::mdy(x))
  dplyr::coalesce(a,b,c)
}
pick_date_column <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      s <- parse_date_best(df[[nm]])
      if (sum(!is.na(s)) > 0) return(list(name = nm, vec = s))
    }
  }
  has_data <- names(df)[grepl("DATA", names(df), ignore.case = TRUE)]
  for (nm in has_data) {
    s <- parse_date_best(df[[nm]])
    if (sum(!is.na(s)) > 0) return(list(name = nm, vec = s))
  }
  list(name = NA_character_, vec = rep(as.Date(NA), nrow(df)))
}

flag_from_any <- function(df, patterns) {
  cols <- names(df)[sapply(names(df), function(nm) any(stringr::str_detect(toupper(nm), toupper(patterns))))]
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  apply(df[cols], 1, function(row) {
    any(stringr::str_detect(toupper(as.character(row)), "1|SIM|TRUE|POSITIVO|PRESENTE"))
  })
}

save_plot <- function(gg, name, w=8, h=5) {
  ensure_dir(PLOT_DIR)
  fn <- file.path(PLOT_DIR, name)
  try(ggplot2::ggsave(filename = fn, plot = gg, width = w, height = h, dpi = 140), silent = TRUE)
  fn
}

# ========================== 4) LEITURA & LIMPEZA =============================
msg("Lendo Excel: {INPUT_XLSX} | Aba: {INPUT_SHEET}")
df_raw <- readxl::read_excel(INPUT_XLSX, sheet = INPUT_SHEET, col_names = FALSE)

# Detectar linha de cabeçalho (heurística) — similar ao seu script (3ª linha)
header_row <- NA_integer_
for (i in seq_len(min(10, nrow(df_raw)))) {
  vals <- toupper(as.character(unlist(df_raw[i,], use.names = FALSE)))
  if (any(vals %in% c("PACIENTE","SEXO","IDADE","OBITO"))) { header_row <- i; break }
}
if (is.na(header_row)) header_row <- 3

df <- readxl::read_excel(INPUT_XLSX, sheet = INPUT_SHEET, col_names = TRUE, skip = header_row - 1) |>
  janitor::clean_names() |>
  rename_with(~ toupper(.x))

# Remover linhas totalmente vazias
df <- df |> filter(if_any(everything(), ~ !is.na(.x)))

# Normalizar campos-chave
if (COL_OBITO %in% names(df)) df[[COL_OBITO]] <- normalize_obito(df[[COL_OBITO]])
if (COL_SEXO %in% names(df))  df[[COL_SEXO]]  <- normalize_sexo(df[[COL_SEXO]])

numize <- function(x) suppressWarnings(as.numeric(as.character(x)))
if (COL_IDADE %in% names(df))     df[[COL_IDADE]]    <- numize(df[[COL_IDADE]])
if (COL_TEMPO_INT %in% names(df)) df[[COL_TEMPO_INT]]<- numize(df[[COL_TEMPO_INT]])

# Escolher coluna de data (DATA_ICTUS preferencialmente)
dc <- pick_date_column(df, CANDIDATAS_DATAS)
df$`_DATA_REF_` <- dc$vec
col_data_ref    <- dc$name
msg("Coluna de data escolhida: {if (is.na(col_data_ref)) 'N/D' else col_data_ref}")

# Janela temporal
df <- df |>
  filter(`_DATA_REF_` >= WIN_START, `_DATA_REF_` <= WIN_END)

# ========================== 5) FEATURES (COMORBIDADES) =======================
# Mapeamento heurístico pelos nomes das colunas (similar ao seu script)
df$COMORB_HAS   <- flag_from_any(df, c("HAS","HIPERTENSAO","HIPERTENSÃO"))
df$COMORB_DM    <- flag_from_any(df, c("DM","DIABETES"))
df$COMORB_TABAG <- flag_from_any(df, c("TABAG","TABAGISMO","FUMO"))
df$COMORB_AVC_P <- flag_from_any(df, c("AVC_PREV","AVC PR"))
df$COMORB_FA    <- flag_from_any(df, c("FA","FIBRILACAO","FIBRILAÇÃO"))
df$COMORB_DAC   <- flag_from_any(df, c("DAC","CORONARIOPATIA","IAM","CORONÁRIA"))

comorb_cols <- c("COMORB_HAS","COMORB_DM","COMORB_TABAG","COMORB_AVC_P","COMORB_FA","COMORB_DAC")

# >>> CORREÇÃO AQUI: across() dentro de mutate() ou base R  <<<
df <- df %>%
  mutate(COMORB_COUNT = rowSums(across(all_of(comorb_cols), ~ as.numeric(.x)), na.rm = TRUE))

# ========================== 6) EDA & TABELAS =================================
ensure_dir(OUT_DIR); ensure_dir(PLOT_DIR); ensure_dir(TAB_DIR)

N_total  <- nrow(df)
sexo_tab <- if ("SEXO" %in% names(df)) table(df$SEXO, useNA = "no") else c()
obito_tab<- if ("OBITO" %in% names(df)) table(df$OBITO, useNA = "no") else c()

# Série mensal
serie_mensal <- df |>
  mutate(MES = format(`_DATA_REF_`, "%Y-%m")) |>
  count(MES, name = "CASOS") |>
  arrange(MES)

readr::write_csv(df, file.path(TAB_DIR, "base_limpa.csv"))
readr::write_csv(serie_mensal, file.path(TAB_DIR, "serie_mensal.csv"))
if (length(sexo_tab))  write.csv(as.data.frame(sexo_tab), file.path(TAB_DIR,"sexo_counts.csv"), row.names = FALSE)
if (length(obito_tab)) write.csv(as.data.frame(obito_tab), file.path(TAB_DIR,"obito_counts.csv"), row.names = FALSE)

# ========================== 7) CORRELAÇÕES (SPEARMAN) ========================
cand <- c(COL_IDADE, COL_TEMPO_INT, "NIHSS_ENTRADA","NIHSS_ALTA","RANKIN_ALTA","PORTA_AGULHA","ICTUS_PORTA")
cand <- cand[cand %in% names(df)]
df[cand] <- lapply(df[cand], numize)

if (length(cand) >= 2) {
  mat <- df |> dplyr::select(all_of(cand))
  mat <- mat[rowSums(is.na(mat)) < ncol(mat), , drop = FALSE]
  if (nrow(mat) > 2) {
    corr <- suppressWarnings(cor(mat, method = "spearman", use = "pairwise.complete.obs"))
    readr::write_csv(as.data.frame(corr), file.path(TAB_DIR, "correlacoes_spearman.csv"))
  }
}

# ========================== 8) PLOTS (ggplot2) ===============================
# Idade
if (COL_IDADE %in% names(df)) {
  p_idade <- ggplot(df, aes(x = !!sym(COL_IDADE))) +
    geom_histogram(bins = 25, fill = "grey60", color = "white") +
    labs(title = "Distribuição de Idade", x = "Idade (anos)", y = "Frequência")
  save_plot(p_idade, "hist_idade.png")
}

# Tempo de internação
if (COL_TEMPO_INT %in% names(df)) {
  p_ti <- ggplot(df, aes(x = !!sym(COL_TEMPO_INT))) +
    geom_histogram(bins = 25, fill = "grey60", color = "white") +
    labs(title = "Tempo de Internação (dias)", x = "Dias", y = "Frequência")
  save_plot(p_ti, "hist_tempo_internacao.png")
}

# Série mensal
if (nrow(serie_mensal) > 0) {
  p_ts <- ggplot(serie_mensal, aes(x = MES, y = CASOS, group = 1)) +
    geom_line() + geom_point() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Internações por mês (2018–2019)", x = "Mês", y = "Casos")
  save_plot(p_ts, "serie_mensal.png", w = 9)
}

# TOAST
if (COL_TOAST %in% names(df)) {
  toast_tab <- df |> mutate(TOAST_U = toupper(as.character(.data[[COL_TOAST]]))) |>
    count(TOAST_U, sort = TRUE)
  p_toast <- ggplot(toast_tab, aes(x = reorder(TOAST_U, n), y = n)) +
    geom_col(fill = "grey60") +
    coord_flip() +
    labs(title = "Distribuição – TOAST", x = "Subtipo", y = "N")
  save_plot(p_toast, "toast_barras.png", w = 8, h = 6)
}

# ========================== 9) RESUMO (TXT) ==================================
idade_desc <- if (COL_IDADE %in% names(df)) {
  sprintf("Idade: média=%.2f | mediana=%.2f | min=%s | max=%s",
          mean(df[[COL_IDADE]], na.rm=TRUE),
          stats::median(df[[COL_IDADE]], na.rm=TRUE),
          suppressWarnings(min(df[[COL_IDADE]], na.rm=TRUE)),
          suppressWarnings(max(df[[COL_IDADE]], na.rm=TRUE)))
} else "Idade: N/D"

tempo_desc <- if (COL_TEMPO_INT %in% names(df)) {
  sprintf("Tempo de internação: média=%.2f | mediana=%.2f | P25=%.2f | P75=%.2f | max=%s",
          mean(df[[COL_TEMPO_INT]], na.rm=TRUE),
          stats::median(df[[COL_TEMPO_INT]], na.rm=TRUE),
          as.numeric(quantile(df[[COL_TEMPO_INT]], .25, na.rm=TRUE)),
          as.numeric(quantile(df[[COL_TEMPO_INT]], .75, na.rm=TRUE)),
          suppressWarnings(max(df[[COL_TEMPO_INT]], na.rm=TRUE)))
} else "Tempo de internação: N/D"

ensure_dir(OUT_DIR)
sink(REPORT_TXT)
cat("RESUMO – INSIGHTS (conforme script)\n")
cat("Gerado em:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n\n")
cat("N registros:", N_total, "\n")
cat("Coluna de data usada:", if (is.na(col_data_ref)) "N/D" else col_data_ref, "\n")
cat("Janela:", format(WIN_START), "a", format(WIN_END), "\n\n")
if (length(sexo_tab)) { cat("Sexo (N):\n"); print(sexo_tab); cat("\n") }
if (length(obito_tab)) { cat("Óbito (N):\n"); print(obito_tab); cat("\n") }
cat(idade_desc, "\n")
cat(tempo_desc, "\n\n")

if (exists("toast_tab")) {
  cat("TOAST (top):\n"); print(head(toast_tab, 10)); cat("\n")
}
if (exists("serie_mensal")) {
  cat("Série mensal (primeiros 6):\n"); print(head(serie_mensal, 6)); cat("\n")
  cat("Série mensal (últimos 6):\n"); print(tail(serie_mensal, 6)); cat("\n")
}
cat("Comorbidades (flags geradas):", paste(comorb_cols, collapse=", "), "\n")
cat("OBS críticas:\n")
cat("- OBITO com possível sub-registro; vazios tratados como 'NAO'.\n")
cat("- PORTA_AGULHA pode conter 0 para 'não aplicável' → diferenciar zero real vs NA.\n")
cat("- TOAST frequentemente NENHUM/INDETERMINADO → lacuna diagnóstica.\n")
sink()

msg("✓ Pipeline concluído.")
msg("→ Tabelas: {TAB_DIR}")
msg("→ Plots:   {PLOT_DIR}")
msg("→ Resumo:  {REPORT_TXT}")

# ========================== 10) DASHBOARD (OPCIONAL) =========================
if (RUN_DASHBOARD) {
  msg("[DASHBOARD] Iniciando Shiny…")
  library(shiny); library(shinydashboard); library(DT)

  ui <- dashboardPage(
    dashboardHeader(title = "AVC – 2018–2019"),
    dashboardSidebar(
      selectInput("sexo", "Sexo", choices = c("Todos", sort(unique(na.omit(df$SEXO))))),
      checkboxGroupInput("toast", "TOAST", choices = sort(unique(na.omit(toupper(df[[COL_TOAST]]))))),
      sliderInput("idade", "Idade", min = floor(min(df[[COL_IDADE]], na.rm=TRUE)),
                  max = ceiling(max(df[[COL_IDADE]], na.rm=TRUE)),
                  value = c(floor(min(df[[COL_IDADE]], na.rm=TRUE)), ceiling(max(df[[COL_IDADE]], na.rm=TRUE))))
    ),
    dashboardBody(
      fluidRow(
        box(width=12, title="Série mensal", status="primary", solidHeader=TRUE,
            plotOutput("p_ts", height = 260)),
        box(width=6, title="Idade", status="info", solidHeader=TRUE,
            plotOutput("p_idade", height = 260)),
        box(width=6, title="Tempo de internação", status="info", solidHeader=TRUE,
            plotOutput("p_tempo", height = 260)),
        box(width=12, title="Tabela filtrada", status="warning", solidHeader=TRUE,
            DTOutput("tbl"))
      )
    )
  )

  server <- function(input, output, session) {
    dff <- reactive({
      x <- df
      if (input$sexo != "Todos" && "SEXO" %in% names(x)) x <- x |> filter(SEXO == input$sexo)
      if (length(input$toast) && COL_TOAST %in% names(x)) x <- x |> filter(toupper(.data[[COL_TOAST]]) %in% input$toast)
      if (COL_IDADE %in% names(x)) x <- x |> filter(between(.data[[COL_IDADE]], input$idade[1], input$idade[2]))
      x
    })

    output$p_ts <- renderPlot({
      s <- dff() |>
        mutate(MES = format(`_DATA_REF_`, "%Y-%m")) |>
        count(MES, name = "CASOS") |>
        arrange(MES)
      ggplot(s, aes(MES, CASOS, group=1)) + geom_line() + geom_point() +
        theme(axis.text.x = element_text(angle=60, hjust=1)) +
        labs(x="Mês", y="Casos", title="Internações por mês")
    })

    output$p_idade <- renderPlot({
      if (!(COL_IDADE %in% names(dff()))) return(NULL)
      ggplot(dff(), aes(!!sym(COL_IDADE))) + geom_histogram(bins=25, fill="grey60", color="white") +
        labs(x="Idade (anos)", y="Frequência")
    })

    output$p_tempo <- renderPlot({
      if (!(COL_TEMPO_INT %in% names(dff()))) return(NULL)
      ggplot(dff(), aes(!!sym(COL_TEMPO_INT))) + geom_histogram(bins=25, fill="grey60", color="white") +
        labs(x="Dias", y="Frequência", title="Tempo de internação")
    })

    output$tbl <- renderDT({
      DT::datatable(dff(), options = list(pageLength = 10, scrollX = TRUE))
    })
  }

  shinyApp(ui, server)
}
