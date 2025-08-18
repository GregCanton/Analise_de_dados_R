# ─────────────────────────────────────────────────────────────────────────────
# Análise de Internações por AVC — Excel local (2018–2019, comorbidades & dashboard)
# Fonte: D:/workspace/Analitico_AVC.xlsx
# Autor: Gregório Platero Canton
# ─────────────────────────────────────────────────────────────────────────────

# ========= PARÂMETROS DO USUÁRIO =============================================
PATH_EXCEL     <- "D:/workspace/Analitico_AVC.xlsx"  # ajuste se preciso
SHEET_INDEX    <- 1                                   # aba a ler
DATE_START     <- as.Date("2018-01-01")               # filtro inferior
DATE_END       <- as.Date("2019-12-31")               # filtro superior
RUN_DASHBOARD  <- TRUE                                 # abre Shiny no navegador
SAVE_PLOTS     <- TRUE                                 # salva PNGs em ./figs
PLOT_DIR       <- "./figs"
# ==============================================================================

# 1) Pacotes -------------------------------------------------------------------
pkgs <- c(
  "tidyverse","readxl","janitor","lubridate","stringr","readr","rlang",
  "ggplot2","ggcorrplot","tsibble","fable","fabletools","feasts",
  "DT","shiny","shinydashboard","plotly","scales","forcats"
)
new <- pkgs[!(pkgs %in% installed.packages()[,1])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)
options(shiny.launch.browser = TRUE)

# Helpers ----------------------------------------------------------------------
numify  <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
msg_cat <- function(...) cat(paste0(..., "\n"))
ensure_dir <- function(p) { if (!dir.exists(p)) dir.create(p, recursive = TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
save_plot <- function(plot, name, width = 9, height = 5, dpi = 120) {
  if (SAVE_PLOTS) {
    ensure_dir(PLOT_DIR)
    fn <- file.path(PLOT_DIR, name)
    try(ggplot2::ggsave(fn, plot = plot, width = width, height = height, dpi = dpi), silent = TRUE)
    msg_cat("[FIG] Salvo: ", fn)
  }
}
# Formatação BR
fmt_int <- function(x) {
  format(as.numeric(x), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
}
fmt_num <- function(x, digits = 1) {
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",", trim = TRUE, nsmall = digits, scientific = FALSE)
}
options(OutDec = ",", scipen = 999)

# 2) Leitura do Excel e cabeçalho ---------------------------------------------
msg_cat("[LEITURA] Lendo Excel: ", PATH_EXCEL)
raw <- readxl::read_excel(PATH_EXCEL, sheet = SHEET_INDEX, col_names = FALSE)

cand_header <- which(apply(raw, 1, function(r) any(stringr::str_detect(toupper(as.character(r)), "^PACIENTE$"))))
header_row <- if (length(cand_header)) cand_header[1] else 1L

dados <- readxl::read_excel(PATH_EXCEL, sheet = SHEET_INDEX, skip = header_row - 1) |>
  janitor::remove_empty(c("rows","cols")) |>
  janitor::clean_names()

msg_cat("[LEITURA] Dimensão após limpeza inicial: ", paste(dim(dados), collapse = " x "))

# 3) Anonimização mínima (sem expor nomes) ------------------------------------
pii_cols <- intersect(names(dados), c("paciente","rg_paciente","nome","nome_paciente"))
if (length(pii_cols) > 0) {
  id_vec <- rep(NA_character_, nrow(dados))
  for (nm in pii_cols) id_vec <- dplyr::coalesce(id_vec, as.character(dados[[nm]]))
  dados <- dados |>
    dplyr::mutate(id_paciente = as.integer(factor(id_vec))) |>
    dplyr::select(-dplyr::all_of(pii_cols))
  rm(id_vec)
  msg_cat("[PRIVACIDADE] Colunas PII removidas: ", paste(pii_cols, collapse = ", "))
}

# 4) Datas e tipos -------------------------------------------------------------
date_patterns <- c("data","dt","nascimento","chegada","ictus","alta","entrada","saida","admissao")
date_cols <- names(dados)[sapply(names(dados), function(nm) any(stringr::str_detect(nm, paste0(date_patterns, collapse = "|"))))]

for (col in date_cols) {
  if (!inherits(dados[[col]], "Date") && !inherits(dados[[col]], "POSIXt")) {
    conv <- suppressWarnings(lubridate::parse_date_time(dados[[col]],
      orders = c("dmy HMS","dmy HM","dmy","ymd HMS","ymd HM","ymd","mdy HMS","mdy HM","mdy")))
    if (sum(!is.na(conv)) > 0) dados[[col]] <- conv
  }
}

# padroniza sexo/óbito e força numérico em métricas contínuas
if ("sexo" %in% names(dados)) {
  dados$sexo <- as.character(dados$sexo)
  dados$sexo <- ifelse(stringr::str_to_upper(dados$sexo) %in% c("M","MASCULINO"), "Masculino",
                       ifelse(stringr::str_to_upper(dados$sexo) %in% c("F","FEMININO"), "Feminino", dados$sexo))
  dados$sexo <- factor(dados$sexo, levels = c("Masculino","Feminino"))
}
if ("obito" %in% names(dados)) {
  dados$obito <- as.character(dados$obito)
  dados$obito <- ifelse(stringr::str_to_upper(dados$obito) %in% c("SIM","S","Y","1"), "Sim",
                        ifelse(stringr::str_to_upper(dados$obito) %in% c("NAO","NÃO","NAO ","N","0"), "Não", dados$obito))
  dados$obito <- factor(dados$obito, levels = c("Não","Sim"))
}
for (nm in c("idade","nihss_entrada","nihss_alta","porta_agulha","tempo_internacao",
             "ictus_porta","porta_tc","porta_medico","ranking")) {
  if (nm %in% names(dados)) dados[[nm]] <- numify(dados[[nm]])
}

# 5) Escolhe coluna de data e filtra 2018–2019 ---------------------------------
cands_data <- c("data_chegada","data_ictus","dt_entrada","dt_alta")
col_data   <- intersect(cands_data, names(dados))
if (length(col_data)) {
  col_data <- col_data[1]
  dados <- dados |>
    dplyr::mutate(.data_filtro = as.Date(.data[[col_data]])) |>
    dplyr::filter(!is.na(.data_filtro), .data_filtro >= DATE_START, .data_filtro <= DATE_END)
  msg_cat("[FILTRO] Janela: ", format(DATE_START), " → ", format(DATE_END),
          " | col_data: ", col_data, " | n: ", nrow(dados))
} else {
  msg_cat("[FILTRO] Nenhuma coluna de data viável (esperado: data_chegada / data_ictus / dt_entrada / dt_alta).")
  dados$.data_filtro <- as.Date(NA)
}

# 6) Detecção de comorbidades (heurística) -------------------------------------
comorb_map <- list(
  "HAS"                 = c("hipert","has\\b"),
  "DM"                  = c("\\bdm\\b","diab"),
  "FA"                  = c("\\bfa\\b","fibril"),
  "DLP"                 = c("dislip","dyslip"),
  "IRC"                 = c("\\birc\\b","renal"),
  "DPOC"                = c("\\bdpoc\\b","pulmon"),
  "IC"                  = c("^ic\\b","insuf.*card"),
  "Tabagismo"           = c("tabag"),
  "AVC_previo"          = c("avc.*prev"),
  "Coronariopatia_IAM"  = c("coron","\\biam\\b","infarto")
)
build_comorb_flags <- function(df, mapping) {
  out <- list()
  for (nm in names(mapping)) {
    pats <- mapping[[nm]]
    cols <- names(df)[Reduce(`|`, lapply(pats, function(p) grepl(p, names(df), ignore.case = TRUE)))]
    if (length(cols) == 0) next
    v <- rep(NA_real_, nrow(df))
    for (cl in cols) {
      val_chr <- tolower(as.character(df[[cl]]))
      pos <- val_chr %in% c("sim","s","y","1","true","t","x","positivo","pos","yes")
      neg <- val_chr %in% c("nao","não","n","0","false","f","negativo","neg","no")
      num <- suppressWarnings(as.numeric(val_chr)); num_pos <- !is.na(num) & num > 0
      v[pos | num_pos] <- 1
      v[neg] <- ifelse(is.na(v[neg]), 0, v[neg])
    }
    out[[nm]] <- as.integer(replace(v, is.na(v), 0))
  }
  if (length(out)) tibble::as_tibble(out) else tibble::tibble()
}
comorb <- build_comorb_flags(dados, comorb_map)
if (ncol(comorb) > 0) {
  dados <- dplyr::bind_cols(dados, comorb)
  dados$comorb_count <- rowSums(dplyr::select(dados, dplyr::all_of(names(comorb))), na.rm = TRUE)
  msg_cat("[COMORB] Flags criadas: ", paste(names(comorb), collapse = ", "),
          " | comorb_count pronto.")
} else {
  msg_cat("[COMORB] Nenhuma comorbidade reconhecida pelos padrões (ajuste comorb_map se necessário).")
}

# 7) Resumos essenciais --------------------------------------------------------
msg_cat("[RESUMO] Registros (2018–2019): ", nrow(dados), " | Variáveis: ", ncol(dados))
if ("sexo" %in% names(dados)) { msg_cat("[RESUMO] Sexo:"); print(table(dados$sexo, useNA = "ifany")) }
if ("idade" %in% names(dados)) {
  qs <- quantile(dados$idade, probs = c(.25,.5,.75), na.rm = TRUE)
  msg_cat(sprintf("[RESUMO] Idade — mediana %s (P25 %s, P75 %s)", fmt_num(qs[2],1), fmt_num(qs[1],1), fmt_num(qs[3],1)))
}

# 8) Gráficos — mais corpo (boxplots/correlações) ------------------------------
# 8.1 Idade por sexo (boxplot)
if ("idade" %in% names(dados) && "sexo" %in% names(dados)) {
  p_box_idade <- dados |>
    dplyr::filter(!is.na(idade), !is.na(sexo)) |>
    ggplot(aes(x = sexo, y = idade, fill = sexo)) +
    geom_boxplot(outlier.alpha = .25) +
    labs(title = "Idade por Sexo (2018–2019)", x = NULL, y = "Idade (anos)") +
    theme_minimal() + theme(legend.position = "none")
  print(p_box_idade); save_plot(p_box_idade, "boxplot_idade_sexo.png")
}
# 8.2 NIHSS por sexo (boxplot)
if ("nihss_entrada" %in% names(dados) && "sexo" %in% names(dados)) {
  p_box_nihss <- dados |>
    dplyr::filter(!is.na(nihss_entrada), !is.na(sexo)) |>
    ggplot(aes(x = sexo, y = nihss_entrada, fill = sexo)) +
    geom_boxplot(outlier.alpha = .25) +
    labs(title = "NIHSS na Entrada por Sexo (2018–2019)", x = NULL, y = "NIHSS") +
    theme_minimal() + theme(legend.position = "none")
  print(p_box_nihss); save_plot(p_box_nihss, "boxplot_nihss_sexo.png")
}
# 8.3 Porta-agulha por comorbidade mais prevalente (boxplot)
if ("porta_agulha" %in% names(dados) && ncol(comorb) > 0) {
  top_flag <- names(sort(colSums(comorb, na.rm = TRUE), decreasing = TRUE))[1]
  p_box_pa <- dados |>
    dplyr::filter(!is.na(porta_agulha)) |>
    dplyr::mutate(flag = factor(ifelse(.data[[top_flag]] == 1, "Comorbidade presente", "Ausente"))) |>
    ggplot(aes(x = flag, y = porta_agulha, fill = flag)) +
    geom_boxplot(outlier.alpha = .25) +
    labs(title = paste0("Porta-agulha por ", top_flag, " (2018–2019)"), x = NULL, y = "Minutos") +
    theme_minimal() + theme(legend.position = "none")
  print(p_box_pa); save_plot(p_box_pa, "boxplot_porta_agulha_comorb.png")
}
# 8.4 Correlação entre comorbidades (phi) — heatmap (robusto)
if (ncol(comorb) >= 2) {
  com_num <- dplyr::mutate_all(comorb, as.numeric)
  keep <- sapply(com_num, function(x) stats::sd(x, na.rm = TRUE) > 0)
  com_num <- com_num[, keep, drop = FALSE]
  if (ncol(com_num) >= 2) {
    Mphi <- suppressWarnings(stats::cor(as.matrix(com_num), use = "pairwise.complete.obs", method = "pearson"))
    Mphi[!is.finite(Mphi)] <- 0; diag(Mphi) <- 1
    p_phi <- ggcorrplot::ggcorrplot(Mphi, lab = FALSE, hc.order = FALSE, type = "lower") +
      labs(title = "Correlação (phi) entre comorbidades")
    print(p_phi); save_plot(p_phi, "correlacao_comorbidades.png")
  } else {
    msg_cat("[COR-COMORB] Todas as comorbidades constantes (sem variação).")
  }
}
# 8.5 Spearman: contínuas vs carga de comorbidades
conts <- c("idade","nihss_entrada","nihss_alta","porta_agulha","tempo_internacao",
           "ictus_porta","porta_tc","porta_medico")
conts <- intersect(conts, names(dados))
if (length(conts) > 0 && "comorb_count" %in% names(dados)) {
  base_corr <- dplyr::select(dados, dplyr::all_of(conts), comorb_count)
  keep <- sapply(base_corr, function(x) stats::sd(x, na.rm = TRUE) > 0)
  base_corr <- base_corr[, keep, drop = FALSE]
  if (ncol(base_corr) >= 2) {
    Ms <- suppressWarnings(stats::cor(base_corr, use = "pairwise.complete.obs", method = "spearman"))
    Ms[!is.finite(Ms)] <- 0; diag(Ms) <- 1
    p_spear <- ggcorrplot::ggcorrplot(Ms, lab = FALSE, hc.order = FALSE, type = "lower") +
      labs(title = "Correlação (Spearman) — contínuas & carga de comorbidades")
    print(p_spear); save_plot(p_spear, "correlacao_spearman_cont.png")
  } else {
    msg_cat("[COR-SPEAR] Sem colunas contínuas com variância suficiente.")
  }
}

# 9) Série temporal mensal (2018–2019) ----------------------------------------
if (any(!is.na(dados$.data_filtro))) {
  df_ts <- dados |>
    dplyr::mutate(mes_ym = tsibble::yearmonth(lubridate::floor_date(.data_filtro, "month"))) |>
    dplyr::count(mes_ym, name = "hosp") |>
    dplyr::arrange(mes_ym) |>
    tsibble::as_tsibble(index = mes_ym) |>
    tsibble::fill_gaps(hosp = 0)

  p_ts <- fabletools::autoplot(df_ts, hosp) +
    labs(title = "Internações por mês (2018–2019)", x = NULL, y = "Contagem")
  print(p_ts); save_plot(p_ts, "serie_mensal_2018_2019.png")

  # Previsão se houver pontos suficientes (>= 12)
  if (nrow(df_ts) >= 12) {
    fit <- df_ts |> fabletools::model(ARIMA = fable::ARIMA(hosp))
    fc  <- fit  |> fabletools::forecast(h = 6)
    p_fc <- fabletools::autoplot(fc, df_ts) +
      labs(title = "Previsão (ARIMA, 6 meses)", x = NULL, y = "Internações")
    print(p_fc); save_plot(p_fc, "previsao_arima_2018_2019.png")
  } else {
    msg_cat("[TS] Série curta (<12 pontos). Previsão pulada.")
  }
}

# 10) Métricas operacionais (pivot robusto) ------------------------------------
tempo_vars <- intersect(
  c("ictus_porta","porta_tc","porta_medico","porta_agulha","tempo_internacao",
    "nihss_entrada","nihss_alta","ranking"),
  names(dados)
)
if (length(tempo_vars)) {
  dados <- dados |>
    dplyr::mutate(dplyr::across(dplyr::all_of(tempo_vars), numify))

  long <- dados |>
    dplyr::select(dplyr::all_of(tempo_vars)) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variavel", values_to = "valor",
                        values_drop_na = TRUE, values_transform = list(valor = as.numeric))

  p_tempos <- ggplot(long, aes(x = valor)) +
    geom_histogram(bins = 30, na.rm = TRUE) +
    facet_wrap(~ variavel, scales = "free") +
    labs(title = "Distribuições — métricas clínicas/operacionais", x = "Valor", y = "Frequência") +
    theme_minimal()
  print(p_tempos); save_plot(p_tempos, "metricas_operacionais.png")

  if ("porta_agulha" %in% names(dados)) {
    valid <- numify(dados$porta_agulha); valid <- valid[!is.na(valid)]
    if (length(valid)) {
      med <- stats::median(valid); q1 <- stats::quantile(valid, .25); q3 <- stats::quantile(valid, .75)
      pct60 <- mean(valid <= 60) * 100
      msg_cat(sprintf("[PORTA-AGULHA] Mediana %s (P25 %s, P75 %s) | %% ≤ 60 min: %s%%",
        fmt_num(med,1), fmt_num(q1,1), fmt_num(q3,1), fmt_num(pct60,1)))
    }
  }
}

# 11) Dashboard Shiny (multi-abas, sem PII) ------------------------------------
if (RUN_DASHBOARD) {
  msg_cat("[DASHBOARD] Iniciando Shiny (Visao Geral, Comorbidades, Tempos & Qualidade, Dados)")

  agg <- if (any(!is.na(dados$.data_filtro))) {
    dados |>
      dplyr::mutate(mes = lubridate::floor_date(.data_filtro, "month")) |>
      dplyr::count(mes, sexo, name = "hosp")
  } else {
    tibble::tibble(mes = as.Date(NA), sexo = factor(NA), hosp = NA_integer_) |> tidyr::drop_na()
  }

  comorb_long <- NULL
  if (ncol(comorb) > 0) {
    comorb_long <- comorb |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~mean(.x, na.rm = TRUE))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "comorbidade", values_to = "prev") |>
      dplyr::mutate(prev = 100*prev) |>
      dplyr::arrange(desc(prev))
  }

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "AVC — Excel local (2018–2019)"),
    shinydashboard::dashboardSidebar(
      sidebarMenu(
        menuItem("Visão Geral", tabName = "overview", icon = icon("chart-line")),
        menuItem("Comorbidades", tabName = "comorb", icon = icon("heartbeat")),
        menuItem("Tempos & Qualidade", tabName = "tempos", icon = icon("stopwatch")),
        menuItem("Dados", tabName = "data", icon = icon("table"))
      )
    ),
    shinydashboard::dashboardBody(
      tabItems(
        tabItem("overview",
          fluidRow(
            valueBoxOutput("vb_total", width = 3),
            valueBoxOutput("vb_age", width = 3),
            valueBoxOutput("vb_nihss", width = 3),
            valueBoxOutput("vb_comorb", width = 3)
          ),
          fluidRow(
            box(width = 12, title = "Série mensal (2018–2019)", status = "primary", solidHeader = TRUE,
                plotlyOutput("plt_ts", height = 360))
          ),
          fluidRow(
            box(width = 6, title = "Idade por Sexo (boxplot)", status = "info", solidHeader = TRUE,
                plotlyOutput("plt_box_idade", height = 320)),
            box(width = 6, title = "NIHSS por Sexo (boxplot)", status = "info", solidHeader = TRUE,
                plotlyOutput("plt_box_nihss", height = 320))
          )
        ),
        tabItem("comorb",
          fluidRow(
            box(width = 6, title = "Prevalência de Comorbidades (%)", status = "success", solidHeader = TRUE,
                plotlyOutput("plt_comorb_prev", height = 360)),
            box(width = 6, title = "Correlação (phi) entre comorbidades", status = "success", solidHeader = TRUE,
                plotlyOutput("plt_comorb_phi", height = 360))
          ),
          fluidRow(
            box(width = 12, title = "Spearman: contínuas vs carga de comorbidades", status = "success", solidHeader = TRUE,
                plotlyOutput("plt_spearman", height = 360))
          )
        ),
        tabItem("tempos",
          fluidRow(
            box(width = 12, title = "Distribuições — métricas clínicas/operacionais", status = "warning", solidHeader = TRUE,
                plotlyOutput("plt_tempos", height = 380))
          ),
          fluidRow(
            box(width = 12, title = "Porta-agulha por comorbidade mais prevalente", status = "warning", solidHeader = TRUE,
                plotlyOutput("plt_box_pa", height = 320))
          )
        ),
        tabItem("data",
          fluidRow(
            box(width = 12, title = "Amostra dos dados (100 linhas, sem PII)", status = "primary", solidHeader = TRUE,
                DTOutput("tbl_dados"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    output$vb_total <- renderValueBox({
      shinydashboard::valueBox(fmt_int(nrow(dados)), "Registros (2018–2019)", icon = icon("database"), color = "purple")
    })
    output$vb_age <- renderValueBox({
      v <- if ("idade" %in% names(dados)) stats::median(dados$idade, na.rm = TRUE) else NA
      shinydashboard::valueBox(ifelse(is.na(v), "—", fmt_num(v, 1)), "Idade mediana", icon = icon("user"), color = "teal")
    })
    output$vb_nihss <- renderValueBox({
      v <- if ("nihss_entrada" %in% names(dados)) stats::median(dados$nihss_entrada, na.rm = TRUE) else NA
      shinydashboard::valueBox(ifelse(is.na(v), "—", fmt_num(v, 1)), "NIHSS (entrada) mediana", icon = icon("notes-medical"), color = "yellow")
    })
    output$vb_comorb <- renderValueBox({
      v <- if ("comorb_count" %in% names(dados)) mean(dados$comorb_count, na.rm = TRUE) else NA
      shinydashboard::valueBox(ifelse(is.na(v), "—", fmt_num(v, 2)), "Comorbidades por paciente (média)", icon = icon("heartbeat"), color = "maroon")
    })

    output$plt_ts <- renderPlotly({
      if (!nrow(agg)) return(NULL)
      dat <- agg |>
        dplyr::group_by(mes) |>
        dplyr::summarise(hosp = sum(hosp, na.rm = TRUE), .groups = "drop")
      g <- ggplot(dat, aes(mes, hosp)) + geom_line() + geom_point() + labs(x = NULL, y = "Contagem")
      ggplotly(g, dynamicTicks = TRUE)
    })
    output$plt_box_idade <- renderPlotly({
      if (!("idade" %in% names(dados) && "sexo" %in% names(dados))) return(NULL)
      g <- dados |> dplyr::filter(!is.na(idade), !is.na(sexo)) |>
        ggplot(aes(x = sexo, y = idade, fill = sexo)) + geom_boxplot(outlier.alpha = .25) +
        labs(x = NULL, y = "Idade")
      ggplotly(g)
    })
    output$plt_box_nihss <- renderPlotly({
      if (!("nihss_entrada" %in% names(dados) && "sexo" %in% names(dados))) return(NULL)
      g <- dados |> dplyr::filter(!is.na(nihss_entrada), !is.na(sexo)) |>
        ggplot(aes(x = sexo, y = nihss_entrada, fill = sexo)) + geom_boxplot(outlier.alpha = .25) +
        labs(x = NULL, y = "NIHSS")
      ggplotly(g)
    })

    output$plt_comorb_prev <- renderPlotly({
      if (is.null(comorb_long) || !nrow(comorb_long)) return(NULL)
      g <- comorb_long |>
        dplyr::mutate(comorbidade = forcats::fct_reorder(comorbidade, prev)) |>
        ggplot(aes(x = comorbidade, y = prev)) +
        geom_col() + coord_flip() + scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        labs(x = NULL, y = "Prevalência (%)")
      ggplotly(g)
    })

    output$plt_comorb_phi <- renderPlotly({
      if (ncol(comorb) < 2) return(NULL)
      com_num <- dplyr::mutate_all(comorb, as.numeric)
      keep <- sapply(com_num, function(x) stats::sd(x, na.rm = TRUE) > 0)
      com_num <- com_num[, keep, drop = FALSE]
      if (ncol(com_num) < 2) return(NULL)
      Mphi <- suppressWarnings(stats::cor(as.matrix(com_num), use = "pairwise.complete.obs", method = "pearson"))
      Mphi[!is.finite(Mphi)] <- 0; diag(Mphi) <- 1
      dfm <- as.data.frame(as.table(Mphi)); names(dfm) <- c("x","y","val")
      g <- ggplot(dfm, aes(x, y, fill = val)) +
        geom_tile() + scale_fill_gradient2(limits = c(-1,1)) +
        labs(x = NULL, y = NULL) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g)
    })

    output$plt_spearman <- renderPlotly({
      conts <- c("idade","nihss_entrada","nihss_alta","porta_agulha","tempo_internacao","ictus_porta","porta_tc","porta_medico")
      conts <- intersect(conts, names(dados))
      if (length(conts) == 0 || !("comorb_count" %in% names(dados))) return(NULL)
      base_corr <- dplyr::select(dados, dplyr::all_of(conts), comorb_count)
      keep <- sapply(base_corr, function(x) sd(x, na.rm = TRUE) > 0)
      base_corr <- base_corr[, keep, drop = FALSE]
      if (ncol(base_corr) < 2) return(NULL)
      Ms <- suppressWarnings(stats::cor(base_corr, use = "pairwise.complete.obs", method = "spearman"))
      Ms[!is.finite(Ms)] <- 0; diag(Ms) <- 1
      dfm <- as.data.frame(as.table(Ms)); names(dfm) <- c("x","y","val")
      g <- ggplot(dfm, aes(x, y, fill = val)) +
        geom_tile() + scale_fill_gradient2(limits = c(-1,1)) +
        labs(x = NULL, y = NULL) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g)
    })

    output$plt_tempos <- renderPlotly({
      tempo_vars <- intersect(
        c("ictus_porta","porta_tc","porta_medico","porta_agulha","tempo_internacao",
          "nihss_entrada","nihss_alta","ranking"),
        names(dados)
      )
      if (!length(tempo_vars)) return(NULL)
      long <- dados |>
        dplyr::mutate(dplyr::across(dplyr::all_of(tempo_vars), numify)) |>
        dplyr::select(dplyr::all_of(tempo_vars)) |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variavel", values_to = "valor", values_drop_na = TRUE)
      g <- ggplot(long, aes(x = valor)) + geom_histogram(bins = 30) +
        facet_wrap(~ variavel, scales = "free") + labs(x = "Valor", y = "Frequência")
      ggplotly(g)
    })
    output$plt_box_pa <- renderPlotly({
      if (!("porta_agulha" %in% names(dados)) || ncol(comorb) == 0) return(NULL)
      top_flag <- names(sort(colSums(comorb, na.rm = TRUE), decreasing = TRUE))[1]
      g <- dados |>
        dplyr::filter(!is.na(porta_agulha)) |>
        dplyr::mutate(flag = factor(ifelse(.data[[top_flag]] == 1, "Comorbidade presente", "Ausente"))) |>
        ggplot(aes(x = flag, y = porta_agulha, fill = flag)) + geom_boxplot(outlier.alpha = .25) +
        labs(x = NULL, y = "Porta-agulha (min)") + theme(legend.position = "none")
      ggplotly(g)
    })

    output$tbl_dados <- renderDT({
      df_show <- dados |> dplyr::select(-dplyr::any_of(c("id_paciente")))
      datatable(head(df_show, 100), options = list(pageLength = 10, scrollX = TRUE))
    })
  }

  shiny::runApp(list(ui = ui, server = server), launch.browser = TRUE)
}

msg_cat("✔ Finalizado — análise 2018–2019 com boxplots, correlações e dashboard multi-abas (figuras em ./figs).")
