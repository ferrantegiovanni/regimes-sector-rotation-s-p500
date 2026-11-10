# =============================================================================
#  TABELLE DEL PAPER
#  Legge le tabelle CSV prodotte dagli script e le converte in frammenti
#  LaTeX (booktabs) in paper/tab/. Nessun numero viene trascritto a mano:
#  se gli script cambiano i risultati, basta rieseguire questo file e poi
#  compilare il paper.
#  Directory di lavoro: la cartella paper/.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

EST  <- "../studi"
dir.create("tab", showWarnings = FALSE)

#  Tutto come testo: le tabelle sono gia' formattate dagli script, e la
#  lettura numerica perderebbe gli zeri finali (-0.30 -> -0.3).
leggi_t <- function(f) read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                                colClasses = "character", na.strings = character(0),
                                encoding = "UTF-8")

#  Testo -> LaTeX: escape dei caratteri speciali, segno meno tipografico.
#  Il dollaro NON si escapa: lo usano gli apici delle stelle ($^{***}$) e
#  le intestazioni matematiche; nelle tabelle non compare come testo.
tex <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("([%&_#])", "\\\\\\1", x)
  x <- gsub("(^|\\s|\\()-(?=\\d)", "\\1$-$", x, perl = TRUE)
  x <- gsub("^-$|^$", "--", x)
  x
}

#  Frammento booktabs: tabella con didascalia sopra e nota sotto, come nel
#  paper di riferimento.
scrivi <- function(df, file, didascalia, etichetta, allinea, nota = NULL,
                   gruppi = NULL, size = "\\small") {
  intest <- paste(tex(names(df)), collapse = " & ")
  righe <- apply(df, 1, function(r) paste(tex(r), collapse = " & "))
  if (!is.null(gruppi)) {
    #  gruppi: vettore lungo nrow(df) con l'etichetta del gruppo di ogni riga
    out <- character(0)
    for (i in seq_along(righe)) {
      if (i == 1 || gruppi[i] != gruppi[i - 1]) {
        if (i > 1) out <- c(out, "\\addlinespace")
        out <- c(out, sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", ncol(df), tex(gruppi[i])))
      }
      out <- c(out, paste0(righe[i], " \\\\"))
    }
    corpo <- out
  } else corpo <- paste0(righe, " \\\\")
  #  La numerazione la fa LaTeX: il prefisso "Table n." serve solo a chi
  #  legge questo file.
  didascalia <- sub("^\\\\textbf\\{Table [0-9]+\\.\\} ", "", didascalia)
  linee <- c(
    "\\begin{table}[htbp]", "\\centering", size,
    sprintf("\\caption{%s}\\label{%s}", didascalia, etichetta),
    #  La tabella si riduce alla larghezza della pagina solo se la supera.
    "\\setbox0\\hbox{%",
    sprintf("\\begin{tabular}{%s}", allinea), "\\toprule",
    paste0(intest, " \\\\"), "\\midrule", corpo, "\\bottomrule", "\\end{tabular}}%",
    "\\ifdim\\wd0>\\linewidth\\resizebox{\\linewidth}{!}{\\box0}\\else\\box0\\fi",
    if (!is.null(nota)) c("\\par\\vspace{3pt}", sprintf("\\parbox{0.95\\linewidth}{\\footnotesize %s}", nota)),
    "\\end{table}")
  writeLines(linee, file.path("tab", file), useBytes = TRUE)
}

stelle_t <- function(stima, sig) paste0(stima, ifelse(nzchar(sig), paste0("$^{", sig, "}$"), ""))

# ---- 1. Rotazione: performance dei regimi --------------------------------
r1 <- leggi_t(file.path(EST, "tabelle_regime/table_R1_performance.csv"))
c1 <- leggi_t(file.path(EST, "tabelle_ciclo/table_C1_performance.csv"))
r2s <- leggi_t(file.path(EST, "tabelle_regime/table_R2_significance.csv"))
c2s <- leggi_t(file.path(EST, "tabelle_ciclo/table_C2_significance.csv"))
diff_vs <- function(s, nome) {
  r <- s |> filter(Group == "Pairwise differences",
                   Item == paste0(nome, " minus Unconditional rotation, Net of costs"))
  if (nrow(r) == 0) c("", "") else c(stelle_t(r$Estimate, r$Sig.), r$`t-stat`)
}
perf <- bind_rows(r1 |> filter(Version != "Gross of costs"),
                  c1 |> filter(Version == "Net of costs", grepl("Business|Monetary", Strategy))) |>
  distinct(Strategy, .keep_all = TRUE)
ordine <- c("Equal-weight sectors", "Unconditional rotation", "HMM regime rotation",
            "SMA regime rotation", "Trend regime rotation", "Business-cycle rotation",
            "Monetary-policy rotation", "Benchmark (S&P 500 TR)")
perf <- perf[match(ordine, perf$Strategy), ]
dd <- t(vapply(perf$Strategy, function(s) {
  if (s %in% c("HMM regime rotation", "SMA regime rotation", "Trend regime rotation")) diff_vs(r2s, s)
  else if (s %in% c("Business-cycle rotation", "Monetary-policy rotation")) diff_vs(c2s, s)
  else c("", "") }, character(2)))
scrivi(perf |> transmute(Strategy, Return = `Annualized return`, Volatility = `Annualized volatility`,
                         Sharpe = `Sharpe ratio`, `Max drawdown` = `Maximum drawdown`,
                         Turnover = `Average monthly turnover`) |>
         mutate(`vs unconditional` = dd[, 1], `t-stat` = dd[, 2]),
       "t_rot_perf.tex", "\\textbf{Table 1.} Sector rotation with and without regime signals, net of costs",
       "tab:rotperf", "lrrrrrrr",
       gruppi = c("No regime", "No regime", rep("Statistical and rule-based regimes", 3),
                  rep("Economic regimes", 2), "Index"),
       nota = "Three TRBC economic sectors held, equally weighted, cap-weighted within each sector; monthly rebalancing; 10 bp per side. The last two columns report the mean annualised difference from the unconditional rotation and its Newey-West t-statistic.")

# ---- 2. Rotazione: diagnostica dei regimi --------------------------------
r3 <- leggi_t(file.path(EST, "tabelle_regime/table_R3_regimes.csv")) |> filter(`Regime model` != "HMM (3 states)")
c3 <- leggi_t(file.path(EST, "tabelle_ciclo/table_C3_regimes.csv"))
diag <- bind_rows(
  bind_rows(lapply(seq_len(nrow(r3)), function(i) data.frame(
    Model = r3$`Regime model`[i], Regime = c("Favourable", "Adverse"),
    Share = c(sprintf("%.1f%%", 100 - as.numeric(sub("%", "", r3$`Share of adverse months`[i]))),
              r3$`Share of adverse months`[i]),
    Switches = as.character(r3$`Regime switches`[i]),
    `Market return` = c(r3$`Market return after favorable`[i], r3$`Market return after adverse`[i]),
    `Market volatility` = c(r3$`Market volatility after favorable`[i], r3$`Market volatility after adverse`[i]),
    check.names = FALSE))),
  c3 |> transmute(Model = `Regime model`, Regime, Share = `Share of months`,
                  Switches = as.character(`Regime switches`),
                  `Market return` = `Market return, next month (ann.)`,
                  `Market volatility` = `Market volatility, next month (ann.)`))
scrivi(diag |> select(-Model), "t_rot_diag.tex",
       "\\textbf{Table 2.} Regimes in the backtest sample and the market in the following month",
       "tab:rotdiag", "lrrrr", gruppi = diag$Model,
       nota = "Share of the 257 formation months in each regime, number of regime switches, and annualised mean and volatility of the market return (Kenneth French) in the month after the regime is observed. Descriptive only: no strategy uses these figures.")

# ---- 3. Rotazione sul ciclo: robustezza ----------------------------------
c5 <- leggi_t(file.path(EST, "tabelle_ciclo/table_C5_robustness.csv"))
scrivi(c5 |> transmute(Variant = sub(" \\(2 phases.*\\)$", "", Variant), Return = `Annualized return`,
                       Sharpe = `Sharpe ratio`, Turnover = `Average monthly turnover`,
                       `vs unconditional` = `Difference vs unconditional (ann.)`, `t-stat` = `t-stat (Newey-West)`),
       "t_rot_rob.tex", "\\textbf{Table 3.} Robustness of the business-cycle rotation, net of costs",
       "tab:rotrob", "lrrrrr", gruppi = c5$Group,
       nota = "Baseline: two phases (above or below trend), industrial production and payrolls, five-month confirmation, three sectors held. Each variant changes one element.")

# ---- 4. Costruzione: performance e caratteristiche -----------------------
k1 <- leggi_t(file.path(EST, "tabelle_costruzione/table_K1_performance.csv")) |> filter(Version != "Gross of costs")
k2 <- leggi_t(file.path(EST, "tabelle_costruzione/table_K2_portfolio_characteristics.csv"))
scrivi(k1 |> left_join(k2, by = "Strategy") |>
         transmute(Method = Strategy, Return = `Annualized return`, Volatility = `Annualized volatility`,
                   Sharpe = `Sharpe ratio`, `Max drawdown` = `Maximum drawdown`,
                   Turnover = `Average monthly turnover`, `Eff. stocks` = `Effective number of stocks`,
                   `Max weight` = `Largest weight`, Beta = `Market beta`),
       "t_cos_perf.tex", "\\textbf{Table 4.} Construction methods on the stocks selected by the business-cycle rotation, net of costs",
       "tab:cosperf", "lrrrrrrrr", size = "\\footnotesize",
       nota = "All methods weight the same stocks at each date (members of the three selected sectors with a complete 60-month return history). Effective number of stocks: inverse of the sum of squared weights, averaged over formation dates. Market beta from a market-model regression of net excess returns.")

k3 <- leggi_t(file.path(EST, "tabelle_costruzione/table_K3_significance.csv")) |>
  filter(Group != "Versus benchmark, net of costs", !grepl("Gross of costs", Item))
scrivi(k3 |> transmute(Comparison = sub(", Net of costs$", "", Item), Estimate = stelle_t(Estimate, Sig.),
                       `t-stat` = `t-stat`, `p-value` = `p-value`),
       "t_cos_sig.tex", "\\textbf{Table 5.} Construction methods against equal weighting",
       "tab:cossig", "lrrr", gruppi = k3$Group,
       nota = "Return differences: mean annualised difference with Newey-West t-statistics. Volatility: ratio of the volatility of each method to that of the equal-weighted portfolio, with the Pitman-Morgan test for paired samples.")

k5 <- leggi_t(file.path(EST, "tabelle_costruzione/table_K5_robustness.csv"))
scrivi(k5 |> transmute(Variant, Return = `Annualized return`, Volatility = `Annualized volatility`,
                       Sharpe = `Sharpe ratio`, Turnover = `Average monthly turnover`,
                       `Eff. stocks` = `Effective number of stocks`),
       "t_cos_rob.tex", "\\textbf{Table 6.} Construction methods: weight cap and common set of stocks, net of costs",
       "tab:cosrob", "lrrrrr", gruppi = k5$Group, size = "\\footnotesize")

cat("Tabelle del paper scritte in paper/tab/:", length(list.files("tab")), "\n")
