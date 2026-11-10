# =============================================================================
#  MODULO 4 — VALUTAZIONE
#  Benchmark e fattori, metriche, Newey-West, attribuzione fattoriale,
#  confronti fra strategie, robustezza e sottoperiodi.
#  Le strategie sono una LISTA NOMINATA di output di backtest
#  (colonne mese_reso, r_lordo, r_netto, turnover): nessun nome cablato.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate) })

# =============================================================================
#  1. FATTORI DI FAMA-FRENCH E TASSO PRIVO DI RISCHIO (con cache)
# =============================================================================
#  Tasso e fattori dalla STESSA fonte: Sharpe, alpha di mercato e alpha
#  multifattoriale restano coerenti fra loro.
scarica_french <- function(zip, base = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/") {
  tmp <- tempfile(fileext = ".zip")
  if (inherits(try(utils::download.file(paste0(base, zip), tmp, mode = "wb", quiet = TRUE),
                   silent = TRUE), "try-error")) return(NULL)
  cart <- tempfile(); dir.create(cart); utils::unzip(tmp, exdir = cart)
  righe <- readLines(list.files(cart, full.names = TRUE)[1], warn = FALSE)
  men <- grepl("^\\s*\\d{6}\\s*,", righe); if (!any(men)) return(NULL)
  ini <- which(men)[1]; fin <- ini
  while (fin <= length(righe) && men[fin]) fin <- fin + 1
  d <- read.csv(text = c(righe[ini - 1], righe[ini:(fin - 1)]), stringsAsFactors = FALSE)
  names(d)[1] <- "aaaamm"
  d <- d |> mutate(mese_reso = as.Date(paste0(trimws(aaaamm), "01"), "%Y%m%d")) |> select(-aaaamm) |>
    mutate(across(where(is.numeric), \(x) x / 100))
  names(d) <- gsub("[^A-Za-z0-9_]", "", names(d)); d
}

carica_fattori <- function(file_cache = "fattori_french.csv",
                           regione = c("US", "Europe", "Developed")) {
  regione <- match.arg(regione)
  if (file.exists(file_cache)) return(leggi(file_cache) |> mutate(mese_reso = as.Date(mese_reso)))
  zip <- switch(regione,
    US        = c(ff3 = "F-F_Research_Data_Factors_CSV.zip", mom = "F-F_Momentum_Factor_CSV.zip",
                  ff5 = "F-F_Research_Data_5_Factors_2x3_CSV.zip"),
    Europe    = c(ff3 = "Europe_3_Factors_CSV.zip", mom = "Europe_Mom_Factor_CSV.zip",
                  ff5 = "Europe_5_Factors_CSV.zip"),
    Developed = c(ff3 = "Developed_3_Factors_CSV.zip", mom = "Developed_Mom_Factor_CSV.zip",
                  ff5 = "Developed_5_Factors_CSV.zip"))
  ff3 <- scarica_french(zip[["ff3"]]); if (is.null(ff3)) stop("Download dei fattori fallito.")
  f <- ff3 |> rename(mkt_rf = MktRF, smb = SMB, hml = HML, rf = RF)
  mm <- scarica_french(zip[["mom"]])
  if (!is.null(mm)) f <- f |> left_join(mm |> select(mese_reso, mom = setdiff(names(mm), "mese_reso")[1]),
                                       by = "mese_reso")
  f5 <- scarica_french(zip[["ff5"]])
  if (!is.null(f5)) f <- f |> left_join(f5 |> select(mese_reso, rmw = RMW, cma = CMA), by = "mese_reso")
  f <- f |> arrange(mese_reso); write.csv(f, file_cache, row.names = FALSE); f
}

#  Benchmark total return da LSEG, con cache. Il benchmark deve essere TOTAL
#  RETURN se i rendimenti delle strategie includono i dividendi.
carica_benchmark_lseg <- function(ric, data_inizio, data_fine, file_cache) {
  if (file.exists(file_cache)) return(leggi(file_cache) |> mutate(mese_reso = as.Date(mese_reso)))
  br <- as.data.frame(Refinitiv::rd_GetHistory(universe = ric, fields = "TR.PriceClose",
          parameters = list("SDate" = data_inizio, "EDate" = data_fine, "Frq" = "M")))
  ch <- gsub("[^a-z]", "", tolower(names(br)))
  b <- data.frame(Data = as.Date(br[[which(grepl("date", ch))[1]]]),
                  liv = suppressWarnings(as.numeric(br[[which(grepl("priceclose", ch))[1]]]))) |>
    filter(!is.na(liv)) |> arrange(Data) |>
    mutate(mese_reso = floor_date(Data, "month"), r_bench = liv / lag(liv) - 1) |>
    select(mese_reso, r_bench)
  write.csv(b, file_cache, row.names = FALSE); b
}

#  Tabella larga: una colonna <nome>_lordo, <nome>_netto, <nome>_tn per strategia.
unisci_serie <- function(strategie, benchmark, fattori) {
  out <- NULL
  for (nm in names(strategie)) {
    s <- strategie[[nm]] |> transmute(mese_reso,
           !!paste0(nm, "_lordo") := r_lordo, !!paste0(nm, "_netto") := r_netto,
           !!paste0(nm, "_tn") := turnover)
    out <- if (is.null(out)) s else full_join(out, s, by = "mese_reso")
  }
  out |> arrange(mese_reso) |> left_join(benchmark, by = "mese_reso") |> left_join(fattori, by = "mese_reso")
}

# =============================================================================
#  2. METRICHE
# =============================================================================
#  Sharpe: media(r - rf) / sd(r - rf) x sqrt(12). Active return sia aritmetico
#  (quello dell'IR) sia geometrico (quello realizzato). Turnover = meta' del
#  movimentato.
metriche <- function(r, r_bench = NULL, turnover = NULL, rf = NULL) {
  r <- as.numeric(r); ok <- !is.na(r); if (sum(ok) < 3) return(NULL)
  rr <- r[ok]; n <- length(rr); cum <- cumprod(1 + rr)
  rfm <- if (is.null(rf)) rep(0, n) else { x <- as.numeric(rf)[ok]; x[is.na(x)] <- 0; x }
  ecc <- rr - rfm
  out <- data.frame(n_mesi = n, rend_annuo = prod(1 + rr)^(12 / n) - 1, volatilita = sd(rr) * sqrt(12),
                    rf_annuo = prod(1 + rfm)^(12 / n) - 1, sharpe = mean(ecc) / sd(ecc) * sqrt(12),
                    max_dd = min(cum / cummax(cum) - 1))
  if (!is.null(turnover)) out$turnover_medio <- mean(as.numeric(turnover)[ok], na.rm = TRUE)
  if (!is.null(r_bench)) {
    rb <- as.numeric(r_bench); o2 <- !is.na(r) & !is.na(rb)
    if (sum(o2) > 3) {
      att <- r[o2] - rb[o2]; n2 <- sum(o2); te <- sd(att) * sqrt(12)
      out$active_aritm <- mean(att) * 12
      out$active_geom  <- prod(1 + r[o2])^(12 / n2) - prod(1 + rb[o2])^(12 / n2)
      out$tracking_err <- te; out$info_ratio <- mean(att) * 12 / te
    }
  }
  out
}

#  Metriche di tutte le strategie, lorde e nette, piu' il benchmark.
tabella_metriche <- function(sb, nomi, versioni = c("lordo", "netto")) {
  righe <- list()
  for (nm in nomi) for (v in versioni)
    righe[[length(righe) + 1]] <- cbind(strategia = paste(nm, v),
      metriche(sb[[paste0(nm, "_", v)]], sb$r_bench, sb[[paste0(nm, "_tn")]], sb$rf))
  bind_rows(righe, cbind(strategia = "Benchmark", metriche(sb$r_bench, NULL, NULL, sb$rf)))
}

# =============================================================================
#  3. REGRESSIONI CON ERRORI NEWEY-WEST
# =============================================================================
regressione_nw <- function(y, X = NULL, lag = NULL, nomi = NULL) {
  y <- as.numeric(y)
  Xm <- if (is.null(X)) matrix(1, length(y), 1) else cbind(1, as.matrix(X))
  colnames(Xm) <- c("alpha", if (!is.null(X)) { if (is.null(nomi)) paste0("b", seq_len(ncol(Xm) - 1)) else nomi })
  ok <- stats::complete.cases(cbind(y, Xm)); y <- y[ok]; Xm <- Xm[ok, , drop = FALSE]
  n <- length(y); k <- ncol(Xm); if (n <= k + 2) return(NULL)
  XtX <- solve(crossprod(Xm)); beta <- XtX %*% crossprod(Xm, y); res <- as.numeric(y - Xm %*% beta)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  u <- Xm * res; S <- crossprod(u) / n
  if (lag > 0) for (l in seq_len(lag)) {
    G <- crossprod(u[(l + 1):n, , drop = FALSE], u[1:(n - l), , drop = FALSE]) / n
    S <- S + (1 - l / (lag + 1)) * (G + t(G))           # kernel di Bartlett
  }
  se <- sqrt(diag(XtX %*% (n * S) %*% XtX)); tt <- as.numeric(beta) / se
  data.frame(termine = colnames(Xm), stima = as.numeric(beta), se_nw = se, t = tt,
             p = 2 * stats::pt(-abs(tt), df = n - k), lag_nw = lag, n_oss = n,
             r2 = 1 - sum(res^2) / sum((y - mean(y))^2), stringsAsFactors = FALSE)
}

stelle <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))

#  Stime ed errori annualizzati con lo STESSO fattore (x12), beta e
#  coefficienti fattoriali no; IC al 95%.
annualizza <- function(d) d |> mutate(
  fattore = ifelse(termine == "alpha", 12, 1),
  stima_a = stima * fattore, se_a = se_nw * fattore,
  ic_basso = stima_a - 1.96 * se_a, ic_alto = stima_a + 1.96 * se_a, sig = stelle(p))

#  (a) active return medio, (b) alpha e beta del modello di mercato.
test_vs_benchmark <- function(sb, nomi, versione = "netto") {
  bind_rows(lapply(nomi, function(nm) {
    r <- sb[[paste0(nm, "_", versione)]]
    t1 <- regressione_nw(r - sb$r_bench)
    t2 <- regressione_nw(r - sb$rf, X = sb$r_bench - sb$rf, nomi = "beta")
    bind_rows(cbind(strategia = paste(nm, versione), test = "Active return medio", t1[t1$termine == "alpha", ]),
              cbind(strategia = paste(nm, versione), test = "Alpha (mercato)",   t2[t2$termine == "alpha", ]),
              cbind(strategia = paste(nm, versione), test = "Beta di mercato",   t2[t2$termine == "beta", ]))
  })) |> annualizza()
}

MODELLI_FATTORIALI <- list(
  "Mercato"           = c("mkt_rf"),
  "Carhart 4 fattori" = c("mkt_rf", "smb", "hml", "mom"),
  "FF5 + momentum"    = c("mkt_rf", "smb", "hml", "rmw", "cma", "mom"))
NOMI_FATTORI <- c(mkt_rf = "mkt", smb = "smb", hml = "hml", mom = "mom", rmw = "rmw", cma = "cma")

#  Attribuzione su lordo e netto: il confronto separa costi e costruzione.
attribuzione <- function(sb, nomi, versioni = c("lordo", "netto"), modelli = MODELLI_FATTORIALI) {
  out <- list()
  for (nm in nomi) for (v in versioni) for (mo in names(modelli)) {
    f <- modelli[[mo]]; if (!all(f %in% names(sb))) next
    reg <- regressione_nw(sb[[paste0(nm, "_", v)]] - sb$rf, X = sb[, f, drop = FALSE], nomi = unname(NOMI_FATTORI[f]))
    if (!is.null(reg)) out[[length(out) + 1]] <- cbind(strategia = paste(nm, v), modello = mo, reg)
  }
  bind_rows(out) |> annualizza()
}

#  Differenze mensili fra coppie di strategie (stessa selezione: isola la
#  pesatura; ottimizzata vs semplici: isola la costruzione).
confronti_coppie <- function(sb, coppie, versione = "netto") {
  bind_rows(lapply(coppie, function(cp) {
    reg <- regressione_nw(sb[[paste0(cp[1], "_", versione)]] - sb[[paste0(cp[2], "_", versione)]])
    cbind(confronto = paste(cp[1], "-", cp[2]), versione = versione, reg[reg$termine == "alpha", ])
  })) |> annualizza()
}

# =============================================================================
#  4. ROBUSTEZZA E SOTTOPERIODI
# =============================================================================
riassumi <- function(serie, etichetta, benchmark, fattori) {
  s <- serie |> left_join(benchmark, by = "mese_reso") |> left_join(fattori |> select(mese_reso, rf), by = "mese_reso")
  m <- metriche(s$r_netto, s$r_bench, s$turnover, s$rf); if (is.null(m)) NULL else cbind(variante = etichetta, m)
}

#  varianti: lista nominata di funzioni senza argomenti che restituiscono un
#  output di backtest. Cosi' qualunque parametro puo' essere fatto variare.
robustezza <- function(varianti, benchmark, fattori) {
  bind_rows(lapply(names(varianti), function(nm) {
    cat("Variante", nm, "...\n"); riassumi(varianti[[nm]](), nm, benchmark, fattori)
  }))
}

sottoperiodi <- function(sb, nomi, periodi, versione = "netto") {
  bind_rows(lapply(names(periodi), function(pn) {
    d <- as.Date(periodi[[pn]]); s <- sb |> filter(mese_reso >= d[1], mese_reso <= d[2])
    if (nrow(s) < 6) return(NULL)
    bind_rows(lapply(nomi, function(nm) cbind(periodo = pn, strategia = paste(nm, versione),
                metriche(s[[paste0(nm, "_", versione)]], s$r_bench, s[[paste0(nm, "_tn")]], s$rf))),
              cbind(periodo = pn, strategia = "Benchmark", metriche(s$r_bench, NULL, NULL, s$rf)))
  }))
}
