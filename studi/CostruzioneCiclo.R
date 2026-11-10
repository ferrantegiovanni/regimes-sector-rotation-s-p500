# =============================================================================
#
#   METODO DI COSTRUZIONE DEL PORTAFOGLIO SULLA ROTAZIONE SETTORIALE
#   Rotazione settoriale sui costituenti storici dell'S&P 500
#
#   Domanda di ricerca:
#     A parita' di segnale, quanto incide il metodo di costruzione del
#     portafoglio su rendimento e rischio?
#
#   E' la domanda classica della costruzione, applicata al segnale di regime. Il segnale resta
#   FISSO: a ogni formazione i titoli candidati sono i membri dei 3 settori
#   scelti dalla rotazione sul ciclo economico (RotazioneCiclo.R). Cambia
#   SOLO il modo di pesarli.
#
# =============================================================================
#
#   COME SI ESEGUE
#   --------------
#   Directory di lavoro = "studi", aperta con il suo
#   .Rproj. Lo script esegue RotazioneCiclo.R fino alla PARTE C4 compresa
#   (pipeline dei dati, settori, regimi, segnale) e ne riusa gli oggetti.
#
#   LEGENDA DELLE SEZIONI
#   ---------------------
#   PARTE K0  Segnale (da RotazioneCiclo.R), parametri
#   PARTE K1  Insieme comune dei titoli e modello di rischio
#   PARTE K2  Metodi di costruzione
#   PARTE K3  Backtest
#   PARTE K4  Valutazione
#   PARTE K5  Robustezza
#   PARTE K6  Tabelle e grafici
#
#   PROTOCOLLO
#   ----------
#     - Stesso segnale, stessi titoli candidati, stesse date, stessi costi
#       per tutti i metodi: ogni differenza e' dovuta alla sola costruzione.
#     - Il modello di rischio e' una covarianza con shrinkage di Ledoit-Wolf
#       (2004), stimata sui soli mesi fino alla formazione.
#     - Per pesare tutti i metodi sullo STESSO insieme di titoli, si usano i
#       candidati con storia completa nella finestra di stima, cosi' che la
#       covarianza sia stimata sugli stessi mesi per tutti. Quanti ne restano fuori e'
#       riportato; l'effetto della restrizione e' misurato nella PARTE K5.
#
# =============================================================================




# #############################################################################
# ##  PARTE K0 — SEGNALE E PARAMETRI                                        ##
# #############################################################################
if (!file.exists("RotazioneCiclo.R"))
  stop("RotazioneCiclo.R non trovato: imposta la directory di lavoro sulla ",
       "cartella \"studi\".")
.righe_rc <- readLines("RotazioneCiclo.R", encoding = "UTF-8")
.fine_rc  <- grep("^# ##  PARTE C5", .righe_rc)[1] - 2
if (is.na(.fine_rc)) stop("Intestazione della PARTE C5 non trovata in RotazioneCiclo.R.")
eval(parse(text = .righe_rc[seq_len(.fine_rc)], encoding = "UTF-8"), envir = globalenv())
rm(.righe_rc, .fine_rc)
cat("\n========== SEGNALE CARICATO (rotazione sul ciclo) ==========\n\n")

# -----------------------------------------------------------------------------
#  PARAMETRI — concordati il 2026-11-10, prima di qualunque risultato
# -----------------------------------------------------------------------------
#  Segnale: rotazione sul ciclo (2 fasi), specificazione principale di
#  RotazioneCiclo.R. Metodi: settori equipesati con pesi di capitalizzazione
#  al loro interno (il riferimento dello studio precedente), EW e VW sui
#  titoli, inverse volatility, minima varianza (GMV), equal risk
#  contribution (ERC). Specificazione principale SENZA tetto ai pesi; tetto
#  del 5% per titolo come variante (PARTE K5).
SEGNALE      <- pesi_strategie$CICLO     # settori scelti a ogni formazione
FIN_RISCHIO  <- 60       # mesi della finestra di stima della covarianza
MIN_MESI_RISCHIO <- 24   # sotto questa storia la stima non si fa
TETTO        <- 0.05     # peso massimo per titolo nella variante vincolata

dir.create("tabelle_costruzione", showWarnings = FALSE)
dir.create("grafici_costruzione", showWarnings = FALSE)


# #############################################################################
# ##  PARTE K1 — INSIEME COMUNE DEI TITOLI E MODELLO DI RISCHIO             ##
# #############################################################################
#  Rendimenti mensili di tutte le security, storia intera (protocollo I2):
#  un titolo entrato da poco nell'indice ha comunque la sua storia di prezzi.
R_tutti <- bt_reg |> filter(!is.na(r)) |> select(periodo, sec_id, r) |>
  pivot_wider(names_from = sec_id, values_from = r) |> arrange(periodo)
periodi_r <- R_tutti$periodo
R_tutti <- as.matrix(R_tutti[, -1])
stopifnot(all(periodi_r[-1] == periodi_r[-length(periodi_r)] %m+% months(1)))

#  Ledoit & Wolf (2004): combinazione convessa della covarianza campionaria e
#  dell'identita' riscalata, con intensita' ottimale in forma chiusa.
shrink_lw <- function(R) {
  R  <- as.matrix(R)
  T_ <- nrow(R); N <- ncol(R)
  X  <- scale(R, center = TRUE, scale = FALSE)
  S  <- crossprod(X) / T_
  m  <- sum(diag(S)) / N
  F_ <- diag(m, N)
  d2 <- sum((S - F_)^2) / N
  b2 <- (sum(rowSums(X^2)^2) - 2 * sum((X %*% S) * X) + T_ * sum(S^2)) / (N * T_^2)
  b2 <- min(b2, d2)
  delta <- if (d2 > 0) b2 / d2 else 0
  Sig <- delta * F_ + (1 - delta) * S
  attr(Sig, "shrinkage") <- delta
  Sig
}

#  A ogni formazione t: candidati = membri dei settori scelti; finestra =
#  ultimi FIN_RISCHIO mesi fino a t compreso (i rendimenti del mese t sono
#  noti a fine t). Insieme comune = candidati con storia completa.
prepara_data <- function(tt) {
  t <- as.Date(tt)
  W <- SEGNALE[[tt]]
  cand <- per_periodo_mb[[tt]]
  cand <- cand[cand$settore %in% names(W)[W > 0], c("sec_id", "settore", "mktcap", "r_next")]
  i_fin <- match(t, periodi_r)
  righe <- max(1, i_fin - FIN_RISCHIO + 1):i_fin
  if (length(righe) < MIN_MESI_RISCHIO) stop("Storia insufficiente per il rischio a ", tt)
  M <- R_tutti[righe, intersect(cand$sec_id, colnames(R_tutti)), drop = FALSE]
  M <- M[, colSums(is.na(M)) == 0, drop = FALSE]
  com <- cand[cand$sec_id %in% colnames(M), ]
  M <- M[, com$sec_id, drop = FALSE]
  Sig <- shrink_lw(M); dimnames(Sig) <- list(com$sec_id, com$sec_id)
  list(t = t, settori = W, cand = cand, com = com, Sig = Sig,
       vol = apply(M, 2, sd), mesi = length(righe),
       esclusi = nrow(cand) - nrow(com),
       quota_cap_esclusa = 1 - sum(com$mktcap) / sum(cand$mktcap))
}

cat("Stima del rischio a ogni formazione...\n")
DATI_K <- lapply(names(SEGNALE), prepara_data)
names(DATI_K) <- names(SEGNALE)

esclusioni <- bind_rows(lapply(DATI_K, function(d) data.frame(
  periodo = d$t, candidati = nrow(d$cand), comuni = nrow(d$com), esclusi = d$esclusi,
  quota_cap_esclusa = d$quota_cap_esclusa, mesi = d$mesi,
  shrinkage = attr(d$Sig, "shrinkage"))))
cat("\n========== INSIEME COMUNE ==========\n")
cat("Candidati per formazione: mediana", median(esclusioni$candidati),
    "| nell'insieme comune: mediana", median(esclusioni$comuni), "| min", min(esclusioni$comuni), "\n")
cat("Esclusi per storia incompleta: mediana", median(esclusioni$esclusi), "| max", max(esclusioni$esclusi),
    "| quota di capitalizzazione esclusa: mediana",
    sprintf("%.1f%%", 100 * median(esclusioni$quota_cap_esclusa)), "max",
    sprintf("%.1f%%", 100 * max(esclusioni$quota_cap_esclusa)), "\n")
cat("Intensita' di shrinkage di Ledoit-Wolf: mediana", round(median(esclusioni$shrinkage), 3), "\n")
if (min(esclusioni$comuni) * TETTO < 1) stop("Troppo pochi titoli per il tetto del ", TETTO)


# #############################################################################
# ##  PARTE K2 — METODI DI COSTRUZIONE                                      ##
# #############################################################################
#  Ogni metodo: function(d) -> pesi nominati per sec_id, non negativi, somma 1.

#  Riferimento dello studio precedente: settori equipesati, pesi di
#  capitalizzazione al loro interno.
pesi_sett_vw <- function(d) {
  W <- d$settori[d$com$settore]
  setNames(W * d$com$mktcap / ave(d$com$mktcap, d$com$settore, FUN = sum), d$com$sec_id)
}
pesi_ew_k  <- function(d) setNames(rep(1 / nrow(d$com), nrow(d$com)), d$com$sec_id)
pesi_vw_k  <- function(d) setNames(d$com$mktcap / sum(d$com$mktcap), d$com$sec_id)

#  Inverse volatility: deviazione standard CAMPIONARIA della finestra. Lo
#  shrinkage di Ledoit-Wolf verso un bersaglio scalare comprime le varianze
#  verso la media e appiattirebbe i pesi; le varianze singole sono stimate
#  bene anche senza shrinkage.
pesi_iv <- function(d) { v <- 1 / d$vol[d$com$sec_id]; v / sum(v) }

#  Minima varianza globale, long-only, con tetto facoltativo:
#     min w' S w   s.t.  sum(w) = 1,  0 <= w <= tetto
pesi_gmv <- function(d, tetto = NULL) {
  S <- d$Sig; n <- ncol(S)
  D <- (S + t(S)) / 2; diag(D) <- diag(D) + 1e-10
  A <- cbind(rep(1, n), diag(n)); b <- c(1, rep(0, n))
  if (!is.null(tetto)) { A <- cbind(A, -diag(n)); b <- c(b, rep(-tetto, n)) }
  sol <- solve.QP(Dmat = D, dvec = rep(0, n), Amat = A, bvec = b, meq = 1)
  w <- pmax(sol$solution, 0); setNames(w / sum(w), colnames(S))
}

#  Equal risk contribution (Maillard, Roncalli & Teiletche 2010), con la
#  discesa ciclica per coordinate di Griveau-Billion, Richard & Roncalli
#  (2013): si risolve  min 0.5 y'Sy - sum(b_i log y_i), poi w = y / sum(y).
pesi_erc <- function(d, tetto = NULL, tol = 1e-10, max_iter = 5000) {
  S <- d$Sig; n <- ncol(S); b <- rep(1 / n, n)
  y <- 1 / sqrt(diag(S)); y <- y / sum(y)
  Sy <- as.numeric(S %*% y)
  for (it in seq_len(max_iter)) {
    y_prec <- y
    for (i in seq_len(n)) {
      a <- S[i, i]; c_ <- Sy[i] - a * y[i]
      nuovo <- (-c_ + sqrt(c_^2 + 4 * a * b[i])) / (2 * a)
      Sy <- Sy + S[, i] * (nuovo - y[i]); y[i] <- nuovo
    }
    if (max(abs(y - y_prec)) < tol * max(abs(y))) break
  }
  w <- setNames(y / sum(y), colnames(S))
  rc <- w * as.numeric(S %*% w); rc <- rc / sum(rc)
  if (max(abs(rc - 1 / n)) > 1e-4) stop("ERC non convergente a ", format(d$t))
  if (is.null(tetto)) w else applica_tetto(w, tetto)
}

#  Tetto per i metodi non ottimizzati con vincolo esplicito: l'eccedenza
#  si redistribuisce in proporzione ai pesi dei titoli sotto il tetto.
applica_tetto <- function(w, tetto) {
  for (k in seq_len(200)) {
    sopra <- w > tetto + 1e-12
    if (!any(sopra)) break
    ecc <- sum(w[sopra] - tetto); w[sopra] <- tetto
    liberi <- w < tetto - 1e-12
    w[liberi] <- w[liberi] + ecc * w[liberi] / sum(w[liberi])
  }
  w
}

METODI <- list(
  SETT_VW = list(etichetta = "Settori EW, cap al loro interno", f = pesi_sett_vw),
  EW      = list(etichetta = "Equal-weighted",                  f = pesi_ew_k),
  VW      = list(etichetta = "Cap-weighted",                    f = pesi_vw_k),
  IV      = list(etichetta = "Inverse volatility",              f = pesi_iv),
  ERC     = list(etichetta = "Equal risk contribution",         f = function(d) pesi_erc(d)),
  GMV     = list(etichetta = "Minima varianza",                 f = function(d) pesi_gmv(d)))


# #############################################################################
# ##  PARTE K3 — BACKTEST                                                   ##
# #############################################################################
#  Stessa contabilita' del motore del modulo 02: rendimento in t+1 con i
#  rendimenti di delisting, turnover sui pesi derivati, costo per lato.
#  Si registrano anche le caratteristiche del portafoglio a ogni data.
esegui_costruzione <- function(f_pesi, costo_unitario = COSTO_UNITARIO) {
  w_prec <- setNames(numeric(0), character(0))
  righe <- vector("list", length(DATI_K))
  for (i in seq_along(DATI_K)) {
    d <- DATI_K[[i]]; tt <- names(DATI_K)[i]
    w <- f_pesi(d)
    stopifnot(abs(sum(w) - 1) < 1e-8, all(w >= -1e-12), setequal(names(w), d$com$sec_id))
    rn <- setNames(d$com$r_next, d$com$sec_id)[names(w)]
    if (anyNA(rn)) stop("Rendimento in t+1 mancante a ", tt)
    tut <- per_periodo_bt[[tt]]
    if (length(w_prec) > 0) {
      rr <- setNames(tut$r, tut$sec_id)[names(w_prec)]; rr[is.na(rr)] <- 0
      w_drift <- w_prec * (1 + rr); w_drift <- w_drift / sum(w_drift)
    } else w_drift <- setNames(numeric(0), character(0))
    tutti <- union(names(w), names(w_drift))
    a <- setNames(rep(0, length(tutti)), tutti); a[names(w)]       <- w
    b <- setNames(rep(0, length(tutti)), tutti); b[names(w_drift)] <- w_drift
    scambiato <- sum(abs(a - b))
    r_l <- sum(w * rn)
    peso_sett <- tapply(w, d$com$settore[match(names(w), d$com$sec_id)], sum)
    righe[[i]] <- data.frame(
      formazione = d$t, mese_reso = d$t %m+% months(1),
      n_titoli = sum(w > 1e-6), n_effettivo = 1 / sum(w^2), peso_max = max(w),
      peso_sett_max = max(peso_sett),
      vol_ex_ante = sqrt(as.numeric(t(w) %*% d$Sig[names(w), names(w)] %*% w) * 12),
      r_lordo = r_l, costo = scambiato * costo_unitario,
      r_netto = r_l - scambiato * costo_unitario, turnover = scambiato / 2)
    w_prec <- w
  }
  bind_rows(righe)
}

cat("\nCostruzione dei portafogli...\n")
serie_k <- lapply(METODI, function(m) esegui_costruzione(m$f))
stopifnot(all(vapply(serie_k, function(s) identical(s$mese_reso, serie_k[[1]]$mese_reso), logical(1))))
cat("Mesi:", nrow(serie_k[[1]]), "| metodi:", length(serie_k), "\n")


# #############################################################################
# ##  PARTE K4 — VALUTAZIONE                                                ##
# #############################################################################
STRAT_K <- vapply(METODI, function(m) m$etichetta, character(1))

tab_k <- serie_k[[1]] |> select(formazione, mese_reso)
for (k in names(serie_k))
  tab_k <- tab_k |> left_join(serie_k[[k]] |> select(mese_reso, !!paste0(k, "_lordo") := r_lordo,
                                                     !!paste0(k, "_netto") := r_netto,
                                                     !!paste0(k, "_tn") := turnover), by = "mese_reso")
tab_k <- tab_k |> left_join(benchmark, by = "mese_reso") |> left_join(fattori, by = "mese_reso")
stopifnot(!anyNA(tab_k$r_bench), !anyNA(tab_k$rf), !anyNA(tab_k$mom))

#  Coerenza con lo studio precedente: sullo stesso insieme di titoli il
#  riferimento SETT_VW non coincide con la rotazione sul ciclo (che usa tutti
#  i candidati); la differenza e' l'effetto della restrizione, in PARTE K5.

metriche_k <- bind_rows(
  lapply(names(STRAT_K), function(k) bind_rows(lapply(c("lordo", "netto"), function(v)
    cbind(strategia = STRAT_K[[k]], codice = k, versione = v,
          metriche(tab_k[[paste0(k, "_", v)]], tab_k$r_bench, tab_k[[paste0(k, "_tn")]], tab_k$rf))))),
  cbind(strategia = "Benchmark", codice = "BENCH", versione = "", metriche(tab_k$r_bench, NULL, NULL, tab_k$rf)))

#  Caratteristiche medie dei portafogli e beta di mercato.
caratt_k <- bind_rows(lapply(names(serie_k), function(k) {
  s <- serie_k[[k]]
  reg <- regressione_nw(tab_k[[paste0(k, "_netto")]] - tab_k$rf, X = tab_k[, "mkt_rf", drop = FALSE], nomi = "mkt")
  data.frame(codice = k, strategia = STRAT_K[[k]],
             n_titoli = mean(s$n_titoli), n_effettivo = mean(s$n_effettivo),
             peso_max = mean(s$peso_max), peso_sett_max = mean(s$peso_sett_max),
             vol_ex_ante = mean(s$vol_ex_ante), vol_realizzata = sd(s$r_lordo) * sqrt(12),
             beta = reg$stima[reg$termine == "mkt"])
}))

cat("\n========== METRICHE ==========\n")
print(as.data.frame(metriche_k |> select(strategia, versione, rend_annuo, volatilita, sharpe,
                                         max_dd, turnover_medio, active_geom) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
cat("\n========== CARATTERISTICHE DEI PORTAFOGLI (medie) ==========\n")
print(as.data.frame(caratt_k |> select(-codice) |> mutate(across(where(is.numeric), \(x) round(x, 3)))),
      row.names = FALSE)

#  Significativita': ogni metodo contro EW (una differenza: la regola dei
#  pesi), al lordo e al netto; contro il benchmark, alpha di Carhart netto.
FATT_K <- c("mkt_rf", "smb", "hml", "mom")
test_k <- bind_rows(lapply(names(STRAT_K), function(k) {
  y  <- tab_k[[paste0(k, "_netto")]]
  t1 <- regressione_nw(y - tab_k$r_bench)
  t2 <- regressione_nw(y - tab_k$rf, X = tab_k[, FATT_K], nomi = c("mkt", "smb", "hml", "mom"))
  bind_rows(cbind(strategia = STRAT_K[[k]], test = "Active return medio", t1[t1$termine == "alpha", ]),
            cbind(strategia = STRAT_K[[k]], test = "Alpha Carhart", t2[t2$termine == "alpha", ]))
})) |> mutate(stima_a = stima * 12, se_a = se_nw * 12, sig = stelle(p))

confronti_k <- bind_rows(lapply(setdiff(names(STRAT_K), "EW"), function(k)
  bind_rows(lapply(c("lordo", "netto"), function(v) {
    d <- regressione_nw(tab_k[[paste0(k, "_", v)]] - tab_k[[paste0("EW_", v)]])
    cbind(confronto = paste(STRAT_K[[k]], "- Equal-weighted"), versione = v, d[d$termine == "alpha", ])
  })))) |> mutate(annua = stima * 12, se_a = se_nw * 12, sig = stelle(p))

#  Rischio: la volatilita' realizzata di ogni metodo contro EW. Test di
#  uguaglianza delle varianze per serie appaiate (Pitman-Morgan): la
#  correlazione fra somma e differenza delle due serie e' nulla se e solo
#  se le varianze coincidono.
pitman_morgan <- function(x, y) {
  ct <- cor.test(x + y, x - y)
  data.frame(rapporto_vol = sd(x) / sd(y), t = unname(ct$statistic), p = ct$p.value)
}
rischio_k <- bind_rows(lapply(setdiff(names(STRAT_K), "EW"), function(k)
  cbind(confronto = paste(STRAT_K[[k]], "- Equal-weighted"),
        pitman_morgan(tab_k[[paste0(k, "_netto")]], tab_k$EW_netto)))) |> mutate(sig = stelle(p))

cat("\n========== SIGNIFICATIVITA' (Newey-West) ==========\n")
print(as.data.frame(test_k |> select(strategia, test, stima_a, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
cat("\nDifferenze di rendimento contro EW:\n")
print(as.data.frame(confronti_k |> select(confronto, versione, annua, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
cat("\nVolatilita' contro EW (Pitman-Morgan, netto):\n")
print(as.data.frame(rischio_k |> mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)

SOTTOPERIODI <- list("2005-2007 Pre-crisis"       = c("2005-01-01", "2007-12-31"),
                     "2008-2009 Financial crisis" = c("2008-01-01", "2009-12-31"),
                     "2010-2019 Expansion"        = c("2010-01-01", "2019-12-31"),
                     "2020-2026 Post-COVID"       = c("2020-01-01", "2026-12-31"))
sottoperiodi_k <- bind_rows(lapply(names(SOTTOPERIODI), function(sp) {
  d <- tab_k |> filter(mese_reso >= as.Date(SOTTOPERIODI[[sp]][1]), mese_reso <= as.Date(SOTTOPERIODI[[sp]][2]))
  bind_rows(lapply(names(STRAT_K), function(k)
    cbind(periodo = sp, strategia = STRAT_K[[k]], metriche(d[[paste0(k, "_netto")]], d$r_bench, NULL, d$rf))),
    cbind(periodo = sp, strategia = "Benchmark", metriche(d$r_bench, NULL, NULL, d$rf)))
}))


# #############################################################################
# ##  PARTE K5 — ROBUSTEZZA                                                 ##
# #############################################################################
#  (a) Tetto del 5% per titolo sui metodi che possono concentrarsi.
#  (b) Effetto della restrizione all'insieme comune: la rotazione sul ciclo
#      dello studio precedente (tutti i candidati) contro lo stesso metodo
#      sull'insieme comune.
varianti_k <- list(
  "Cap-weighted, 5% cap"          = function(d) applica_tetto(pesi_vw_k(d), TETTO),
  "Equal risk contribution, 5% cap" = function(d) pesi_erc(d, TETTO),
  "Minimum variance, 5% cap"      = function(d) pesi_gmv(d, TETTO))
robustezza_k <- bind_rows(
  bind_rows(lapply(names(varianti_k), function(nm) {
    s <- esegui_costruzione(varianti_k[[nm]])
    d <- s |> left_join(tab_k |> select(mese_reso, r_bench, rf), by = "mese_reso")
    cbind(gruppo = "5% weight cap", variante = nm, metriche(d$r_netto, d$r_bench, d$turnover, d$rf),
          n_effettivo = mean(s$n_effettivo), peso_max = mean(s$peso_max))
  })),
  {
    d <- serie_rot$CICLO |> left_join(tab_k |> select(mese_reso, r_bench, rf), by = "mese_reso")
    cbind(gruppo = "Common set", variante = "Previous study: all candidates (sectors EW, cap within)",
          metriche(d$r_netto, d$r_bench, d$turnover, d$rf), n_effettivo = NA, peso_max = NA)
  },
  cbind(gruppo = "Common set", variante = "This study: common set (sectors EW, cap within)",
        metriche(tab_k$SETT_VW_netto, tab_k$r_bench, tab_k$SETT_VW_tn, tab_k$rf),
        n_effettivo = mean(serie_k$SETT_VW$n_effettivo), peso_max = mean(serie_k$SETT_VW$peso_max)))
cat("\n========== ROBUSTEZZA (netto) ==========\n")
print(as.data.frame(robustezza_k |> select(gruppo, variante, rend_annuo, volatilita, sharpe,
                                           turnover_medio, n_effettivo, peso_max) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)


# #############################################################################
# ##  PARTE K6 — TABELLE E GRAFICI                                          ##
# #############################################################################
TRAD_K <- c("Settori EW, cap al loro interno" = "Sectors EW, cap-weighted within",
            "Equal-weighted" = "Equal-weighted", "Cap-weighted" = "Cap-weighted",
            "Inverse volatility" = "Inverse volatility", "Equal risk contribution" = "Equal risk contribution",
            "Minima varianza" = "Minimum variance", "Benchmark" = "Benchmark (S&P 500 TR)",
            "lordo" = "Gross of costs", "netto" = "Net of costs",
            "Active return medio" = "Mean active return", "Alpha Carhart" = "Carhart alpha")
en_k <- function(x) { x <- as.character(x); ifelse(x %in% names(TRAD_K), unname(TRAD_K[x]), x) }
en_meno <- function(x) paste(en_k(sub(" - .*$", "", x)), "minus", en_k(sub("^.* - ", "", x)))
pct <- function(x, d = 2) ifelse(is.na(x), "", paste0(formatC(100*x, format="f", digits=d), "%"))
num <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format="f", digits=d))

tabK1 <- metriche_k |> transmute(
  Strategy = en_k(strategia), Version = en_k(versione), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
  `Average monthly turnover` = pct(turnover_medio),
  `Active return (geometric)` = pct(active_geom), `Tracking error` = pct(tracking_err),
  `Information ratio` = num(info_ratio))

tabK2 <- caratt_k |> transmute(
  Strategy = en_k(strategia), `Stocks held` = num(n_titoli, 0), `Effective number of stocks` = num(n_effettivo, 1),
  `Largest weight` = pct(peso_max, 1), `Largest sector weight` = pct(peso_sett_max, 1),
  `Ex-ante volatility` = pct(vol_ex_ante, 1), `Realized volatility (gross)` = pct(vol_realizzata, 1),
  `Market beta` = num(beta, 2))

tabK3 <- bind_rows(
  test_k |> transmute(Group = "Versus benchmark, net of costs", Item = paste0(en_k(strategia), ": ", en_k(test)),
                      Estimate = pct(stima_a), `Std. error (Newey-West)` = pct(se_a),
                      `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig),
  confronti_k |> transmute(Group = "Return difference versus equal-weighted",
                           Item = paste0(en_meno(confronto), ", ", en_k(versione)),
                           Estimate = pct(annua), `Std. error (Newey-West)` = pct(se_a),
                           `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig),
  rischio_k |> transmute(Group = "Volatility versus equal-weighted (Pitman-Morgan), net",
                         Item = en_meno(confronto), Estimate = paste0("ratio ", num(rapporto_vol, 3)),
                         `Std. error (Newey-West)` = "", `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig))

tabK4 <- sottoperiodi_k |> transmute(
  Subperiod = periodo, Strategy = en_k(strategia), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd))

tabK5 <- robustezza_k |> transmute(
  Group = gruppo, Variant = variante, `Annualized return` = pct(rend_annuo),
  `Annualized volatility` = pct(volatilita), `Sharpe ratio` = num(sharpe),
  `Average monthly turnover` = pct(turnover_medio),
  `Effective number of stocks` = num(n_effettivo, 1), `Largest weight` = pct(peso_max, 1))

nomi_tabK <- c("table_K1_performance", "table_K2_portfolio_characteristics", "table_K3_significance",
               "table_K4_subperiods", "table_K5_robustness")
for (i in 1:5) write.csv(get(paste0("tabK", i)), paste0("tabelle_costruzione/", nomi_tabK[i], ".csv"),
                         row.names = FALSE, na = "")
write_xlsx(list(Performance = tabK1, Characteristics = tabK2, Significance = tabK3,
                Subperiods = tabK4, Robustness = tabK5), "tabelle_costruzione/construction_tables.xlsx")

# =============================== GRAFICI =====================================
ETI_K <- c(en_k(STRAT_K), "Benchmark (S&P 500 TR)")
COL_K <- setNames(c("#7B3F99", "#1F4E79", "#C55A11", "#B8860B", "#2E7D32", "#B23A2E", "#7F7F7F"), ETI_K)
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        plot.caption = element_text(colour = "grey45", size = 8, hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9, colour = "grey30"))
mese_en <- function(d) paste(month.name[month(d)], year(d))
PERIODO_K <- paste(mese_en(min(tab_k$mese_reso)), "to", mese_en(max(tab_k$mese_reso)))
DIDA_K <- "Same stocks for every method: members of the 3 sectors chosen by the business-cycle rotation with full 60-month history; monthly rebalancing, 10 bp per side."

ricchezza <- tab_k |> select(mese_reso, r_bench, all_of(paste0(names(STRAT_K), "_netto"))) |>
  pivot_longer(-mese_reso, names_to = "codice", values_to = "r") |>
  mutate(serie = ifelse(codice == "r_bench", "Benchmark (S&P 500 TR)",
                        en_k(STRAT_K[sub("_netto$", "", codice)])),
         serie = factor(serie, levels = ETI_K)) |>
  group_by(serie) |> arrange(mese_reso) |>
  mutate(valore = 100 * cumprod(1 + r), dd = valore / cummax(pmax(valore, 100)) - 1) |> ungroup()

ggsave("grafici_costruzione/figure_K1_wealth.png",
       ggplot(ricchezza, aes(mese_reso, valore, colour = serie)) +
         geom_line(linewidth = .7) + scale_colour_manual(values = COL_K) +
         scale_y_log10(labels = label_number(accuracy = 1)) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Growth of 100 invested, by construction method",
              subtitle = paste0("Net of transaction costs, log scale, ", PERIODO_K),
              x = NULL, y = "Value (base 100)", caption = DIDA_K) +
         tema + guides(colour = guide_legend(nrow = 2)),
       width = 9, height = 5.4, dpi = 300)

ggsave("grafici_costruzione/figure_K2_drawdown.png",
       ggplot(ricchezza, aes(mese_reso, dd, colour = serie)) +
         geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
         geom_line(linewidth = .6) + scale_colour_manual(values = COL_K) +
         scale_y_continuous(labels = percent_format(accuracy = 1)) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Drawdown from previous peak", subtitle = paste0("Net of transaction costs, ", PERIODO_K),
              x = NULL, y = NULL) + tema + guides(colour = guide_legend(nrow = 2)),
       width = 9, height = 4.8, dpi = 300)

#  Etichette alternate sopra e sotto i punti, in ordine di volatilita':
#  i metodi hanno rendimenti simili e le etichette si sovrapporrebbero.
punti <- metriche_k |> filter(versione != "lordo") |>
  mutate(serie = factor(en_k(strategia), levels = ETI_K)) |>
  arrange(volatilita) |> mutate(vjust = ifelse(row_number() %% 2 == 0, 2.2, -1.2))
ggsave("grafici_costruzione/figure_K3_risk_return.png",
       ggplot(punti, aes(volatilita, rend_annuo, colour = serie)) +
         geom_point(size = 4) +
         geom_text(aes(label = serie, vjust = vjust), size = 3.2, show.legend = FALSE) +
         scale_colour_manual(values = COL_K) +
         scale_x_continuous(labels = percent_format(accuracy = .1), expand = expansion(mult = .2)) +
         scale_y_continuous(labels = percent_format(accuracy = .1), expand = expansion(mult = .2)) +
         labs(title = "Risk and return by construction method",
              subtitle = paste0("Net of transaction costs, ", PERIODO_K),
              x = "Annualized volatility", y = "Annualized return") +
         tema + theme(legend.position = "none"),
       width = 8, height = 5.8, dpi = 300)

concentrazione <- bind_rows(lapply(names(serie_k), function(k)
  serie_k[[k]] |> transmute(formazione, n_effettivo, serie = factor(en_k(STRAT_K[[k]]), levels = ETI_K))))
ggsave("grafici_costruzione/figure_K4_effective_stocks.png",
       ggplot(concentrazione, aes(formazione, n_effettivo, colour = serie)) +
         geom_line(linewidth = .6) + scale_colour_manual(values = COL_K) +
         scale_y_log10() +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Effective number of stocks held",
              subtitle = "Inverse of the sum of squared weights at each formation date, log scale",
              x = NULL, y = NULL) + tema + guides(colour = guide_legend(nrow = 2)),
       width = 9, height = 4.8, dpi = 300)

cat("\n========== TABELLA PRINCIPALE ==========\n")
print(as.data.frame(tabK1 |> filter(Version != "Gross of costs")), row.names = FALSE)
cat("\nStudio completato: tabelle_costruzione/ e grafici_costruzione/.\n")
