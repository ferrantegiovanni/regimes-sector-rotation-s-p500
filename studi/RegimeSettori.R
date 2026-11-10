# =============================================================================
#
#   ROTAZIONE SETTORIALE GUIDATA DAL REGIME DI MERCATO
#   Rotazione settoriale sui costituenti storici dell'S&P 500
#
#   Domanda di ricerca:
#     Un segnale di regime di mercato, identificato fuori campione, migliora
#     l'allocazione fra i settori dell'S&P 500 rispetto a una rotazione che
#     ignora il regime?
#
# =============================================================================
#
#   COME SI ESEGUE
#   --------------
#   Directory di lavoro = questa cartella ("studi"):
#   basta aprire il progetto .Rproj che contiene. Tutto il file in una volta
#   (Ctrl+Shift+Enter).
#
#   Lo script esegue ../R/00_universo.R, che con i moduli generici in ../R
#   costruisce universo e dataset dai file in ../dati: pannello, bt_reg
#   (rendimenti in t+1 con la regola sui delisting gia' applicata),
#   benchmark, fattori; i moduli forniscono anche metriche() e
#   regressione_nw().
#
#   Al primo giro serve Workspace aperto per scaricare il settore dei
#   titoli (PARTE R2). La stima espandente dell'HMM impiega qualche minuto ed
#   e' messa in cache in regimi_hmm.rds; si rifa' da sola se cambiano i
#   parametri o i dati di mercato.
#
# -----------------------------------------------------------------------------
#
#   LEGENDA DELLE SEZIONI
#   ---------------------
#   PARTE R0  Pipeline dei dati, parametri dello studio
#   PARTE R1  Serie di mercato per il regime
#   PARTE R2  Settori TRBC dei titoli
#   PARTE R3  Rendimenti settoriali point-in-time
#   PARTE R4  Hidden Markov model, stima espandente
#   PARTE R5  Regimi da regole (media mobile, trend)
#   PARTE R6  Previsione condizionata al regime e rotazione
#   PARTE R7  Valutazione
#   PARTE R8  Robustezza
#   PARTE R9  Tabelle e grafici
#
# -----------------------------------------------------------------------------
#
#   PROTOCOLLO TEMPORALE (anti look-ahead)
#   --------------------------------------
#   A fine mese t si conoscono: i rendimenti fino al mese t compreso, la
#   membership a t, la capitalizzazione a t. Ogni quantita' usata a t per
#   decidere i pesi e' calcolata SOLO con questa informazione:
#
#     1. HMM: parametri ristimati a ogni t sulla storia di mercato fino a t
#        (finestra espandente). Si usano le probabilita' FILTRATE
#        P(S_t | F_t), mai quelle smussate, che userebbero dati successivi;
#        la previsione per t+1 e' P(S_t+1 | F_t) = P(S_t | F_t) x A.
#     2. Regole: media mobile e trend calcolati con i livelli fino a t.
#     3. Rendimento atteso di ciascun settore in ciascuno stato: media dei
#        rendimenti settoriali realizzati FINO a t, pesata con le probabilita'
#        di stato prodotte IN TEMPO REALE (a ogni tau con il modello stimato a
#        tau). Nessuna probabilita' viene ricalcolata a posteriori.
#     4. Portafoglio formato a fine t, rendimento in t+1, costi sui pesi
#        derivati (timeline del modulo 02).
#
#   FILE PRODOTTI
#   -------------
#     settori_lseg.csv        settori GICS e TRBC per RIC, congelati  PARTE R2
#     regimi_hmm.rds          stime HMM espandenti (cache)        PARTE R4
#     tabelle_regime/         tabelle in inglese, csv e xlsx      PARTE R9
#     grafici_regime/         grafici a 300 dpi                   PARTE R9
#
# =============================================================================




# #############################################################################
# ##  PARTE R0 — PIPELINE DEI DATI E PARAMETRI                              ##
# #############################################################################
#  Universo e dataset dei costituenti: ../R/00_universo.R, con i moduli
#  generici e i dati congelati in ../dati (fuori dal repository).
cat("Directory di lavoro:", getwd(), "\n")
DIR_PROGETTO <- ".."
if (!file.exists(file.path(DIR_PROGETTO, "R", "00_universo.R")))
  stop("../R/00_universo.R non trovato: imposta la directory di lavoro sulla ",
       "cartella \"studi\".")
source(file.path(DIR_PROGETTO, "R", "00_universo.R"), encoding = "UTF-8")

# -----------------------------------------------------------------------------
#  PARAMETRI DELLO STUDIO — fissati prima di vedere i risultati
# -----------------------------------------------------------------------------
#  Specifica confermata il 2026-10-31, prima del download dei settori GICS e
#  quindi prima di qualunque risultato della rotazione. Ogni modifica
#  successiva va registrata nel registro delle decisioni con la motivazione.
#  Modifiche successive: 2026-11-01, classificazione da GICS a TRBC per
#  copertura insufficiente dei titoli usciti (motivazione nella PARTE R2).
FILE_SETTORI     <- "settori_lseg.csv"   # GICS e TRBC per RIC; si usa il TRBC (PARTE R2)
FILE_HMM         <- "regimi_hmm.rds"

#  Mercato per il regime: Kenneth French, Mkt-RF e RF, dal 1926. La serie e'
#  il rendimento di un indice value-weighted di tutto il mercato USA, che a
#  fine mese t e' osservabile; la lunga storia serve a stimare l'HMM con
#  molti cicli di mercato gia' dalla prima data di formazione.
INIZIO_STIMA_HMM <- "1926-07-01"
K_STATI          <- 2       # stati dell'HMM: calmo / turbolento
RIFIT_COMPLETO   <- 12      # ogni 12 mesi la stima riparte anche da valori standard
SIG_MIN          <- 0.005   # pavimento della volatilita' mensile di stato

#  Regole (letteratura):
#    media mobile a 10 mesi  — Faber (2007)
#    rendimento di mercato a 24 mesi negativo — Daniel & Moskowitz (2016)
MESI_SMA         <- 10
MESI_TREND       <- 24

#  Rotazione
INIZIO_SETTORI   <- "2003-01-01"   # prima formazione con rendimento settoriale
N_TOP            <- 3              # settori detenuti, equipesati fra loro
PRIOR_MESI       <- 12             # shrinkage della media di stato verso la
                                   # media incondizionata del settore

dir.create("tabelle_regime", showWarnings = FALSE)
dir.create("grafici_regime", showWarnings = FALSE)

#  Ultima formazione con rendimento successivo osservabile: si tronca il
#  campione, non i titoli (stessa regola del motore del modulo 02).
fine_formazione <- max(bt_reg$periodo[!is.na(bt_reg$r_next)])

#  Prima formazione del backtest come periodo (primo giorno del mese):
#  DATA_BACKTEST e' una data di fine mese, come data_formazione.
INIZIO_BT <- floor_date(as.Date(DATA_BACKTEST), "month")
cat("Ultima formazione:", format(fine_formazione), "\n")


# #############################################################################
# ##  PARTE R1 — SERIE DI MERCATO PER IL REGIME                              ##
# #############################################################################
#  Convenzione dei fattori: mese_reso = primo giorno del mese del rendimento,
#  la stessa di `periodo` nel pannello. A fine mese t si usano le righe con
#  periodo <= t.
mercato <- fattori |>
  filter(mese_reso >= as.Date(INIZIO_STIMA_HMM), !is.na(mkt_rf), !is.na(rf)) |>
  transmute(periodo = mese_reso, r_mkt = mkt_rf + rf, ecc_mkt = mkt_rf) |>
  arrange(periodo)

#  La serie deve essere contigua: media mobile e trend sono calcolati per
#  posizione e un mese mancante li sposterebbe in silenzio.
stopifnot(all(mercato$periodo[-1] == mercato$periodo[-nrow(mercato)] %m+% months(1)))
if (max(mercato$periodo) < fine_formazione)
  stop("I fattori French terminano a ", format(max(mercato$periodo)),
       ", prima dell'ultima formazione: aggiorna fattori_french.csv.")

cat("Mercato:", nrow(mercato), "mesi |", format(min(mercato$periodo)), "-",
    format(max(mercato$periodo)), "\n")


# #############################################################################
# ##  PARTE R2 — SETTORI TRBC DEI TITOLI                                    ##
# #############################################################################
#  Classificazione: settore economico TRBC (Refinitiv Business
#  Classification), per TUTTI i titoli.
#
#  Perche' non GICS. Il file scaricato contiene entrambe le classificazioni,
#  ma LSEG non restituisce il GICS per 290 dei 369 RIC scaduti, ne' come
#  valore corrente ne' a una data storica: usarlo avrebbe escluso dai
#  panieri soprattutto i titoli usciti dall'indice (fino a 187 membri e al
#  19,5% della capitalizzazione in un mese), cioe' una distorsione legata
#  alla sopravvivenza. Il TRBC copre il 100% dei RIC, scaduti compresi.
#  Una fonte unica per tutti i titoli e' preferita a una miscela GICS + TRBC,
#  che classificherebbe in modo sistematicamente diverso sopravvissuti e
#  usciti. Decisione del 2026-11-01, presa sul controllo di copertura e non
#  sui risultati.
#
#  Limite dichiarato: il settore e' l'ultimo noto, applicato all'intero
#  periodo. Il settore non entra ne' nei rendimenti ne' nella membership:
#  decide solo in quale paniere cade un titolo.
#
#  Input : ponte (RIC -> sec_id)   Output: settori_lseg.csv -> `settori_sec`
if (FORZA_RIGENERAZIONE || !file.exists(FILE_SETTORI)) {
  if (!exists("rd")) rd <- connetti_lseg()
  campi_settore <- c("TR.GICSSector", "TR.TRBCEconomicSector")
  #  Estrazione a lotti del modulo 01; le colonne si riconoscono per contenuto.
  f_settori <- function(rics)
    rinomina_per_contenuto(Refinitiv::rd_GetData(rics = rics, Eikonformulas = campi_settore),
                           list(RIC = "^instrument$|^ric$", settore_gics = "gics",
                                settore_trbc = "trbc|economic")) |>
      come_testo(c("RIC", "settore_gics", "settore_trbc"))
  settori_ric <- estrai_a_lotti(sort(unique(ponte$RIC)), f_settori, lotto = 50) |>
    filter(!is.na(RIC)) |> distinct(RIC, .keep_all = TRUE) |>
    mutate(data_estrazione = Sys.Date())
  write.csv(settori_ric, FILE_SETTORI, row.names = FALSE, na = "")
  cat("Settori scaricati e congelati in", FILE_SETTORI, "\n")
}

settori_ric <- leggi(FILE_SETTORI) |> come_testo(c("RIC", "settore_gics", "settore_trbc"))

#  "Academic & Educational Services" e' un settore TRBC con 3 soli RIC: un
#  paniere cosi' piccolo resterebbe vuoto in molti mesi. Confluisce in
#  Consumer Cyclicals, dove GICS colloca le stesse societa' (Consumer
#  Discretionary).
ACCORPAMENTI <- c("Academic & Educational Services" = "Consumer Cyclicals")

#  Da RIC a security. Una security con piu' RIC prende il settore del RIC
#  non scaduto, se c'e', altrimenti del primo disponibile; i casi in cui i
#  RIC della stessa security riportano settori diversi vengono elencati.
settori_lunghi <- ponte |> select(RIC, sec_id) |>
  inner_join(settori_ric |> select(RIC, settore_trbc), by = "RIC") |>
  filter(!is.na(settore_trbc)) |>
  mutate(settore_trbc = ifelse(settore_trbc %in% names(ACCORPAMENTI),
                               unname(ACCORPAMENTI[settore_trbc]), settore_trbc),
         scaduto = grepl("\\^", RIC))

conflitti <- settori_lunghi |> group_by(sec_id) |>
  filter(n_distinct(settore_trbc) > 1) |> ungroup()
if (nrow(conflitti) > 0) {
  cat("\n!! Security con settori diversi fra i propri RIC:", n_distinct(conflitti$sec_id), "\n")
  print(as.data.frame(conflitti |> arrange(sec_id, scaduto)), row.names = FALSE)
}

settori_sec <- settori_lunghi |> arrange(sec_id, scaduto, RIC) |>
  group_by(sec_id) |> summarise(settore = first(settore_trbc), .groups = "drop")

cat("\nSettori distinti:", n_distinct(settori_sec$settore), "\n")
print(as.data.frame(settori_sec |> count(settore, name = "security")), row.names = FALSE)


# #############################################################################
# ##  PARTE R3 — RENDIMENTI SETTORIALI POINT-IN-TIME                        ##
# #############################################################################
#  Paniere del settore s a t = membri dell'indice a t con settore s, pesati
#  per capitalizzazione di fine t; rendimento in t+1 da bt_reg, quindi con i
#  rendimenti terminali di delisting gia' applicati (modulo 02, regola
#  "effettiva": documentale dove disponibile, altrimenti rendimento nullo,
#  anche per le uscite della finestra di stima 2003-2004).
membri <- bt_reg |>
  filter(eleggibile, periodo >= as.Date(INIZIO_SETTORI), periodo <= fine_formazione) |>
  select(sec_id, periodo, r, r_next, mktcap) |>
  left_join(settori_sec, by = "sec_id")

#  Copertura: quanta parte dell'indice resta fuori dai panieri settoriali.
copertura <- membri |> group_by(periodo) |>
  summarise(n = n(),
            senza_settore = sum(is.na(settore)),
            senza_cap     = sum(is.na(mktcap) | mktcap <= 0),
            quota_cap_coperta = sum(mktcap[!is.na(settore)], na.rm = TRUE) /
              sum(mktcap, na.rm = TRUE), .groups = "drop")
cat("\n========== COPERTURA DEI SETTORI ==========\n")
cat("Membri senza settore, per mese: mediana", median(copertura$senza_settore),
    "| max", max(copertura$senza_settore), "\n")
cat("Membri senza capitalizzazione : max", max(copertura$senza_cap), "\n")
cat("Quota di capitalizzazione coperta: min",
    sprintf("%.2f%%", 100 * min(copertura$quota_cap_coperta)), "\n")

membri_ok <- membri |> filter(!is.na(settore), !is.na(mktcap), mktcap > 0)

#  Presidio: dopo la regola sui delisting e la troncatura del campione nessun
#  membro deve essere privo di rendimento in t+1. Se accade e' un caso nuovo.
senza_r <- membri_ok |> filter(is.na(r_next))
if (nrow(senza_r) > 0) {
  print(as.data.frame(senza_r |> count(periodo)), row.names = FALSE)
  stop(nrow(senza_r), " membri senza rendimento in t+1: caso non previsto.")
}

r_settori <- membri_ok |> group_by(periodo, settore) |>
  summarise(r_s = sum(mktcap * r_next) / sum(mktcap), n_titoli = n(),
            cap = sum(mktcap), .groups = "drop")

SETTORI <- sort(unique(r_settori$settore))
R_mat <- r_settori |> select(periodo, settore, r_s) |>
  pivot_wider(names_from = settore, values_from = r_s) |> arrange(periodo)
periodi_set <- R_mat$periodo
R_mat <- as.matrix(R_mat[, SETTORI])
if (anyNA(R_mat)) stop("Settori senza membri in alcuni mesi: rivedere la classificazione.")

cat("Panieri settoriali:", length(SETTORI), "settori |", nrow(R_mat), "mesi |",
    "titoli per settore e mese: min", min(r_settori$n_titoli), "\n")


# #############################################################################
# ##  PARTE R4 — HIDDEN MARKOV MODEL, STIMA ESPANDENTE                      ##
# #############################################################################
#  HMM gaussiano a K stati sull'excess return mensile di mercato:
#     x_t | S_t = k  ~  N(mu_k, sig_k^2),   P(S_t+1 = j | S_t = i) = A[i, j]
#  Stima di massima verosimiglianza con l'algoritmo EM (Baum-Welch) e
#  filtro di Hamilton scalato. Riferimenti: Hamilton (1989), Ang & Bekaert
#  (2002), Kritzman, Page & Turkington (2012).
#
#  Gli stati sono ordinati per volatilita' crescente: lo stato 1 e' il regime
#  calmo, l'ultimo quello turbolento. L'ordinamento risolve il problema
#  dell'etichettatura fra una stima e la successiva.

#  Passo avanti-indietro: probabilita' filtrate (alpha), smussate (gamma),
#  somma delle transizioni attese (xi) e log-verosimiglianza. Le smussate
#  servono SOLO all'EM sul campione di stima, mai alla decisione.
hmm_fb <- function(x, par) {
  n <- length(x); K <- length(par$mu)
  dens <- vapply(seq_len(K), function(k) dnorm(x, par$mu[k], par$sig[k]), numeric(n))
  alpha <- matrix(0, n, K); cc <- numeric(n)
  a <- par$delta * dens[1, ]; cc[1] <- sum(a); alpha[1, ] <- a / cc[1]
  for (t in 2:n) {
    a <- as.numeric(alpha[t - 1, ] %*% par$A) * dens[t, ]
    cc[t] <- sum(a); alpha[t, ] <- a / cc[t]
  }
  beta <- matrix(1, n, K)
  for (t in (n - 1):1)
    beta[t, ] <- as.numeric(par$A %*% (dens[t + 1, ] * beta[t + 1, ])) / cc[t + 1]
  gamma <- alpha * beta; gamma <- gamma / rowSums(gamma)
  B  <- dens[-1, , drop = FALSE] * beta[-1, , drop = FALSE] / cc[-1]
  xi <- par$A * crossprod(alpha[-n, , drop = FALSE], B)
  list(alpha = alpha, gamma = gamma, xi = xi, loglik = sum(log(cc)))
}

hmm_em <- function(x, par, max_iter = 2000, tol = 1e-7) {
  n <- length(x); ll_prec <- -Inf
  for (it in seq_len(max_iter)) {
    fb <- hmm_fb(x, par)
    g  <- fb$gamma; ng <- colSums(g)
    mu <- colSums(g * x) / ng
    s2 <- colSums(g * (x - rep(mu, each = n))^2) / ng
    par <- list(delta = g[1, ], A = fb$xi / rowSums(fb$xi),
                mu = mu, sig = pmax(sqrt(s2), SIG_MIN))
    if (abs(fb$loglik - ll_prec) < tol) break
    ll_prec <- fb$loglik
  }
  fb <- hmm_fb(x, par)
  c(par, list(loglik = fb$loglik, iter = it, filtrata = fb$alpha[n, ]))
}

ordina_stati <- function(par) {
  o <- order(par$sig)
  par$mu <- par$mu[o]; par$sig <- par$sig[o]; par$delta <- par$delta[o]
  par$A <- par$A[o, o, drop = FALSE]; par$filtrata <- par$filtrata[o]
  par
}

par_iniziali <- function(x, K) {
  A <- matrix(0.05 / (K - 1), K, K); diag(A) <- 0.95
  list(delta = rep(1 / K, K), A = A,
       mu  = mean(x) + seq(0.005, -0.010, length.out = K),
       sig = sd(x) * seq(0.6, 1.8, length.out = K))
}

#  A ogni data di formazione t: stima sulla storia fino a t, partendo dalle
#  stime di t-1 (rapido e stabile). Ogni RIFIT_COMPLETO mesi si riparte
#  anche da valori standard e si tiene la verosimiglianza migliore, per non
#  restare intrappolati in un massimo locale.
stima_regimi_hmm <- function(periodi, K) {
  out <- vector("list", length(periodi)); par_prec <- NULL
  for (i in seq_along(periodi)) {
    t <- periodi[i]
    x <- mercato$ecc_mkt[mercato$periodo <= t]
    cand <- list()
    if (!is.null(par_prec)) cand$caldo <- hmm_em(x, par_prec)
    if (is.null(par_prec) || (i - 1) %% RIFIT_COMPLETO == 0)
      cand$freddo <- hmm_em(x, par_iniziali(x, K))
    m <- ordina_stati(cand[[which.max(vapply(cand, function(z) z$loglik, numeric(1)))]])
    out[[i]] <- data.frame(periodo = t, n_oss = length(x), loglik = m$loglik,
                           iter = m$iter, stato = seq_len(K),
                           p_filtrata = m$filtrata,
                           p_prevista = as.numeric(m$filtrata %*% m$A),
                           mu = m$mu, sig = m$sig, persistenza = diag(m$A))
    par_prec <- m[c("delta", "A", "mu", "sig")]
    if (i %% 24 == 0) cat("  HMM K =", K, ":", format(t), "\n")
  }
  bind_rows(out)
}

periodi_regime <- periodi_set   # una stima per ogni data di formazione

#  Cache: si riusa solo se parametri e dati di mercato sono identici.
firma_hmm <- list(inizio = INIZIO_STIMA_HMM, rifit = RIFIT_COMPLETO, sig_min = SIG_MIN,
                  periodi = periodi_regime, mercato = mercato$ecc_mkt)
cache <- if (file.exists(FILE_HMM)) readRDS(FILE_HMM) else NULL
if (!FORZA_RIGENERAZIONE && !is.null(cache) && identical(cache$firma, firma_hmm)) {
  hmm <- cache$hmm
  cat("Stime HMM lette dalla cache.\n")
} else {
  cat("Stima HMM espandente su", length(periodi_regime), "date (qualche minuto)...\n")
  t0 <- Sys.time()
  hmm <- list(K2 = stima_regimi_hmm(periodi_regime, 2),
              K3 = stima_regimi_hmm(periodi_regime, 3))
  cat("Tempo di stima:", format(round(Sys.time() - t0, 1)), "\n")
  saveRDS(list(firma = firma_hmm, hmm = hmm), FILE_HMM)
}

#  Diagnostica: le stime devono essere stabili nel tempo.
ultima <- hmm$K2 |> filter(periodo == max(periodo))
cat("\n========== HMM A 2 STATI, ULTIMA STIMA (", format(max(hmm$K2$periodo)), ") ==========\n", sep = "")
print(as.data.frame(ultima |> transmute(stato = c("calmo", "turbolento"),
                                        media_annua = 12 * mu, vol_annua = sqrt(12) * sig,
                                        persistenza, durata_attesa_mesi = 1 / (1 - persistenza)) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
salti <- hmm$K2 |> filter(stato == 2) |> arrange(periodo) |>
  mutate(d_sig = abs(sig - lag(sig))) |> filter(!is.na(d_sig))
cat("Massima variazione mensile della vol. turbolenta stimata:",
    sprintf("%.2f pp annui", 100 * sqrt(12) * max(salti$d_sig)), "\n")


# #############################################################################
# ##  PARTE R5 — REGIMI DA REGOLE                                           ##
# #############################################################################
#  Due indicatori deterministici della letteratura, usati come termine di
#  paragone dell'HMM. Producono uno stato certo: probabilita' 1 o 0.
#    SMA:   rialzo se il livello di fine t >= media dei livelli t-9..t
#    TREND: ribasso se il rendimento cumulato di mercato t-23..t e' negativo
mercato <- mercato |>
  mutate(livello = cumprod(1 + r_mkt),
         sma     = as.numeric(stats::filter(livello, rep(1 / MESI_SMA, MESI_SMA), sides = 1)),
         trend   = livello / dplyr::lag(livello, MESI_TREND) - 1)

regola <- function(nome, rialzo) {
  d <- mercato |> filter(periodo %in% periodi_regime)
  r <- rialzo(d)
  if (anyNA(r)) stop("Regola ", nome, " non calcolabile in alcune date.")
  bind_rows(data.frame(periodo = d$periodo, stato = 1L, p_prevista = as.numeric(r)),
            data.frame(periodo = d$periodo, stato = 2L, p_prevista = as.numeric(!r)))
}

#  Formato comune a tutti i metodi: periodo, stato (1 = rialzo/calmo,
#  ultimo = ribasso/turbolento), probabilita' prevista per t+1.
REGIMI <- list(
  HMM   = hmm$K2 |> select(periodo, stato, p_prevista),
  SMA   = regola("SMA",   function(d) d$livello >= d$sma),
  TREND = regola("TREND", function(d) d$trend >= 0),
  HMM3  = hmm$K3 |> select(periodo, stato, p_prevista))

#  Riepilogo: quota di mesi nel regime avverso e numero di cambi di regime.
riepilogo_regimi <- bind_rows(lapply(names(REGIMI), function(nm) {
  p_avv <- REGIMI[[nm]] |> group_by(periodo) |> filter(stato == max(stato)) |>
    ungroup() |> arrange(periodo) |> filter(periodo >= INIZIO_BT)
  avv <- p_avv$p_prevista >= 0.5
  data.frame(metodo = nm, mesi = length(avv), quota_avversa = mean(avv),
             cambi = sum(avv[-1] != avv[-length(avv)]))
}))
cat("\n========== REGIMI NEL CAMPIONE DI BACKTEST ==========\n")
print(as.data.frame(riepilogo_regimi |> mutate(across(where(is.numeric), \(x) round(x, 3)))),
      row.names = FALSE)


# #############################################################################
# ##  PARTE R6 — PREVISIONE CONDIZIONATA AL REGIME E ROTAZIONE              ##
# #############################################################################
#  A ogni t, per il settore s e lo stato k:
#
#           sum_tau  pi_tau,k * r_s,tau+1  +  PRIOR * m_s
#  mu_s,k = ----------------------------------------------      tau+1 <= t
#                 sum_tau  pi_tau,k        +  PRIOR
#
#  dove pi_tau e' la probabilita' di stato per tau+1 prevista in tempo reale
#  a tau e m_s la media incondizionata del settore sullo stesso campione. Lo
#  shrinkage evita che uno stato raramente osservato (all'inizio del campione
#  il regime turbolento ha pochi mesi) produca medie estreme.
#
#  Rendimento atteso per t+1:   E_t[r_s] = sum_k pi_t,k * mu_s,k
#  E' qui che la probabilita' entra come miscela: con pi_t = (0,6; 0,4) la
#  previsione e' per il 60% quella del regime calmo e per il 40% quella del
#  turbolento.
#
#  Portafoglio: gli N_TOP settori con il rendimento atteso piu' alto,
#  equipesati fra loro, pesati per capitalizzazione al loro interno.
prob_matrice <- function(reg) {
  w <- reg |> pivot_wider(names_from = stato, values_from = p_prevista) |>
    arrange(periodo)
  P <- as.matrix(w[, -1]); rownames(P) <- format(w$periodo); P
}

previsioni_settori <- function(reg, prior = PRIOR_MESI) {
  P <- if (is.null(reg)) matrix(1, length(periodi_set), 1,
                                dimnames = list(format(periodi_set), "1"))
       else prob_matrice(reg)
  stopifnot(identical(rownames(P), format(periodi_set)))
  periodi_bt <- periodi_set[periodi_set >= INIZIO_BT]
  out <- vector("list", length(periodi_bt))
  for (j in seq_along(periodi_bt)) {
    i <- match(periodi_bt[j], periodi_set)
    m <- i - 1                              # formazioni tau < t: r in tau+1 <= t
    Rp <- R_mat[seq_len(m), , drop = FALSE]
    Pp <- P[seq_len(m), , drop = FALSE]
    m_s <- colMeans(Rp)
    mu  <- (crossprod(Pp, Rp) + prior * matrix(m_s, ncol(P), ncol(R_mat), byrow = TRUE)) /
      (colSums(Pp) + prior)
    out[[j]] <- setNames(as.numeric(P[i, , drop = FALSE] %*% mu), SETTORI)
  }
  names(out) <- format(periodi_bt)
  out
}

pesi_top <- function(prev, n_top = N_TOP) lapply(prev, function(e) {
  sel <- names(sort(e, decreasing = TRUE))[seq_len(n_top)]
  setNames(rep(1 / n_top, n_top), sel)
})

#  Motore a livello di titolo: dai pesi dei settori ai pesi dei titoli, con
#  turnover sui pesi derivati e costo per lato come nel motore del modulo 02.
per_periodo_bt <- split(bt_reg |> select(sec_id, periodo, r), bt_reg$periodo)
per_periodo_mb <- split(membri_ok, membri_ok$periodo)

esegui_rotazione <- function(pesi_settori, costo_unitario = COSTO_UNITARIO) {
  w_prec <- setNames(numeric(0), character(0))
  righe  <- vector("list", length(pesi_settori))
  for (i in seq_along(pesi_settori)) {
    tt  <- names(pesi_settori)[i]
    W   <- pesi_settori[[i]]
    tut <- per_periodo_bt[[tt]]
    cand <- per_periodo_mb[[tt]]
    cand <- cand[cand$settore %in% names(W)[W > 0], ]
    if (length(setdiff(names(W)[W > 0], cand$settore)) > 0)
      stop("Settore senza membri a ", tt)

    w <- W[cand$settore] * cand$mktcap / ave(cand$mktcap, cand$settore, FUN = sum)
    names(w) <- cand$sec_id
    stopifnot(abs(sum(w) - 1) < 1e-9, !anyNA(cand$r_next))

    if (length(w_prec) > 0) {
      rr <- setNames(tut$r, tut$sec_id)[names(w_prec)]; rr[is.na(rr)] <- 0
      w_drift <- w_prec * (1 + rr); w_drift <- w_drift / sum(w_drift)
    } else w_drift <- setNames(numeric(0), character(0))
    tutti <- union(names(w), names(w_drift))
    a <- setNames(rep(0, length(tutti)), tutti); a[names(w)]       <- w
    b <- setNames(rep(0, length(tutti)), tutti); b[names(w_drift)] <- w_drift
    scambiato <- sum(abs(a - b))

    r_l <- sum(w * cand$r_next)
    righe[[i]] <- data.frame(
      formazione = as.Date(tt), mese_reso = as.Date(tt) %m+% months(1),
      n_titoli = length(w), settori = paste(names(W)[W > 0], collapse = "; "),
      r_lordo = r_l, costo = scambiato * costo_unitario,
      r_netto = r_l - scambiato * costo_unitario, turnover = scambiato / 2,
      stringsAsFactors = FALSE)
    w_prec <- w
  }
  bind_rows(righe)
}

#  Strategie confrontate. Ogni coppia differisce per UN elemento:
#    Settori EW      tutti gli 11 settori equipesati: nessuna selezione
#    Incondizionata  top N per media storica: selezione senza regime
#    HMM / SMA / TREND  top N per media condizionata al regime
#  Il contributo del regime e' quindi  (HMM - Incondizionata).
prev <- list(INCOND = previsioni_settori(NULL),
             HMM    = previsioni_settori(REGIMI$HMM),
             SMA    = previsioni_settori(REGIMI$SMA),
             TREND  = previsioni_settori(REGIMI$TREND))

pesi_ew_settori <- lapply(prev$INCOND, function(e)
  setNames(rep(1 / length(SETTORI), length(SETTORI)), SETTORI))

STRATEGIE <- c(EW_SETT = "Settori EW", INCOND = "Incondizionata",
               HMM = "Regime HMM", SMA = "Regime SMA", TREND = "Regime TREND")

pesi_strategie <- c(list(EW_SETT = pesi_ew_settori), lapply(prev, pesi_top))
cat("\nEsecuzione delle rotazioni...\n")
serie_rot <- lapply(pesi_strategie, esegui_rotazione)

#  Stesso campione per tutte le strategie (protocollo I13).
mesi_strat <- vapply(serie_rot, nrow, integer(1))
stopifnot(length(unique(mesi_strat)) == 1,
          all(vapply(serie_rot, function(s) identical(s$mese_reso, serie_rot[[1]]$mese_reso),
                     logical(1))))
cat("Mesi di backtest:", mesi_strat[1], "| titoli medi in portafoglio (HMM):",
    round(mean(serie_rot$HMM$n_titoli)), "\n")


# #############################################################################
# ##  PARTE R7 — VALUTAZIONE                                                ##
# #############################################################################
serie_reg <- serie_rot$EW_SETT |> select(formazione, mese_reso)
for (k in names(serie_rot))
  serie_reg <- serie_reg |>
    left_join(serie_rot[[k]] |> select(mese_reso, !!paste0(k, "_lordo") := r_lordo,
                                       !!paste0(k, "_netto") := r_netto,
                                       !!paste0(k, "_tn") := turnover), by = "mese_reso")
serie_reg <- serie_reg |> left_join(benchmark, by = "mese_reso") |>
  left_join(fattori, by = "mese_reso")
stopifnot(!anyNA(serie_reg$r_bench), !anyNA(serie_reg$rf), !anyNA(serie_reg$mom))

# ---- Metriche --------------------------------------------------------------
tabella_reg <- bind_rows(
  lapply(names(STRATEGIE), function(k) bind_rows(
    cbind(strategia = STRATEGIE[[k]], versione = "lordo",
          metriche(serie_reg[[paste0(k, "_lordo")]], serie_reg$r_bench,
                   serie_reg[[paste0(k, "_tn")]], serie_reg$rf)),
    cbind(strategia = STRATEGIE[[k]], versione = "netto",
          metriche(serie_reg[[paste0(k, "_netto")]], serie_reg$r_bench,
                   serie_reg[[paste0(k, "_tn")]], serie_reg$rf)))),
  cbind(strategia = "Benchmark", versione = "",
        metriche(serie_reg$r_bench, NULL, NULL, serie_reg$rf)))

cat("\n========== METRICHE DELLE ROTAZIONI ==========\n")
print(as.data.frame(tabella_reg |> select(strategia, versione, rend_annuo, volatilita, sharpe,
                                          max_dd, turnover_medio, active_geom, info_ratio) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)

# ---- Significativita' (Newey-West) -----------------------------------------
#  Rispetto al benchmark: active return medio e alpha di Carhart, al netto.
FATT_C <- c("mkt_rf", "smb", "hml", "mom")
test_reg <- bind_rows(lapply(names(STRATEGIE), function(k) {
  y  <- serie_reg[[paste0(k, "_netto")]]
  t1 <- regressione_nw(y - serie_reg$r_bench)
  t2 <- regressione_nw(y - serie_reg$rf, X = serie_reg[, FATT_C], nomi = c("mkt", "smb", "hml", "mom"))
  bind_rows(cbind(strategia = STRATEGIE[[k]], test = "Active return medio", t1[t1$termine == "alpha", ]),
            cbind(strategia = STRATEGIE[[k]], test = "Alpha Carhart", t2[t2$termine == "alpha", ]),
            cbind(strategia = STRATEGIE[[k]], test = "Beta di mercato", t2[t2$termine == "mkt", ]))
})) |> mutate(fattore = ifelse(grepl("Beta", test), 1, 12),
              stima_a = stima * fattore, se_a = se_nw * fattore, sig = stelle(p))

#  Confronti a coppie: differenza media mensile, annualizzata.
COPPIE <- list(c("HMM", "INCOND"), c("SMA", "INCOND"), c("TREND", "INCOND"),
               c("HMM", "SMA"), c("HMM", "TREND"), c("INCOND", "EW_SETT"))
confronti_reg <- bind_rows(lapply(COPPIE, function(cp) bind_rows(lapply(c("lordo", "netto"), function(v) {
  d <- regressione_nw(serie_reg[[paste0(cp[1], "_", v)]] - serie_reg[[paste0(cp[2], "_", v)]])
  cbind(confronto = paste(STRATEGIE[[cp[1]]], "-", STRATEGIE[[cp[2]]]), versione = v,
        d[d$termine == "alpha", ])
})))) |> mutate(annua = stima * 12, se_a = se_nw * 12, sig = stelle(p))

cat("\n========== SIGNIFICATIVITA' (Newey-West, netto) ==========\n")
print(as.data.frame(test_reg |> select(strategia, test, stima_a, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
cat("\nConfronti a coppie:\n")
print(as.data.frame(confronti_reg |> select(confronto, versione, annua, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)

# ---- Sottoperiodi ----------------------------------------------------------
SOTTOPERIODI <- list("2005-2007 Pre-crisis"       = c("2005-01-01", "2007-12-31"),
                     "2008-2009 Financial crisis" = c("2008-01-01", "2009-12-31"),
                     "2010-2019 Expansion"        = c("2010-01-01", "2019-12-31"),
                     "2020-2026 Post-COVID"       = c("2020-01-01", "2026-12-31"))
sottoperiodi_reg <- bind_rows(lapply(names(SOTTOPERIODI), function(sp) {
  d <- serie_reg |> filter(mese_reso >= as.Date(SOTTOPERIODI[[sp]][1]),
                           mese_reso <= as.Date(SOTTOPERIODI[[sp]][2]))
  bind_rows(lapply(names(STRATEGIE), function(k)
    cbind(periodo = sp, strategia = STRATEGIE[[k]],
          metriche(d[[paste0(k, "_netto")]], d$r_bench, NULL, d$rf))),
    cbind(periodo = sp, strategia = "Benchmark", metriche(d$r_bench, NULL, NULL, d$rf)))
}))

#  Rendimenti medi di mercato nei regimi previsti: il regime avverso deve
#  davvero precedere mesi peggiori o piu' volatili, altrimenti il segnale non
#  separa nulla. E' una verifica descrittiva, non entra nelle decisioni.
qualita_regimi <- bind_rows(lapply(c("HMM", "SMA", "TREND"), function(nm) {
  REGIMI[[nm]] |> filter(stato == max(stato), periodo >= INIZIO_BT,
                         periodo <= fine_formazione) |>
    transmute(mese_reso = periodo %m+% months(1), avverso = p_prevista >= 0.5) |>
    inner_join(mercato |> select(mese_reso = periodo, r_mkt), by = "mese_reso") |>
    group_by(metodo = nm, regime = ifelse(avverso, "avverso", "favorevole")) |>
    summarise(mesi = n(), media_annua = 12 * mean(r_mkt), vol_annua = sqrt(12) * sd(r_mkt),
              .groups = "drop")
}))
cat("\n========== MERCATO NEL MESE SUCCESSIVO, PER REGIME PREVISTO ==========\n")
print(as.data.frame(qualita_regimi |> mutate(across(where(is.numeric), \(x) round(x, 4)))),
      row.names = FALSE)


# #############################################################################
# ##  PARTE R8 — ROBUSTEZZA                                                 ##
# #############################################################################
#  Ogni variante cambia UN elemento della rotazione HMM di riferimento.
#  Le varianti verificano, non selezionano: la specificazione principale
#  resta quella fissata nella PARTE R0.
varianti <- list(
  "Baseline (K = 2, top 3, prior 12, 10 bp)" = list(gruppo = "Baseline"),
  "Top 2 sectors"         = list(gruppo = "Sectors held", n_top = 2),
  "Top 4 sectors"         = list(gruppo = "Sectors held", n_top = 4),
  "3-state HMM"           = list(gruppo = "Regime model", reg = REGIMI$HMM3),
  "Prior 0 months"        = list(gruppo = "Shrinkage", prior = 1e-8),
  "Prior 36 months"       = list(gruppo = "Shrinkage", prior = 36),
  "25 bp"                 = list(gruppo = "Transaction costs", costo = 0.0025),
  "50 bp"                 = list(gruppo = "Transaction costs", costo = 0.0050))

robustezza_reg <- bind_rows(lapply(names(varianti), function(nm) {
  v  <- varianti[[nm]]
  pv <- previsioni_settori(if (is.null(v$reg)) REGIMI$HMM else v$reg,
                           prior = if (is.null(v$prior)) PRIOR_MESI else v$prior)
  s  <- esegui_rotazione(pesi_top(pv, if (is.null(v$n_top)) N_TOP else v$n_top),
                         if (is.null(v$costo)) COSTO_UNITARIO else v$costo)
  d  <- s |> left_join(serie_reg |> select(mese_reso, r_bench, rf), by = "mese_reso")
  cbind(gruppo = v$gruppo, variante = nm, metriche(d$r_netto, d$r_bench, d$turnover, d$rf))
}))
cat("\n========== ROBUSTEZZA (rotazione HMM, netto) ==========\n")
print(as.data.frame(robustezza_reg |> select(gruppo, variante, rend_annuo, volatilita, sharpe,
                                             active_geom, turnover_medio) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)


# #############################################################################
# ##  PARTE R9 — TABELLE E GRAFICI                                          ##
# #############################################################################
#  Come nel modulo di output del kit: logica in italiano, presentazione in
#  inglese, traduzione solo qui.
TRAD_R <- c("Settori EW" = "Equal-weight sectors", "Incondizionata" = "Unconditional rotation",
            "Regime HMM" = "HMM regime rotation", "Regime SMA" = "SMA regime rotation",
            "Regime TREND" = "Trend regime rotation", "Benchmark" = "Benchmark (S&P 500 TR)",
            "lordo" = "Gross of costs", "netto" = "Net of costs",
            "Active return medio" = "Mean active return", "Alpha Carhart" = "Carhart alpha",
            "Beta di mercato" = "Market beta",
            "avverso" = "Adverse", "favorevole" = "Favorable",
            "HMM" = "HMM (2 states)", "HMM3" = "HMM (3 states)",
            "SMA" = "10-month moving average", "TREND" = "24-month market return")
en_esatto <- function(x) { x <- as.character(x); ifelse(x %in% names(TRAD_R), unname(TRAD_R[x]), x) }
#  "A - B" -> "A minus B", traducendo i due membri separatamente.
en_coppia <- function(x) paste(en_esatto(sub(" - .*$", "", x)), "minus", en_esatto(sub("^.* - ", "", x)))

pct <- function(x, d = 2) ifelse(is.na(x), "", paste0(formatC(100*x, format="f", digits=d), "%"))
num <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format="f", digits=d))

tabR1 <- tabella_reg |> transmute(
  Strategy = en_esatto(strategia), Version = en_esatto(versione), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
  `Average monthly turnover` = pct(turnover_medio),
  `Active return (geometric)` = pct(active_geom), `Tracking error` = pct(tracking_err),
  `Information ratio` = num(info_ratio))

tabR2 <- bind_rows(
  test_reg |> transmute(Group = "Versus benchmark, net of costs",
                        Item = paste0(en_esatto(strategia), ": ", en_esatto(test)),
                        Estimate = ifelse(grepl("Beta", test), num(stima_a, 3), pct(stima_a)),
                        `Std. error (Newey-West)` = ifelse(grepl("Beta", test), num(se_a, 3), pct(se_a)),
                        `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig),
  confronti_reg |> transmute(Group = "Pairwise differences", Item = paste0(en_coppia(confronto), ", ", en_esatto(versione)),
                             Estimate = pct(annua), `Std. error (Newey-West)` = pct(se_a),
                             `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig))

tabR3 <- bind_rows(
  riepilogo_regimi |> left_join(
    qualita_regimi |> select(metodo, regime, media_annua, vol_annua) |>
      pivot_wider(names_from = regime, values_from = c(media_annua, vol_annua)), by = "metodo")) |>
  transmute(`Regime model` = en_esatto(metodo), Months = mesi,
            `Share of adverse months` = pct(quota_avversa, 1), `Regime switches` = cambi,
            `Market return after favorable` = pct(media_annua_favorevole),
            `Market return after adverse` = pct(media_annua_avverso),
            `Market volatility after favorable` = pct(vol_annua_favorevole),
            `Market volatility after adverse` = pct(vol_annua_avverso))

tabR4 <- sottoperiodi_reg |> transmute(
  Subperiod = periodo, Strategy = en_esatto(strategia), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
  `Active return (geometric)` = pct(active_geom))

tabR5 <- robustezza_reg |> transmute(
  Group = gruppo, Variant = variante,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Active return (geometric)` = pct(active_geom),
  `Average monthly turnover` = pct(turnover_medio))

nomi_tabR <- c("table_R1_performance", "table_R2_significance", "table_R3_regimes",
               "table_R4_subperiods", "table_R5_robustness")
for (i in 1:5) write.csv(get(paste0("tabR", i)), paste0("tabelle_regime/", nomi_tabR[i], ".csv"),
                         row.names = FALSE, na = "")
write_xlsx(list(Performance = tabR1, Significance = tabR2, Regimes = tabR3,
                Subperiods = tabR4, Robustness = tabR5), "tabelle_regime/regime_tables.xlsx")

# =============================== GRAFICI =====================================
ETI_R <- c("HMM regime rotation", "SMA regime rotation", "Trend regime rotation",
           "Unconditional rotation", "Equal-weight sectors", "Benchmark (S&P 500 TR)")
COL_R <- c("HMM regime rotation"    = "#1F4E79", "SMA regime rotation"   = "#C55A11",
           "Trend regime rotation"  = "#2E7D32", "Unconditional rotation" = "#7B3F99",
           "Equal-weight sectors"   = "#B8860B", "Benchmark (S&P 500 TR)" = "#7F7F7F")
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        plot.caption = element_text(colour = "grey45", size = 8, hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9, colour = "grey30"))
mese_en <- function(d) paste(month.name[month(d)], year(d))
PERIODO_R <- paste(mese_en(min(serie_reg$mese_reso)), "to", mese_en(max(serie_reg$mese_reso)))
DIDA <- paste0("Top ", N_TOP, " TRBC economic sectors by regime-conditional expected return, ",
               "cap-weighted within sector, monthly rebalancing, 10 bp per side.")

# ---- R1. Probabilita' del regime avverso -----------------------------------
p_avv <- bind_rows(lapply(c("HMM", "SMA", "TREND"), function(nm)
  REGIMI[[nm]] |> filter(stato == max(stato)) |> mutate(metodo = en_esatto(nm)))) |>
  mutate(metodo = factor(metodo, levels = en_esatto(c("HMM", "SMA", "TREND"))))
ggsave("grafici_regime/figure_R1_regime_probability.png",
       ggplot(p_avv, aes(periodo, p_prevista)) +
         geom_area(fill = "#1F4E79", alpha = .25) + geom_line(colour = "#1F4E79", linewidth = .5) +
         facet_wrap(~ metodo, ncol = 1) +
         scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .5, 1)) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Real-time probability of the adverse regime",
              subtitle = "Probability for month t+1 using information up to the end of month t; HMM re-estimated every month on an expanding window",
              x = NULL, y = NULL) + tema,
       width = 9, height = 6.5, dpi = 300)

# ---- R2. Curva di ricchezza ------------------------------------------------
larga <- serie_reg |> transmute(mese_reso,
  `HMM regime rotation`    = cumprod(1 + HMM_netto) * 100,
  `SMA regime rotation`    = cumprod(1 + SMA_netto) * 100,
  `Trend regime rotation`  = cumprod(1 + TREND_netto) * 100,
  `Unconditional rotation` = cumprod(1 + INCOND_netto) * 100,
  `Equal-weight sectors`   = cumprod(1 + EW_SETT_netto) * 100,
  `Benchmark (S&P 500 TR)` = cumprod(1 + r_bench) * 100)
lunga <- larga |> pivot_longer(-mese_reso, names_to = "serie", values_to = "valore") |>
  mutate(serie = factor(serie, levels = ETI_R))
ggsave("grafici_regime/figure_R2_wealth.png",
       ggplot(lunga, aes(mese_reso, valore, colour = serie)) +
         geom_line(linewidth = .7) + scale_colour_manual(values = COL_R) +
         scale_y_log10(labels = label_number(accuracy = 1)) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Growth of 100 invested",
              subtitle = paste0("Net of transaction costs, log scale, ", PERIODO_R),
              x = NULL, y = "Value (base 100)", caption = DIDA) + tema,
       width = 9, height = 5.2, dpi = 300)

# ---- R3. Settori detenuti dalla rotazione HMM ------------------------------
detenuti <- bind_rows(lapply(names(pesi_strategie$HMM), function(tt)
  data.frame(periodo = as.Date(tt), settore = names(pesi_strategie$HMM[[tt]]))))
ggsave("grafici_regime/figure_R3_sectors_held.png",
       ggplot(detenuti, aes(periodo, settore)) +
         geom_tile(fill = "#1F4E79", height = .8) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         scale_y_discrete(limits = rev(SETTORI)) +
         labs(title = "Sectors held by the HMM regime rotation",
              subtitle = "Formation month; each held sector carries 1/3 of the portfolio",
              x = NULL, y = NULL) + tema,
       width = 9, height = 4.8, dpi = 300)

# ---- R4. Rendimenti attesi di stato all'ultima data ------------------------
#  Le medie condizionate stimate con tutta l'informazione disponibile
#  all'ultima formazione: mostrano quali settori il modello considera
#  difensivi e quali ciclici.
m_fin <- nrow(R_mat) - 1
P_fin <- prob_matrice(REGIMI$HMM)[seq_len(m_fin), , drop = FALSE]
R_fin <- R_mat[seq_len(m_fin), , drop = FALSE]
mu_fin <- (crossprod(P_fin, R_fin) + PRIOR_MESI * matrix(colMeans(R_fin), 2, ncol(R_fin), byrow = TRUE)) /
  (colSums(P_fin) + PRIOR_MESI)
mu_fin <- data.frame(settore = rep(SETTORI, each = 2),
                     regime = rep(c("Calm", "Turbulent"), length(SETTORI)),
                     mu = 12 * as.numeric(mu_fin))
ggsave("grafici_regime/figure_R4_conditional_means.png",
       ggplot(mu_fin, aes(mu, settore, fill = regime)) +
         geom_col(position = position_dodge(width = .8), width = .75) +
         geom_vline(xintercept = 0, colour = "grey40", linewidth = .3) +
         scale_fill_manual(values = c(Calm = "#1F4E79", Turbulent = "#C55A11")) +
         scale_x_continuous(labels = percent_format(accuracy = 1)) +
         scale_y_discrete(limits = rev(SETTORI)) +
         labs(title = "Regime-conditional sector returns",
              subtitle = paste0("Annualized, estimated with information up to ", mese_en(fine_formazione),
                                "; weights are real-time HMM probabilities"),
              x = NULL, y = NULL) + tema,
       width = 8, height = 5, dpi = 300)

cat("\n========== TABELLA PRINCIPALE ==========\n")
print(as.data.frame(tabR1 |> filter(Version != "Gross of costs")), row.names = FALSE)
cat("\nStudio completato: tabelle_regime/ e grafici_regime/.\n")
