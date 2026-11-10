# =============================================================================
#
#   ROTAZIONE SETTORIALE GUIDATA DAL CICLO ECONOMICO
#   Rotazione settoriale sui costituenti storici dell'S&P 500
#
#   Domanda di ricerca:
#     Le fasi del ciclo economico, identificate in tempo reale, migliorano
#     l'allocazione fra i settori dell'S&P 500 rispetto a una rotazione che
#     le ignora? E il regime di politica monetaria?
#
#   Secondo studio del progetto. Il primo (RegimeSettori.R) usava un HMM
#   sul rendimento di mercato: identificava regimi di VOLATILITA', non di
#   ciclo, e il suo contributo alla rotazione e' risultato nullo. Qui il
#   regime e' definito in termini economici.
#
# =============================================================================
#
#   COME SI ESEGUE
#   --------------
#   Directory di lavoro = questa cartella ("studi"),
#   aperta con il suo progetto .Rproj. Tutto il file in una volta.
#
#   Lo script esegue RegimeSettori.R fino alla PARTE R3 compresa: universo e
#   dataset (../R/00_universo.R), settori TRBC, rendimenti settoriali point-in-time. Ne
#   riusa quindi universo, settori e panieri senza ricalcolarli diversamente:
#   i due studi cambiano SOLO il regime.
#
#   Al primo giro scarica dalla rete i dati del ciclo (Philadelphia Fed) e le
#   decisioni del FOMC (Federal Reserve Board) e li congela su disco.
#
# -----------------------------------------------------------------------------
#
#   LEGENDA DELLE SEZIONI
#   ---------------------
#   PARTE C0  Universo e settori (da RegimeSettori.R), parametri
#   PARTE C1  Dati del ciclo e della politica monetaria, congelati
#   PARTE C2  Regime A: fasi del ciclo in tempo reale
#   PARTE C3  Regime C: politica monetaria
#   PARTE C4  Previsione condizionata al regime e rotazione
#   PARTE C5  Valutazione
#   PARTE C6  Robustezza
#   PARTE C7  Tabelle e grafici
#
# -----------------------------------------------------------------------------
#
#   PROTOCOLLO TEMPORALE (anti look-ahead)
#   --------------------------------------
#     1. Ciclo: a fine mese t si usa la VERSIONE dei dati pubblicata nel mese
#        t (Real-Time Data Set for Macroeconomists, Philadelphia Fed), cioe'
#        le serie esattamente come erano note allora, con le revisioni
#        successive escluse. L'ultimo dato di quella versione e' di norma il
#        mese t-1: il ritardo di pubblicazione e' rispettato per costruzione.
#        Anche media e deviazione standard della standardizzazione sono
#        calcolate su quella sola versione.
#     2. Politica monetaria: conta l'ultima decisione annunciata entro la fine
#        del mese t. Le decisioni del FOMC sono note il giorno stesso e non
#        vengono mai riviste.
#     3. Rendimenti attesi dei settori per regime: media dei rendimenti
#        realizzati fino a t, pesata con i regimi COME ERANO NOTI a ciascuna
#        data (stessa regola di RegimeSettori.R, PARTE R6).
#     4. Portafoglio formato a fine t, rendimento in t+1: timeline del modulo 02.
#
#   FILE PRODOTTI
#   -------------
#     rtdsm_iptMvMd.xlsx     produzione industriale, tutte le versioni   C1
#     rtdsm_employMvMd.xlsx  occupazione non agricola, tutte le versioni C1
#     fomc_decisioni.csv     decisioni sul tasso obiettivo dal 1990      C1
#     fonti_ciclo.csv        provenienza e data di download dei file     C1
#     tabelle_ciclo/         tabelle in inglese, csv e xlsx              C7
#     grafici_ciclo/         grafici a 300 dpi                           C7
#
# =============================================================================




# #############################################################################
# ##  PARTE C0 — UNIVERSO E SETTORI, PARAMETRI                              ##
# #############################################################################
#  Si valuta RegimeSettori.R fino alla riga che precede la PARTE R4 (l'HMM):
#  restano definiti bt_reg, membri_ok, R_mat (rendimenti settoriali),
#  SETTORI, periodi_set, benchmark, fattori, mercato e le funzioni dei moduli
#  (metriche, regressione_nw, stelle).
if (!file.exists("RegimeSettori.R"))
  stop("RegimeSettori.R non trovato: imposta la directory di lavoro sulla ",
       "cartella \"studi\".")
.righe_rs <- readLines("RegimeSettori.R", encoding = "UTF-8")
.fine_rs  <- grep("^# ##  PARTE R4", .righe_rs)[1] - 2
if (is.na(.fine_rs)) stop("Intestazione della PARTE R4 non trovata in RegimeSettori.R.")
eval(parse(text = .righe_rs[seq_len(.fine_rs)], encoding = "UTF-8"), envir = globalenv())
rm(.righe_rs, .fine_rs)
cat("\n========== UNIVERSO E SETTORI CARICATI ==========\n\n")

# -----------------------------------------------------------------------------
#  PARAMETRI — fissati prima di vedere i risultati
# -----------------------------------------------------------------------------
#  Specifica concordata il 2026-11-10, prima di scaricare i dati del ciclo e
#  quindi prima di qualunque risultato:
#    regime A (principale): fasi del ciclo da produzione industriale e
#      occupazione non agricola, livello x direzione;
#    regime C (confronto):  ultima mossa del FOMC sul tasso obiettivo,
#      come in Conover, Jensen, Johnson & Mercer (2008);
#    rotazione: identica a RegimeSettori.R (N_TOP, PRIOR_MESI, costi).
#  Ogni modifica successiva va registrata con la motivazione.
URL_RTDSM  <- "https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/real-time-data/data-files/xlsx/"
URL_FOMC   <- c("https://www.federalreserve.gov/monetarypolicy/openmarket.htm",
                "https://www.federalreserve.gov/monetarypolicy/openmarket_archive.htm")
FILE_IPT    <- "rtdsm_iptMvMd.xlsx"
FILE_EMPLOY <- "rtdsm_employMvMd.xlsx"
FILE_FOMC   <- "fomc_decisioni.csv"
FILE_FONTI  <- "fonti_ciclo.csv"

#  Indicatore del ciclo
MESI_CRESCITA       <- 12             # crescita a 12 mesi: niente stagionalita' residua
MESI_DIREZIONE      <- 3              # direzione = variazione dell'indicatore in 3 mesi
INIZIO_STORIA_CICLO <- "1948-01-01"   # scala dal dopoguerra: la smobilitazione
                                      # 1945-47 gonfierebbe la varianza
#  Correzioni del 2026-11-10, decise sulla distribuzione delle fasi e PRIMA
#  di qualunque rendimento della rotazione. Con la media dal 1948 il 47% dei
#  mesi risultava in contrazione (la crescita di lungo periodo e' calata
#  dopo il 2000) e le fasi cambiavano ogni 3 mesi circa (rumore mensile).
#    - livello rispetto alla media mobile di 10 anni della crescita, non a
#      quella dal 1948: segue il trend senza filtri a due code, che in tempo
#      reale sono inaffidabili (Orphanides & van Norden 2002; Hamilton 2018);
#    - indicatore smussato su 3 mesi prima di livello e direzione, come il
#      CFNAI-MA3 della Chicago Fed.
MESI_MEDIA_MOBILE   <- 120
MESI_LISCIATURA     <- 3

#  Criteri di accettazione del regime, fissati prima di verificarli e
#  valutati sulle sole fasi, mai sui rendimenti:
#    ogni fase copre almeno il 10% dei mesi del backtest;
#    durata media di ogni fase di almeno 5 mesi (Bry & Boschan 1971);
#    le recessioni NBER cadono in larga parte in contrazione.
QUOTA_MIN_FASE      <- 0.10
DURATA_MIN_FASE     <- 5

#  Secondo passo, previsto se i criteri non fossero soddisfatti (e non lo
#  erano: ripresa all'8% dei mesi, espansione di 4,7 mesi in media): una
#  fase nuova si adotta solo dopo essere stata segnalata per MESI_CONFERMA
#  mesi consecutivi, in parallelo con le regole di censura di Bry & Boschan.
#  Usa solo le fasi gia' note a ciascuna data: nessun look-ahead.
MESI_CONFERMA       <- 5

#  Fasi. Terzo passo, previsto se i criteri non fossero ancora soddisfatti:
#  con la conferma di 5 mesi la ripresa copriva l'8,2% dei mesi (soglia
#  10%). La specificazione principale usa quindi 2 fasi, il solo livello
#  dell'indicatore rispetto al trend; le 4 fasi (livello x direzione, la
#  logica dell'OCSE per il suo indicatore) restano come variante di
#  robustezza (PARTE C6). Decisione del 2026-11-10, prima di qualunque
#  rendimento della rotazione.
FASI_2 <- c("Sopra il trend", "Sotto il trend")
FASI_4 <- c("Ripresa", "Espansione", "Rallentamento", "Contrazione")
N_FASI <- 2
FASI   <- if (N_FASI == 2) FASI_2 else FASI_4
STANCE <- c("Espansiva", "Restrittiva")

#  Recessioni NBER (mesi successivi al picco fino alla fine), SOLO per la
#  diagnostica ex post: l'NBER le annuncia con mesi di ritardo e non entrano
#  in alcuna decisione.
RECESSIONI_NBER <- list(c("2008-01-01", "2009-06-01"), c("2020-03-01", "2020-04-01"))

dir.create("tabelle_ciclo", showWarnings = FALSE)
dir.create("grafici_ciclo", showWarnings = FALSE)


# #############################################################################
# ##  PARTE C1 — DATI DEL CICLO E DELLA POLITICA MONETARIA                  ##
# #############################################################################
#  Si scaricano una volta e si congelano, come i dati LSEG in ../dati.
fonti <- if (file.exists(FILE_FONTI)) leggi(FILE_FONTI) else
  data.frame(file = character(), url = character(), data_download = character())
registra_fonte <- function(file, url) {
  fonti <<- bind_rows(fonti |> filter(.data$file != !!file),
                      data.frame(file = file, url = url, data_download = format(Sys.Date())))
  write.csv(fonti, FILE_FONTI, row.names = FALSE)
}

for (f in c(FILE_IPT, FILE_EMPLOY)) if (FORZA_RIGENERAZIONE || !file.exists(f)) {
  url <- paste0(URL_RTDSM, sub("^rtdsm_", "", f))
  download.file(url, f, mode = "wb", quiet = TRUE)
  registra_fonte(f, url)
  cat("Scaricato e congelato:", f, "\n")
}

#  Decisioni del FOMC: tabelle annuali della Federal Reserve Board, ciascuna
#  preceduta da un'intestazione con l'anno. Le date sono in inglese e si
#  convertono con month.name, che non dipende dalla lingua del sistema.
if (FORZA_RIGENERAZIONE || !file.exists(FILE_FOMC)) {
  primo_numero <- function(x) suppressWarnings(as.numeric(sub("^\\D*(\\d+(\\.\\d+)?).*$", "\\1", x)))
  leggi_pagina_fomc <- function(url) {
    pg <- rvest::read_html(url)
    bind_rows(lapply(rvest::html_elements(pg, "div.data-table"), function(b) {
      anno <- trimws(rvest::html_text(xml2::xml_find_first(b, "preceding-sibling::h4[1]")))
      tb <- rvest::html_table(rvest::html_element(b, "table"), convert = FALSE)
      names(tb) <- c("giorno", "aumento", "riduzione", "livello")[seq_len(ncol(tb))]
      cbind(anno = anno, tb)
    }))
  }
  grezzo <- bind_rows(lapply(URL_FOMC, leggi_pagina_fomc))
  fomc <- grezzo |>
    mutate(mese  = match(sub("^([A-Za-z]+).*$", "\\1", trimws(giorno)), month.name),
           gg    = as.integer(sub("^[A-Za-z]+\\s+(\\d+).*$", "\\1", trimws(giorno))),
           data  = as.Date(sprintf("%s-%02d-%02d", anno, mese, gg)),
           aumento   = coalesce(primo_numero(aumento), 0),
           riduzione = coalesce(primo_numero(riduzione), 0),
           variazione_pb = aumento - riduzione) |>
    filter(!is.na(data)) |>
    transmute(data, variazione_pb, livello = trimws(livello), data_estrazione = Sys.Date()) |>
    arrange(data)
  if (anyNA(fomc$data) || any(fomc$variazione_pb == 0))
    stop("Decisioni FOMC non interpretate: controllare la tabella di origine.")
  write.csv(fomc, FILE_FOMC, row.names = FALSE)
  registra_fonte(FILE_FOMC, paste(URL_FOMC, collapse = " | "))
  cat("Decisioni FOMC scaricate e congelate:", nrow(fomc), "\n")
}
fomc <- leggi(FILE_FOMC) |> mutate(data = as.Date(data))
cat("Decisioni FOMC:", nrow(fomc), "| dal", format(min(fomc$data)), "al", format(max(fomc$data)),
    "| rialzi", sum(fomc$variazione_pb > 0), "| tagli", sum(fomc$variazione_pb < 0), "\n")


# #############################################################################
# ##  PARTE C2 — REGIME A: FASI DEL CICLO IN TEMPO REALE                    ##
# #############################################################################
#  File RTDSM: una riga per mese di riferimento, una colonna per versione
#  ("IPT03M1" = versione di gennaio 2003). "#N/A" = dato non ancora
#  pubblicato in quella versione.
leggi_rtdsm <- function(f) {
  d <- readxl::read_excel(f, col_types = "text")
  date <- as.Date(paste0(sub(":", "-", d[[1]]), "-01"))
  M <- suppressWarnings(apply(as.matrix(d[, -1]), 2, as.numeric))
  cod <- sub("^[A-Za-z]+", "", names(d)[-1])
  aa <- as.integer(sub("M.*$", "", cod)); mm <- as.integer(sub("^.*M", "", cod))
  versione <- as.Date(sprintf("%d-%02d-01", ifelse(aa >= 60, 1900L, 2000L) + aa, mm))
  stopifnot(!anyNA(date), !anyNA(versione),
            all(date[-1] == date[-length(date)] %m+% months(1)))   # mesi contigui
  list(date = date, versione = versione, M = M)
}
rtdsm <- list(ip = leggi_rtdsm(FILE_IPT), occ = leggi_rtdsm(FILE_EMPLOY))

#  Crescita a 12 mesi meno la sua media mobile degli ultimi 10 anni,
#  standardizzata, dentro UNA versione. Le righe sono mesi contigui
#  (verificato), quindi un ritardo di k righe e' di k mesi. La media mobile
#  guarda solo indietro; la deviazione standard serve soltanto a rendere
#  confrontabili le due serie prima di farne la media.
z_crescita <- function(s, v, media_mobile = MESI_MEDIA_MOBILE) {
  j <- match(v, s$versione)
  if (is.na(j)) stop("Versione ", format(v), " assente nei dati RTDSM.")
  x <- s$M[, j]
  g <- x / dplyr::lag(x, MESI_CRESCITA) - 1
  dev <- if (is.null(media_mobile)) g - mean(g[s$date >= as.Date(INIZIO_STORIA_CICLO)], na.rm = TRUE)
         else g - as.numeric(stats::filter(g, rep(1 / media_mobile, media_mobile), sides = 1))
  ok <- !is.na(dev) & s$date >= as.Date(INIZIO_STORIA_CICLO)
  data.frame(data = s$date[ok], z = dev[ok] / sd(dev[ok]))
}

#  Fase alla data di formazione t, con la sola versione del mese t.
fase_ciclo <- function(t, serie = names(rtdsm), mesi_dir = MESI_DIREZIONE,
                       media_mobile = MESI_MEDIA_MOBILE, lisciatura = MESI_LISCIATURA) {
  zz <- Reduce(function(a, b) inner_join(a, b, by = "data"),
               lapply(serie, function(k) z_crescita(rtdsm[[k]], t, media_mobile) |> rename(!!k := z)))
  zz <- zz |> arrange(data)
  stopifnot(all(zz$data[-1] == zz$data[-nrow(zz)] %m+% months(1)))
  zz$indice <- as.numeric(stats::filter(rowMeans(as.matrix(zz[, serie, drop = FALSE])),
                                        rep(1 / lisciatura, lisciatura), sides = 1))
  ultimo <- max(zz$data)
  liv <- zz$indice[zz$data == ultimo]
  prec <- zz$indice[zz$data == ultimo %m-% months(mesi_dir)]
  dirz <- liv - prec
  data.frame(periodo = t, ultimo_dato = ultimo, livello = liv, direzione = dirz,
             fase4 = if (liv >= 0) { if (dirz >= 0) "Espansione" else "Rallentamento" }
                     else          { if (dirz >= 0) "Ripresa"    else "Contrazione" },
             fase2 = if (liv >= 0) "Sopra il trend" else "Sotto il trend")
}

#  Conferma: la fase adottata a t cambia solo se le ultime `mesi` fasi
#  segnalate (t compreso) coincidono. Dipende solo da segnali fino a t.
conferma_fasi <- function(grezza, mesi = MESI_CONFERMA) {
  adottata <- grezza
  if (mesi > 1) for (i in seq_along(grezza)[-1]) {
    ult <- grezza[max(1, i - mesi + 1):i]
    adottata[i] <- if (i >= mesi && all(ult == grezza[i])) grezza[i] else adottata[i - 1]
  }
  adottata
}

calcola_fasi <- function(..., n_fasi = N_FASI, mesi_conferma = MESI_CONFERMA) {
  f <- bind_rows(lapply(periodi_set, fase_ciclo, ...)) |> arrange(periodo)
  f |> mutate(fase_segnalata = if (n_fasi == 2) fase2 else fase4,
              fase = conferma_fasi(fase_segnalata, mesi_conferma))
}
fasi <- calcola_fasi()

#  Presidi: l'ultimo dato deve precedere la data di formazione (mai lo
#  stesso mese) e non essere troppo vecchio.
ritardo_mesi <- interval(fasi$ultimo_dato, fasi$periodo) %/% months(1)
cat("\nRitardo dell'ultimo dato rispetto alla formazione (mesi):\n")
print(table(ritardo_mesi))
stopifnot(all(ritardo_mesi >= 1), all(ritardo_mesi <= 3))

#  Formato comune dei regimi: periodo, stato, probabilita' per t+1. Il
#  regime noto a t si assume persistente in t+1 (probabilita' 1 o 0).
regime_da_etichette <- function(periodo, etichetta, livelli)
  bind_rows(lapply(seq_along(livelli), function(k)
    data.frame(periodo = periodo, stato = k, p_prevista = as.numeric(etichetta == livelli[k]))))

regime_A <- regime_da_etichette(fasi$periodo, fasi$fase, FASI)

cat("\n========== FASI DEL CICLO (formazioni 2003-", format(max(fasi$periodo), "%Y"), ") ==========\n", sep = "")
print(as.data.frame(fasi |> filter(periodo >= INIZIO_BT) |> count(fase) |>
                      mutate(quota = round(n / sum(n), 3))), row.names = FALSE)

#  Verifica ex post contro le recessioni NBER (solo descrittiva).
in_recessione <- function(p) Reduce(`|`, lapply(RECESSIONI_NBER, function(r)
  p >= as.Date(r[1]) & p <= as.Date(r[2])))
cat("\nFasi nei mesi di recessione NBER:\n")
print(as.data.frame(fasi |> filter(in_recessione(periodo)) |> count(fase)), row.names = FALSE)

#  Criteri di accettazione (PARTE C0), sul campione del backtest.
criteri_fasi <- function(f) {
  f <- f |> filter(periodo >= INIZIO_BT, periodo <= fine_formazione) |> arrange(periodo)
  tratti <- rle(f$fase)
  data.frame(fase = FASI,
             quota  = vapply(FASI, function(k) mean(f$fase == k), numeric(1)),
             durata = vapply(FASI, function(k) mean(tratti$lengths[tratti$values == k]), numeric(1))) |>
    mutate(durata = ifelse(is.nan(durata), 0, durata),
           ok = quota >= QUOTA_MIN_FASE & durata >= DURATA_MIN_FASE)
}
crit <- criteri_fasi(fasi)
cat("\n========== CRITERI DI ACCETTAZIONE DELLE FASI ==========\n")
print(as.data.frame(crit |> mutate(across(where(is.numeric), \(x) round(x, 3)))), row.names = FALSE)
cat("Cambi di fase dal 2005:",
    with(fasi |> filter(periodo >= INIZIO_BT), sum(fase[-1] != fase[-length(fase)])), "|",
    "recessioni NBER nella fase piu' debole (", FASI[length(FASI)], "):",
    sprintf("%.0f%%", 100 * mean(fasi$fase[in_recessione(fasi$periodo)] == FASI[length(FASI)])), "\n")
if (!all(crit$ok)) cat("!! Criteri NON soddisfatti: la specificazione delle fasi va rivista.\n")


# #############################################################################
# ##  PARTE C3 — REGIME C: POLITICA MONETARIA                               ##
# #############################################################################
#  Conover et al. (2008), sulla scia di Jensen, Mercer & Johnson (1996):
#  politica espansiva dopo un taglio del tasso, restrittiva dopo un rialzo,
#  fino alla mossa di segno opposto. Qui il tasso e' il federal funds target
#  (gli autori usano il tasso di sconto, che dal 2003 lo segue in modo
#  meccanico). Al limite dello zero non ci sono mosse: il regime resta quello
#  dell'ultimo taglio, cioe' espansivo, coerente con la politica effettiva.
stance_al <- function(t) {
  fine_mese <- ceiling_date(t, "month") - days(1)
  ultima <- fomc |> filter(data <= fine_mese) |> slice_max(data, n = 1, with_ties = FALSE)
  if (nrow(ultima) == 0) stop("Nessuna decisione FOMC prima di ", format(t))
  if (ultima$variazione_pb < 0) "Espansiva" else "Restrittiva"
}
stance <- data.frame(periodo = periodi_set,
                     stance = vapply(periodi_set, stance_al, character(1)))
regime_C <- regime_da_etichette(stance$periodo, stance$stance, STANCE)

cat("\n========== POLITICA MONETARIA ==========\n")
print(as.data.frame(stance |> filter(periodo >= INIZIO_BT) |> count(stance) |>
                      mutate(quota = round(n / sum(n), 3))), row.names = FALSE)
cat("Cambi di regime dal 2005:",
    with(stance |> filter(periodo >= INIZIO_BT), sum(stance[-1] != stance[-length(stance)])), "\n")


# #############################################################################
# ##  PARTE C4 — PREVISIONE CONDIZIONATA E ROTAZIONE                        ##
# #############################################################################
#  Stesse regole di RegimeSettori.R (PARTE R6): per settore s e regime k
#
#           sum_tau  pi_tau,k * r_s,tau+1  +  PRIOR * m_s
#  mu_s,k = ----------------------------------------------      tau+1 <= t
#                 sum_tau  pi_tau,k        +  PRIOR
#
#  con pi_tau il regime noto a tau. Si detengono gli N_TOP settori con il
#  rendimento atteso piu' alto, equipesati, pesati per capitalizzazione al
#  loro interno.
prob_matrice <- function(reg) {
  w <- reg |> pivot_wider(names_from = stato, values_from = p_prevista) |> arrange(periodo)
  P <- as.matrix(w[, -1]); rownames(P) <- format(w$periodo); P
}

stima_mu <- function(Pp, Rp, prior) {
  m_s <- colMeans(Rp)
  (crossprod(Pp, Rp) + prior * matrix(m_s, ncol(Pp), ncol(Rp), byrow = TRUE)) /
    (colSums(Pp) + prior)
}

previsioni_settori <- function(reg, prior = PRIOR_MESI) {
  P <- if (is.null(reg)) matrix(1, length(periodi_set), 1, dimnames = list(format(periodi_set), "1"))
       else prob_matrice(reg)
  stopifnot(identical(rownames(P), format(periodi_set)))
  periodi_bt <- periodi_set[periodi_set >= INIZIO_BT]
  out <- lapply(periodi_bt, function(t) {
    i <- match(t, periodi_set); m <- i - 1     # formazioni tau < t: r in tau+1 <= t
    mu <- stima_mu(P[seq_len(m), , drop = FALSE], R_mat[seq_len(m), , drop = FALSE], prior)
    setNames(as.numeric(P[i, , drop = FALSE] %*% mu), SETTORI)
  })
  setNames(out, format(periodi_bt))
}

pesi_top <- function(prev, n_top = N_TOP) lapply(prev, function(e) {
  sel <- names(sort(e, decreasing = TRUE))[seq_len(n_top)]
  setNames(rep(1 / n_top, n_top), sel)
})

#  Motore a livello di titolo, identico a RegimeSettori.R: pesi dei settori
#  -> pesi dei titoli, turnover sui pesi derivati, costo per lato.
per_periodo_bt <- split(bt_reg |> select(sec_id, periodo, r), bt_reg$periodo)
per_periodo_mb <- split(membri_ok, membri_ok$periodo)

esegui_rotazione <- function(pesi_settori, costo_unitario = COSTO_UNITARIO) {
  w_prec <- setNames(numeric(0), character(0))
  righe  <- vector("list", length(pesi_settori))
  for (i in seq_along(pesi_settori)) {
    tt <- names(pesi_settori)[i]; W <- pesi_settori[[i]]
    tut <- per_periodo_bt[[tt]]
    cand <- per_periodo_mb[[tt]]
    cand <- cand[cand$settore %in% names(W)[W > 0], ]
    if (length(setdiff(names(W)[W > 0], cand$settore)) > 0) stop("Settore senza membri a ", tt)
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
    righe[[i]] <- data.frame(formazione = as.Date(tt), mese_reso = as.Date(tt) %m+% months(1),
                             n_titoli = length(w), r_lordo = r_l,
                             costo = scambiato * costo_unitario,
                             r_netto = r_l - scambiato * costo_unitario,
                             turnover = scambiato / 2)
    w_prec <- w
  }
  bind_rows(righe)
}

#  Strategie. Ogni coppia differisce per UN elemento:
#    Settori EW      tutti i settori equipesati
#    Incondizionata  top N per media storica, senza regime
#    Ciclo           top N per media condizionata alla fase del ciclo
#    Fed             top N per media condizionata alla politica monetaria
STRATEGIE <- c(EW_SETT = "Settori EW", INCOND = "Incondizionata",
               CICLO = "Regime ciclo", FED = "Regime Fed")
prev <- list(INCOND = previsioni_settori(NULL),
             CICLO  = previsioni_settori(regime_A),
             FED    = previsioni_settori(regime_C))
pesi_strategie <- c(
  list(EW_SETT = lapply(prev$INCOND, function(e) setNames(rep(1 / length(SETTORI), length(SETTORI)), SETTORI))),
  lapply(prev, pesi_top))

cat("\nEsecuzione delle rotazioni...\n")
serie_rot <- lapply(pesi_strategie, esegui_rotazione)
stopifnot(length(unique(vapply(serie_rot, nrow, integer(1)))) == 1,
          all(vapply(serie_rot, function(s) identical(s$mese_reso, serie_rot[[1]]$mese_reso), logical(1))))
cat("Mesi di backtest:", nrow(serie_rot[[1]]), "\n")


# #############################################################################
# ##  PARTE C5 — VALUTAZIONE                                                ##
# #############################################################################
serie_c <- serie_rot$EW_SETT |> select(formazione, mese_reso)
for (k in names(serie_rot))
  serie_c <- serie_c |>
    left_join(serie_rot[[k]] |> select(mese_reso, !!paste0(k, "_lordo") := r_lordo,
                                       !!paste0(k, "_netto") := r_netto,
                                       !!paste0(k, "_tn") := turnover), by = "mese_reso")
serie_c <- serie_c |> left_join(benchmark, by = "mese_reso") |> left_join(fattori, by = "mese_reso")
stopifnot(!anyNA(serie_c$r_bench), !anyNA(serie_c$rf), !anyNA(serie_c$mom))

tabella_c <- bind_rows(
  lapply(names(STRATEGIE), function(k) bind_rows(lapply(c("lordo", "netto"), function(v)
    cbind(strategia = STRATEGIE[[k]], versione = v,
          metriche(serie_c[[paste0(k, "_", v)]], serie_c$r_bench,
                   serie_c[[paste0(k, "_tn")]], serie_c$rf))))),
  cbind(strategia = "Benchmark", versione = "", metriche(serie_c$r_bench, NULL, NULL, serie_c$rf)))

cat("\n========== METRICHE ==========\n")
print(as.data.frame(tabella_c |> select(strategia, versione, rend_annuo, volatilita, sharpe,
                                        max_dd, turnover_medio, active_geom, info_ratio) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)

FATT_C <- c("mkt_rf", "smb", "hml", "mom")
test_c <- bind_rows(lapply(names(STRATEGIE), function(k) {
  y  <- serie_c[[paste0(k, "_netto")]]
  t1 <- regressione_nw(y - serie_c$r_bench)
  t2 <- regressione_nw(y - serie_c$rf, X = serie_c[, FATT_C], nomi = c("mkt", "smb", "hml", "mom"))
  bind_rows(cbind(strategia = STRATEGIE[[k]], test = "Active return medio", t1[t1$termine == "alpha", ]),
            cbind(strategia = STRATEGIE[[k]], test = "Alpha Carhart", t2[t2$termine == "alpha", ]),
            cbind(strategia = STRATEGIE[[k]], test = "Beta di mercato", t2[t2$termine == "mkt", ]))
})) |> mutate(fattore = ifelse(grepl("Beta", test), 1, 12),
              stima_a = stima * fattore, se_a = se_nw * fattore, sig = stelle(p))

COPPIE <- list(c("CICLO", "INCOND"), c("FED", "INCOND"), c("CICLO", "FED"), c("INCOND", "EW_SETT"))
confronti_c <- bind_rows(lapply(COPPIE, function(cp) bind_rows(lapply(c("lordo", "netto"), function(v) {
  d <- regressione_nw(serie_c[[paste0(cp[1], "_", v)]] - serie_c[[paste0(cp[2], "_", v)]])
  cbind(confronto = paste(STRATEGIE[[cp[1]]], "-", STRATEGIE[[cp[2]]]), versione = v, d[d$termine == "alpha", ])
})))) |> mutate(annua = stima * 12, se_a = se_nw * 12, sig = stelle(p))

cat("\n========== SIGNIFICATIVITA' (Newey-West, netto) ==========\n")
print(as.data.frame(test_c |> select(strategia, test, stima_a, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)
cat("\nConfronti a coppie:\n")
print(as.data.frame(confronti_c |> select(confronto, versione, annua, se_a, t, p, sig) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)

SOTTOPERIODI <- list("2005-2007 Pre-crisis"       = c("2005-01-01", "2007-12-31"),
                     "2008-2009 Financial crisis" = c("2008-01-01", "2009-12-31"),
                     "2010-2019 Expansion"        = c("2010-01-01", "2019-12-31"),
                     "2020-2026 Post-COVID"       = c("2020-01-01", "2026-12-31"))
sottoperiodi_c <- bind_rows(lapply(names(SOTTOPERIODI), function(sp) {
  d <- serie_c |> filter(mese_reso >= as.Date(SOTTOPERIODI[[sp]][1]),
                         mese_reso <= as.Date(SOTTOPERIODI[[sp]][2]))
  bind_rows(lapply(names(STRATEGIE), function(k)
    cbind(periodo = sp, strategia = STRATEGIE[[k]], metriche(d[[paste0(k, "_netto")]], d$r_bench, NULL, d$rf))),
    cbind(periodo = sp, strategia = "Benchmark", metriche(d$r_bench, NULL, NULL, d$rf)))
}))

#  Regimi e mercato nel mese successivo: descrittivo, non entra nelle scelte.
etichette_regimi <- bind_rows(
  fasi |> transmute(periodo, metodo = "Ciclo", regime = fase),
  stance |> transmute(periodo, metodo = "Fed", regime = stance)) |>
  filter(periodo >= INIZIO_BT, periodo <= fine_formazione)
regimi_c <- etichette_regimi |>
  mutate(mese_reso = periodo %m+% months(1)) |>
  inner_join(mercato |> select(mese_reso = periodo, r_mkt), by = "mese_reso") |>
  group_by(metodo, regime) |>
  summarise(mesi = n(), media_annua = 12 * mean(r_mkt), vol_annua = sqrt(12) * sd(r_mkt), .groups = "drop") |>
  group_by(metodo) |> mutate(quota = mesi / sum(mesi)) |> ungroup()
cambi_c <- etichette_regimi |> arrange(metodo, periodo) |> group_by(metodo) |>
  summarise(cambi = sum(regime[-1] != regime[-n()]), .groups = "drop")
cat("\n========== REGIMI E MERCATO NEL MESE SUCCESSIVO ==========\n")
print(as.data.frame(regimi_c |> left_join(cambi_c, by = "metodo") |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)


# #############################################################################
# ##  PARTE C6 — ROBUSTEZZA                                                 ##
# #############################################################################
#  Ogni variante cambia UN elemento della rotazione sul ciclo. Verificano,
#  non selezionano.
varianti <- list(
  "Baseline (2 phases, IP + payrolls, 5-month confirmation, top 3)" = list(gruppo = "Baseline"),
  "4 phases (level x direction)" = list(gruppo = "Cycle phases", n_fasi = 4),
  "No confirmation"             = list(gruppo = "Cycle phases", conferma = 1),
  "Industrial production only"  = list(gruppo = "Cycle indicator", serie = "ip"),
  "Payroll employment only"     = list(gruppo = "Cycle indicator", serie = "occ"),
  "Top 2 sectors"               = list(gruppo = "Sectors held", n_top = 2),
  "Top 4 sectors"               = list(gruppo = "Sectors held", n_top = 4),
  "Prior 36 months"             = list(gruppo = "Shrinkage", prior = 36),
  "25 bp"                       = list(gruppo = "Transaction costs", costo = 0.0025),
  "50 bp"                       = list(gruppo = "Transaction costs", costo = 0.0050))

robustezza_c <- bind_rows(lapply(names(varianti), function(nm) {
  v <- varianti[[nm]]
  reg <- if (is.null(v$serie) && is.null(v$n_fasi) && is.null(v$conferma)) regime_A else {
    nf <- if (is.null(v$n_fasi)) N_FASI else v$n_fasi
    fv <- calcola_fasi(serie = if (is.null(v$serie)) names(rtdsm) else v$serie, n_fasi = nf,
                       mesi_conferma = if (is.null(v$conferma)) MESI_CONFERMA else v$conferma)
    regime_da_etichette(fv$periodo, fv$fase, if (nf == 2) FASI_2 else FASI_4)
  }
  pv <- previsioni_settori(reg, prior = if (is.null(v$prior)) PRIOR_MESI else v$prior)
  s  <- esegui_rotazione(pesi_top(pv, if (is.null(v$n_top)) N_TOP else v$n_top),
                         if (is.null(v$costo)) COSTO_UNITARIO else v$costo)
  d  <- s |> left_join(serie_c |> select(mese_reso, r_bench, rf, INCOND_netto), by = "mese_reso")
  dd <- regressione_nw(d$r_netto - d$INCOND_netto)
  cbind(gruppo = v$gruppo, variante = nm, metriche(d$r_netto, d$r_bench, d$turnover, d$rf),
        vs_incond = dd$stima[1] * 12, t_vs_incond = dd$t[1])
}))
cat("\n========== ROBUSTEZZA (rotazione sul ciclo, netto) ==========\n")
print(as.data.frame(robustezza_c |> select(gruppo, variante, rend_annuo, sharpe, active_geom,
                                           turnover_medio, vs_incond, t_vs_incond) |>
                      mutate(across(where(is.numeric), \(x) round(x, 4)))), row.names = FALSE)


# #############################################################################
# ##  PARTE C7 — TABELLE E GRAFICI                                          ##
# #############################################################################
TRAD_C <- c("Settori EW" = "Equal-weight sectors", "Incondizionata" = "Unconditional rotation",
            "Regime ciclo" = "Business-cycle rotation", "Regime Fed" = "Monetary-policy rotation",
            "Benchmark" = "Benchmark (S&P 500 TR)", "lordo" = "Gross of costs", "netto" = "Net of costs",
            "Active return medio" = "Mean active return", "Alpha Carhart" = "Carhart alpha",
            "Beta di mercato" = "Market beta",
            "Ciclo" = "Business cycle", "Fed" = "Monetary policy",
            "Ripresa" = "Recovery", "Espansione" = "Expansion", "Rallentamento" = "Slowdown",
            "Contrazione" = "Contraction", "Espansiva" = "Expansionary", "Restrittiva" = "Restrictive",
            "Sopra il trend" = "Above trend", "Sotto il trend" = "Below trend")
en_c <- function(x) { x <- as.character(x); ifelse(x %in% names(TRAD_C), unname(TRAD_C[x]), x) }
en_coppia <- function(x) paste(en_c(sub(" - .*$", "", x)), "minus", en_c(sub("^.* - ", "", x)))
pct <- function(x, d = 2) ifelse(is.na(x), "", paste0(formatC(100*x, format="f", digits=d), "%"))
num <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format="f", digits=d))

tabC1 <- tabella_c |> transmute(
  Strategy = en_c(strategia), Version = en_c(versione), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
  `Average monthly turnover` = pct(turnover_medio),
  `Active return (geometric)` = pct(active_geom), `Tracking error` = pct(tracking_err),
  `Information ratio` = num(info_ratio))

tabC2 <- bind_rows(
  test_c |> transmute(Group = "Versus benchmark, net of costs",
                      Item = paste0(en_c(strategia), ": ", en_c(test)),
                      Estimate = ifelse(grepl("Beta", test), num(stima_a, 3), pct(stima_a)),
                      `Std. error (Newey-West)` = ifelse(grepl("Beta", test), num(se_a, 3), pct(se_a)),
                      `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig),
  confronti_c |> transmute(Group = "Pairwise differences", Item = paste0(en_coppia(confronto), ", ", en_c(versione)),
                           Estimate = pct(annua), `Std. error (Newey-West)` = pct(se_a),
                           `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig))

tabC3 <- regimi_c |> left_join(cambi_c, by = "metodo") |>
  mutate(ordine = match(regime, c(FASI, STANCE))) |> arrange(metodo, ordine) |>
  transmute(`Regime model` = en_c(metodo), Regime = en_c(regime), Months = mesi,
            `Share of months` = pct(quota, 1), `Regime switches` = cambi,
            `Market return, next month (ann.)` = pct(media_annua),
            `Market volatility, next month (ann.)` = pct(vol_annua))

tabC4 <- sottoperiodi_c |> transmute(
  Subperiod = periodo, Strategy = en_c(strategia), Months = n_mesi,
  `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
  `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
  `Active return (geometric)` = pct(active_geom))

tabC5 <- robustezza_c |> transmute(
  Group = gruppo, Variant = variante,
  `Annualized return` = pct(rend_annuo), `Sharpe ratio` = num(sharpe),
  `Active return (geometric)` = pct(active_geom), `Average monthly turnover` = pct(turnover_medio),
  `Difference vs unconditional (ann.)` = pct(vs_incond), `t-stat (Newey-West)` = num(t_vs_incond))

nomi_tabC <- c("table_C1_performance", "table_C2_significance", "table_C3_regimes",
               "table_C4_subperiods", "table_C5_robustness")
for (i in 1:5) write.csv(get(paste0("tabC", i)), paste0("tabelle_ciclo/", nomi_tabC[i], ".csv"),
                         row.names = FALSE, na = "")
write_xlsx(list(Performance = tabC1, Significance = tabC2, Regimes = tabC3,
                Subperiods = tabC4, Robustness = tabC5), "tabelle_ciclo/cycle_tables.xlsx")

# =============================== GRAFICI =====================================
ETI_C <- c("Business-cycle rotation", "Monetary-policy rotation", "Unconditional rotation",
           "Equal-weight sectors", "Benchmark (S&P 500 TR)")
COL_C <- c("Business-cycle rotation" = "#1F4E79", "Monetary-policy rotation" = "#C55A11",
           "Unconditional rotation" = "#7B3F99", "Equal-weight sectors" = "#B8860B",
           "Benchmark (S&P 500 TR)" = "#7F7F7F")
COL_FASI <- c(`Above trend` = "#1F4E79", `Below trend` = "#B23A2E",
              Recovery = "#8FB8DE", Expansion = "#1F4E79", Slowdown = "#E0A458", Contraction = "#B23A2E",
              Expansionary = "#2E7D32", Restrictive = "#C55A11")
COL_FASI <- COL_FASI[c(en_c(FASI), "Expansionary", "Restrictive")]
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        plot.caption = element_text(colour = "grey45", size = 8, hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9, colour = "grey30"))
mese_en <- function(d) paste(month.name[month(d)], year(d))
PERIODO_C <- paste(mese_en(min(serie_c$mese_reso)), "to", mese_en(max(serie_c$mese_reso)))
DIDA_C <- paste0("Top ", N_TOP, " TRBC economic sectors by regime-conditional expected return, ",
                 "cap-weighted within sector, monthly rebalancing, 10 bp per side.")

# ---- C1. Regimi nel tempo --------------------------------------------------
striscia <- bind_rows(
  fasi |> transmute(periodo, riga = "Business cycle (real-time)", regime = en_c(fase)),
  stance |> transmute(periodo, riga = "Monetary policy", regime = en_c(stance))) |>
  mutate(riga = factor(riga, levels = c("Monetary policy", "Business cycle (real-time)")),
         regime = factor(regime, levels = names(COL_FASI)))
rec <- data.frame(inizio = as.Date(sapply(RECESSIONI_NBER, `[`, 1)),
                  fine = as.Date(sapply(RECESSIONI_NBER, `[`, 2)) %m+% months(1))
ggsave("grafici_ciclo/figure_C1_regimes.png",
       ggplot(striscia) +
         geom_rect(data = rec, aes(xmin = inizio, xmax = fine, ymin = .4, ymax = 2.6),
                   fill = "grey80", alpha = .6, inherit.aes = FALSE) +
         geom_tile(aes(periodo, riga, fill = regime), height = .7) +
         scale_fill_manual(values = COL_FASI, drop = FALSE) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Regimes known at each formation date",
              subtitle = "Business-cycle phase from real-time vintages of industrial production and payrolls; grey bands are NBER recessions (ex post, for reference only)",
              x = NULL, y = NULL) + tema + guides(fill = guide_legend(nrow = 1)),
       width = 10, height = 3.6, dpi = 300)

# ---- C2. Curva di ricchezza ------------------------------------------------
larga <- serie_c |> transmute(mese_reso,
  `Business-cycle rotation`  = cumprod(1 + CICLO_netto) * 100,
  `Monetary-policy rotation` = cumprod(1 + FED_netto) * 100,
  `Unconditional rotation`   = cumprod(1 + INCOND_netto) * 100,
  `Equal-weight sectors`     = cumprod(1 + EW_SETT_netto) * 100,
  `Benchmark (S&P 500 TR)`   = cumprod(1 + r_bench) * 100)
lunga <- larga |> pivot_longer(-mese_reso, names_to = "serie", values_to = "valore") |>
  mutate(serie = factor(serie, levels = ETI_C))
ggsave("grafici_ciclo/figure_C2_wealth.png",
       ggplot(lunga, aes(mese_reso, valore, colour = serie)) +
         geom_line(linewidth = .7) + scale_colour_manual(values = COL_C) +
         scale_y_log10(labels = label_number(accuracy = 1)) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         labs(title = "Growth of 100 invested",
              subtitle = paste0("Net of transaction costs, log scale, ", PERIODO_C),
              x = NULL, y = "Value (base 100)", caption = DIDA_C) + tema,
       width = 9, height = 5.2, dpi = 300)

# ---- C3. Settori detenuti dalla rotazione sul ciclo ------------------------
detenuti <- bind_rows(lapply(names(pesi_strategie$CICLO), function(tt)
  data.frame(periodo = as.Date(tt), settore = names(pesi_strategie$CICLO[[tt]])))) |>
  left_join(fasi |> select(periodo, fase), by = "periodo") |> mutate(fase = en_c(fase))
ggsave("grafici_ciclo/figure_C3_sectors_held.png",
       ggplot(detenuti, aes(periodo, settore, fill = fase)) +
         geom_tile(height = .8) +
         scale_fill_manual(values = COL_FASI) +
         scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
         scale_y_discrete(limits = rev(SETTORI)) +
         labs(title = "Sectors held by the business-cycle rotation",
              subtitle = "Formation month, coloured by the cycle phase known at formation",
              x = NULL, y = NULL) + tema,
       width = 9, height = 4.8, dpi = 300)

# ---- C4. Rendimenti dei settori per fase all'ultima data -------------------
m_fin <- nrow(R_mat) - 1
mu_fin <- stima_mu(prob_matrice(regime_A)[seq_len(m_fin), , drop = FALSE],
                   R_mat[seq_len(m_fin), , drop = FALSE], PRIOR_MESI)
mu_fin <- data.frame(settore = rep(SETTORI, each = length(FASI)),
                     fase = factor(rep(en_c(FASI), length(SETTORI)), levels = en_c(FASI)),
                     mu = 12 * as.numeric(mu_fin))
ggsave("grafici_ciclo/figure_C4_returns_by_phase.png",
       ggplot(mu_fin, aes(mu, settore, fill = fase)) +
         geom_col(position = position_dodge(width = .85), width = .8) +
         geom_vline(xintercept = 0, colour = "grey40", linewidth = .3) +
         scale_fill_manual(values = COL_FASI) +
         scale_x_continuous(labels = percent_format(accuracy = 1)) +
         scale_y_discrete(limits = rev(SETTORI)) +
         labs(title = "Sector returns by business-cycle phase",
              subtitle = paste0("Annualized, next-month returns, estimated with information up to ",
                                mese_en(fine_formazione), "; shrunk towards each sector's mean"),
              x = NULL, y = NULL) + tema,
       width = 8.5, height = 6, dpi = 300)

cat("\n========== TABELLA PRINCIPALE ==========\n")
print(as.data.frame(tabC1 |> filter(Version != "Gross of costs")), row.names = FALSE)
cat("\nStudio completato: tabelle_ciclo/ e grafici_ciclo/.\n")
