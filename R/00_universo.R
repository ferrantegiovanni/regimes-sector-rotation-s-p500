# =============================================================================
#  UNIVERSO E DATASET DEI COSTITUENTI STORICI DELL'S&P 500
#  Costruisce, con i moduli generici di questa cartella, gli oggetti comuni a
#  tutti gli studi del progetto:
#    iv         intervalli di appartenenza all'indice [date_in, date_out)
#    ponte      codice RIC -> chiave di security (PermID + ShareClass)
#    pannello   una security, un mese, una riga: r, prezzo, mktcap, in_indice
#    bt_reg     timeline: eleggibilita' alla formazione t e rendimento in t+1,
#               con i rendimenti di delisting gia' applicati
#    benchmark  S&P 500 Total Return (mese_reso, r_bench)
#    fattori    tasso privo di rischio e fattori di Kenneth French
#
#  Uso: dagli script degli studi,
#    DIR_PROGETTO <- ".."      # cartella radice del progetto
#    source(file.path(DIR_PROGETTO, "R", "00_universo.R"))
#
#  DATI. Stanno in dati/ e NON fanno parte del repository: i dati di mercato
#  provengono da LSEG con licenza universitaria e non sono ridistribuibili.
#  Ogni fase si esegue solo se il suo file manca (o con FORZA_RIGENERAZIONE):
#  la prima volta servono LSEG Workspace e la variabile d'ambiente
#  LSEG_APP_KEY; poi tutto si legge dai file congelati.
#    SPX Current Constituents.xlsx            costituenti correnti     (LSEG)
#    SPX Leavers & Joiners Constituents.xlsx  eventi di indice         (LSEG)
#    SPX_membership_intervals.csv             membership               fase 1
#    crosswalk_universo_finale.csv            chiave di security       fase 2
#    prezzi_mensili_grezzi.csv                serie mensili            fase 3
#    delisting_compilato.xlsx                 ricerca documentale sulle uscite
#    benchmark_spxtr.csv                      benchmark total return   fase 6
#    fattori_french.csv                       fattori French           fase 6
# =============================================================================
if (!exists("DIR_PROGETTO")) DIR_PROGETTO <- "."
for (.m in c("01_dati_pit.R", "02_segnali_backtest.R", "04_valutazione.R"))
  source(file.path(DIR_PROGETTO, "R", .m), encoding = "UTF-8")
rm(.m)
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(ggplot2); library(scales); library(quadprog)
})

# -----------------------------------------------------------------------------
#  PARAMETRI
# -----------------------------------------------------------------------------
FORZA_RIGENERAZIONE <- FALSE          # TRUE: riestrae tutto da LSEG
BASELINE_INDICE  <- "1994-12-30"      # primo giorno coperto dal database LSEG
DATA_INIZIO      <- "2003-01-01"      # inizio delle serie (finestra di stima)
DATA_BACKTEST    <- "2005-01-31"      # prima data di formazione valutata
DATA_FINE        <- "2026-06-30"
COSTO_UNITARIO   <- 0.0010            # 10 bp PER LATO sul movimentato
REGOLA_DELISTING <- "effettiva"       # documentale dove disponibile, altrimenti R_CONSERVATIVO
R_CONSERVATIVO   <- 0                 # liquidazione all'ultimo prezzo noto
RIC_BENCHMARK    <- ".SPXTR"          # sempre la versione TOTAL RETURN

DIR_DATI <- file.path(DIR_PROGETTO, "dati")
f_dati <- function(x) file.path(DIR_DATI, x)
FILE_CORRENTI   <- f_dati("SPX Current Constituents.xlsx")
FILE_EVENTI     <- f_dati("SPX Leavers & Joiners Constituents.xlsx")
FILE_INTERVALLI <- f_dati("SPX_membership_intervals.csv")
FILE_CROSSWALK  <- f_dati("crosswalk_universo_finale.csv")
FILE_PREZZI     <- f_dati("prezzi_mensili_grezzi.csv")
FILE_DELISTING  <- f_dati("delisting_compilato.xlsx")
FILE_BENCH      <- f_dati("benchmark_spxtr.csv")
FILE_FATTORI    <- f_dati("fattori_french.csv")

if (!dir.exists(DIR_DATI))
  stop("Cartella dei dati non trovata: ", normalizePath(DIR_DATI, mustWork = FALSE),
       ". I dati non sono nel repository: vanno estratti da LSEG (vedi README).")
cat("Dati da:", normalizePath(DIR_DATI), "\n")

# -----------------------------------------------------------------------------
#  FASE 1 — MEMBERSHIP POINT-IN-TIME
# -----------------------------------------------------------------------------
if (da_rigenerare(FILE_INTERVALLI, FORZA_RIGENERAZIONE)) {
  mb <- ricostruisci_membership(leggi_eventi_lseg(FILE_EVENTI), leggi_correnti_lseg(FILE_CORRENTI),
                                BASELINE_INDICE, Sys.Date())
  write.csv(mb$iv, FILE_INTERVALLI, row.names = FALSE, na = "")
}
iv <- leggi(FILE_INTERVALLI) |> mutate(date_in = as.Date(date_in), date_out = as.Date(date_out))

# -----------------------------------------------------------------------------
#  FASE 2 — CHIAVE DI SECURITY (crosswalk congelato)
# -----------------------------------------------------------------------------
#  Solo i codici che sono stati membri nel periodo delle serie. I gruppi di
#  codici con la stessa chiave si verificano sulle serie di prezzo.
if (da_rigenerare(FILE_CROSSWALK, FORZA_RIGENERAZIONE)) {
  if (!exists("rd")) rd <- connetti_lseg()
  iv_p <- iv |> filter(is.na(date_out) | date_out >= as.Date(DATA_INIZIO), date_in <= as.Date(DATA_FINE))
  rics <- sort(unique(trimws(sub(" \\(expired\\)", "", iv_p$code))))
  cw <- estrai_a_lotti(rics, lseg_identificativi(), lotto = 50, completo = function(d) !is.na(d$PermID)) |>
    distinct(RIC, .keep_all = TRUE) |> costruisci_chiave()
  f_px <- function(r) lseg_serie_mensili(DATA_INIZIO, DATA_FINE, "TR.PriceClose")(r) |>
    transmute(RIC, Data, p = prezzo) |> filter(!is.na(p))
  dz <- disambigua_crosswalk(cw, f_px)
  write.csv(dz$crosswalk |> mutate(data_estrazione = Sys.Date()), FILE_CROSSWALK, row.names = FALSE, na = "")
}
ponte <- leggi(FILE_CROSSWALK) |>
  come_testo(c("RIC", "PermID", "QuoteId", "ShareClass", "ISIN", "Nome", "sec_id", "classe")) |>
  filter(!is.na(RIC), !is.na(sec_id)) |> distinct(RIC, .keep_all = TRUE) |>
  select(RIC, sec_id, PermID, ShareClass, ISIN, Nome)
cat("Crosswalk:", nrow(ponte), "codici |", n_distinct(ponte$sec_id), "security\n")

# -----------------------------------------------------------------------------
#  FASE 3 — SERIE MENSILI (storia intera, congelata)
# -----------------------------------------------------------------------------
if (da_rigenerare(FILE_PREZZI, FORZA_RIGENERAZIONE)) {
  if (!exists("rd")) rd <- connetti_lseg()
  px <- estrai_a_lotti(unique(ponte$RIC), lseg_serie_mensili(DATA_INIZIO, DATA_FINE), lotto = 25,
                       completo = function(d) !is.na(d$Data) & !(is.na(d$ret) & is.na(d$prezzo)))
  write.csv(px, FILE_PREZZI, row.names = FALSE, na = "")
}
mensili <- leggi(FILE_PREZZI) |> mutate(RIC = trimws(as.character(RIC)), Data = as.Date(Data))
#  Un file estratto con i nomi dei campi LSEG (rendimento in percentuale) si
#  porta al formato del modulo: ret decimale, prezzo, mktcap.
if ("TotalReturn" %in% names(mensili))
  mensili <- mensili |> transmute(RIC, Data, ret = TotalReturn / 100, prezzo = PriceClose, mktcap = MktCap)

# -----------------------------------------------------------------------------
#  FASE 4 — PANNELLO CANONICO
# -----------------------------------------------------------------------------
pannello <- costruisci_pannello(mensili, ponte, iv, DATA_FINE)

# -----------------------------------------------------------------------------
#  FASE 5 — TIMELINE E RENDIMENTI DI DELISTING
# -----------------------------------------------------------------------------
#  Informazione fino a fine mese t -> formazione -> rendimento in t+1.
#  Gli studi del progetto selezionano per settore, non per segnale del
#  singolo titolo: il segnale della timeline e' quindi una costante.
segnale_costante <- function(df) df |> transmute(sec_id, periodo, segnale = 1)
bt <- prepara_timeline(pannello, segnale_costante, DATA_BACKTEST, DATA_FINE)

#  Uscite definitive: rendimento terminale dalla ricerca documentale dove
#  disponibile, altrimenti liquidazione all'ultimo prezzo. La regola vale per
#  tutte le uscite, anche quelle della finestra di stima precedente al
#  backtest.
effettivi <- if (file.exists(FILE_DELISTING))
  rendimenti_terminali(as.data.frame(readxl::read_excel(FILE_DELISTING, sheet = 1)), pannello) else NULL
cat("Rendimenti terminali da ricerca documentale:", if (is.null(effettivi)) 0 else nrow(effettivi), "\n")
cat("Rendimenti in t+1 mancanti fra gli eleggibili del backtest:\n")
casi <- classifica_mancanti(bt)
if (any(casi$tipo == "buco interno")) stop("Buchi interni nelle serie: casi da esaminare.")
bt_reg <- applica_delisting(bt, REGOLA_DELISTING, effettivi, R_CONSERVATIVO)

# -----------------------------------------------------------------------------
#  FASE 6 — BENCHMARK E FATTORI
# -----------------------------------------------------------------------------
benchmark <- if (file.exists(FILE_BENCH)) {
  leggi(FILE_BENCH) |> mutate(mese_reso = as.Date(mese_reso))
} else {
  if (!exists("rd")) rd <- connetti_lseg()
  carica_benchmark_lseg(RIC_BENCHMARK, DATA_INIZIO, DATA_FINE, FILE_BENCH)
}
fattori <- carica_fattori(FILE_FATTORI, "US")

cat("\n========== UNIVERSO E DATASET PRONTI ==========\n\n")
