# Regime di mercato e rotazione settoriale

Rotazione settoriale guidata dal regime di mercato sui costituenti storici dell'S&P 500. L'universo e il dataset dei costituenti vengono dalla pipeline dei dati `../R/00_universo.R`, costruita con i moduli generici in `../R`: membership point-in-time, chiave di security, pannello mensile, rendimenti di delisting, benchmark e fattori. I dati stanno in `../dati`, che non fa parte del repository.

Le decisioni metodologiche, con motivazioni ed evidenze, sono in [REGISTRO_DECISIONI.md](REGISTRO_DECISIONI.md). La sintesi dei risultati, in inglese, è in [RESULTS_SUMMARY.md](RESULTS_SUMMARY.md).

## I tre studi

| Script | Domanda | Dipende da |
|---|---|---|
| `RegimeSettori.R` | Un regime di mercato stimato con un HMM migliora la rotazione settoriale? | `../R/00_universo.R` |
| `RotazioneCiclo.R` | Le fasi del ciclo economico in tempo reale, o la politica monetaria, migliorano la rotazione settoriale? | `RegimeSettori.R` fino alla PARTE R3 |
| `CostruzioneCiclo.R` | A parità di segnale, quanto conta il metodo di costruzione del portafoglio? | `RotazioneCiclo.R` fino alla PARTE C4 |

Ogni script esegue da solo quelli da cui dipende (fino alla parte indicata), quindi si può lanciare direttamente quello che interessa.

## Come si esegue

1. Aprire `studi.Rproj`, così la directory di lavoro è questa cartella.
2. Eseguire lo script per intero (Ctrl+Shift+Enter), oppure da terminale:

   ```
   Rscript RegimeSettori.R
   Rscript RotazioneCiclo.R
   Rscript CostruzioneCiclo.R
   ```

Tempi indicativi con tutti i file già presenti:
- `RegimeSettori.R`: circa 1 minuto;
- `RotazioneCiclo.R`: circa 2 minuti;
- `CostruzioneCiclo.R`: circa 3 minuti.

La pipeline dei dati richiede circa 45 secondi ed è inclusa in ciascun tempo.

**Prerequisiti**
- R con `dplyr`, `tidyr`, `lubridate`, `readxl`, `writexl`, `ggplot2`, `scales`, `quadprog`, più `rvest` e `xml2` (solo per scaricare le decisioni FOMC) e `Refinitiv` (solo per le estrazioni LSEG).
- I file dei dati in `../dati` (vedi l'intestazione di `../R/00_universo.R`).

**Avviso normale in esecuzione**: "il pacchetto … è stato creato con R versione 4.5.3" segnala solo una differenza di versione minore.

## Dati esterni, congelati alla prima esecuzione

| File | Contenuto | Fonte | Scaricato |
|---|---|---|---|
| `settori_lseg.csv` | Settore GICS e TRBC per RIC (si usa il TRBC) | LSEG Workspace (`TR.GICSSector`, `TR.TRBCEconomicSector`) | 2026-11-01 |
| `rtdsm_iptMvMd.xlsx` | Produzione industriale, tutte le versioni mensili dal 1962 | Philadelphia Fed, Real-Time Data Set for Macroeconomists | 2026-11-10 |
| `rtdsm_employMvMd.xlsx` | Occupazione non agricola, tutte le versioni mensili dal 1964 | Philadelphia Fed, Real-Time Data Set for Macroeconomists | 2026-11-10 |
| `fomc_decisioni.csv` | Decisioni sul tasso obiettivo dei fed funds dal 1990 | Federal Reserve Board, pagine *Open Market Operations* | 2026-11-10 |
| `fonti_ciclo.csv` | Indirizzo e data di download dei tre file precedenti | — | — |
| `regimi_hmm.rds` | Cache delle stime HMM espandenti (si ricalcola da sola se cambiano parametri o dati) | calcolato | — |

Lo scaricamento dei settori richiede Workspace aperto e la variabile d'ambiente `LSEG_APP_KEY`. Gli altri file si scaricano senza credenziali. Per riscaricare tutto si imposta `FORZA_RIGENERAZIONE <- TRUE` in `../R/00_universo.R`.

## Output

| Cartella | Studio | Contenuto |
|---|---|---|
| `tabelle_regime/`, `grafici_regime/` | HMM | 5 tabelle (anche in `regime_tables.xlsx`), 4 grafici |
| `tabelle_ciclo/`, `grafici_ciclo/` | Ciclo e politica monetaria | 5 tabelle (anche in `cycle_tables.xlsx`), 4 grafici |
| `tabelle_costruzione/`, `grafici_costruzione/` | Metodo di costruzione | 5 tabelle (anche in `construction_tables.xlsx`), 4 grafici |

Tabelle e grafici sono in inglese, come nel resto del progetto; codice e commenti sono in italiano.
