# Registro delle decisioni — Regime di mercato e rotazione settoriale

Ogni voce riporta la decisione, la motivazione, le alternative considerate, l'evidenza su cui si basa, l'effetto misurato e lo stato. Vale la regola del protocollo del progetto: **la specifica si fissa prima dei risultati, e un risultato sfavorevole è un esito, non un difetto da correggere**. Quando una decisione è stata presa dopo aver visto dei numeri, la voce dice quali.

Indice:
- [Studio 1 — HMM e rotazione settoriale](#studio-1--hmm-e-rotazione-settoriale-regimesettorir) (D1-D5)
- [Studio 2 — Ciclo economico e politica monetaria](#studio-2--ciclo-economico-e-politica-monetaria-rotazionecicloR) (D6-D15)
- [Studio 3 — Metodo di costruzione](#studio-3--metodo-di-costruzione-costruzionecicloR) (D16-D20)
- [Organizzazione dei file](#organizzazione-dei-file) (D21-D22)

---

## Studio 1 — HMM e rotazione settoriale (`RegimeSettori.R`)

### D1. Specifica dello studio — 2026-10-31

**Decisione.**
- **Regime**: identificato con un hidden Markov model gaussiano a 2 stati (calmo e turbolento) sull'excess return mensile del mercato (Kenneth French, Mkt-RF, dal luglio 1926).
  - Stima per massima verosimiglianza (EM/Baum-Welch), ripetuta **a ogni data di formazione su finestra espandente**.
  - Partenza dalle stime del mese precedente; ogni 12 mesi anche da valori standard, tenendo la verosimiglianza migliore.
  - Solo **probabilità filtrate**, mai smussate. La previsione per t+1 è P(S_t | F_t) × A.
  - Confronto con due regole: media mobile a 10 mesi (Faber 2007) e rendimento di mercato a 24 mesi negativo (Daniel & Moskowitz 2016).
- **Rotazione**:
  - a ogni t, rendimento atteso di ciascun settore = media dei rendimenti settoriali realizzati fino a t, pesata con le probabilità di stato **prodotte in tempo reale** a ciascuna data;
  - shrinkage verso la media del settore con peso di 12 mesi;
  - si detengono i 3 settori migliori, equipesati, con pesi di capitalizzazione al loro interno;
  - ribilanciamento mensile, 10 bp per lato.
- **Confronti**: settori equipesati, rotazione incondizionata (stessa regola senza regime), benchmark.

**Motivazione.**
- La serie French offre una lunga storia di stima già alla prima formazione.
- La stima condizionata lascia ai dati, e non alla letteratura, la scelta dei settori per regime.
- Il confronto con la rotazione incondizionata isola il contributo del regime: una differenza alla volta.

**Alternative considerate.**
- Allocazione momentum ↔ difensivo, o azioni ↔ T-bill, invece della rotazione settoriale.
- Mappatura ciclici/difensivi fissata ex ante.
- Switch binario invece della miscela per probabilità.
- S&P 500 TR scaricato da LSEG invece del mercato French.

**Stato.** Confermata dall'autore il 2026-10-31, prima di qualunque risultato.

### D2. Prima formazione del backtest — 2026-10-31

**Decisione.** La prima formazione si confronta con `floor_date(DATA_BACKTEST, "month")`, non con `DATA_BACKTEST`.

**Motivazione.** `DATA_BACKTEST` è una data di fine mese (2005-01-31), mentre `periodo` è il primo giorno del mese. Il confronto diretto escludeva gennaio 2005: 256 mesi invece dei 257 della pipeline dei dati.

**Evidenza.** Dopo la correzione i mesi sono 257 e il benchmark rende 11,11% annuo, identico a quello della pipeline dei dati.

**Stato.** Correzione di un errore, nessuna scelta metodologica.

### D3. Uscite dall'indice prima del backtest — 2026-10-31

**Decisione.** Due membri escono definitivamente nel biennio di sola stima: Southtrust Corp (formazione 2004-10) e FleetBoston Financial Corp (formazione 2004-03). Ricevono il rendimento terminale conservativo della pipeline dei dati (0).

**Motivazione.** La prima pipeline dei dati trattava i delisting solo dalle formazioni del 2005; qui il 2003-2004 serve a stimare le medie settoriali. Dal 2026-11-11 la regola è applicata a tutte le uscite dal modulo 02 (D22), con lo stesso esito. Si applicano la stessa classificazione delle uscite e la stessa regola conservativa. Un buco interno resta un caso da esaminare e ferma l'esecuzione.

**Alternative.** Ricerca documentale del corrispettivo effettivo, come nella pipeline dei dati. Non svolta, perché i casi sono due e riguardano solo la finestra di stima.

**Stato.** Applicata.

### D4. Classificazione settoriale: da GICS a TRBC — 2026-11-01

**Decisione.** Si usa il **settore economico TRBC** (Refinitiv Business Classification) per tutti i titoli, non il GICS. Il settore TRBC "Academic & Educational Services" (3 RIC) confluisce in Consumer Cyclicals.

**Motivazione.** LSEG non restituisce il GICS per **290 dei 369 RIC scaduti**. Escluderli avrebbe tolto dai panieri settoriali proprio i titoli usciti dall'indice, una distorsione legata alla sopravvivenza. Il TRBC copre il 100% dei RIC, scaduti compresi.

**Evidenza.**
- Con il GICS, ogni mese restavano senza settore in mediana 78 membri dell'indice (massimo 187), e nel mese peggiore restava fuori il 19,5% della capitalizzazione.
- Sui RIC scaduti sono stati provati anche `TR.GICSSector` a una data storica (`SDate`), `TR.GICSSectorCode`, `TR.GICSIndustry` e la serie storica: tutti vuoti.
- Il TRBC coincide in larga parte con il GICS sui titoli che hanno entrambi. La differenza principale: Communication Services del GICS è ripartito fra Consumer Cyclicals e Technology.
- Accorpamento: GICS colloca le società di "Academic & Educational Services" in Consumer Discretionary; il paniere a sé sarebbe rimasto vuoto in molti mesi.

**Alternative.**
- GICS dove disponibile e TRBC tradotto in GICS per i titoli usciti: scartata, perché classificherebbe in modo sistematicamente diverso sopravvissuti e usciti.
- Solo GICS, escludendo i titoli senza: scartata per la distorsione di sopravvivenza.

**Nota di trasparenza.** La prima esecuzione con il GICS ha prodotto e stampato rendimenti calcolati sull'indice parziale. Sono stati scartati senza essere interpretati: la decisione si basa sul controllo di copertura, non su quei numeri.

**Limite dichiarato.** Il settore è l'ultimo noto, applicato all'intero periodo.

**Stato.** Confermata dall'autore il 2026-11-01.

### D5. Esito dello studio 1

- **Contributo del regime**: la rotazione HMM rende **8,85%** netto annuo contro **9,02%** della rotazione incondizionata, una differenza di −0,19% con t = −0,30.
- **Regole**: anche la media mobile a 10 mesi (−1,92%, t = −1,72) e il trend a 24 mesi (+0,89%, t = 0,72) non migliorano sulla rotazione incondizionata.
- **Confronto con il benchmark**: nessuna rotazione batte l'S&P 500 TR (11,11%).
- **Diagnosi**: l'HMM univariato identifica **regimi di volatilità, non di ciclo**. Dopo un mese "turbolento" il mercato è stato più volatile (24,5% contro 14,3%) ma ha reso di più (20,2% contro 11,1%), per i rimbalzi del 2009 e del 2020.

**Stato.** Esito riportato così com'è.

---

## Studio 2 — Ciclo economico e politica monetaria (`RotazioneCiclo.R`)

### D6. HMM fuori dalla rotazione settoriale — 2026-11-09

**Decisione.**
- Non si usa più l'HMM univariato per la rotazione settoriale.
- Il regime viene ridefinito in termini economici (ciclo e politica monetaria).
- L'HMM è rinviato a un progetto separato sulla gestione del rischio delle strategie momentum.

**Motivazione.** Il motivo è economico, non il risultato nullo. L'HMM sul rendimento di mercato misura la volatilità, mentre la rotazione settoriale scommette sul rendimento relativo dei settori, che dipende dal ciclo economico. L'uso naturale di un regime di volatilità è il controllo del rischio, che per il momentum è rilevante per i crolli (Daniel & Moskowitz 2016).

**Stato.** Decisione dell'autore. Lo studio 1 resta e si riporta.

### D7. Quali regimi — 2026-11-10

**Decisione.**
- **Regime A, principale**: fasi del ciclo economico.
- **Regime C, confronto**: politica monetaria.

**Alternative considerate e scartate.**
- **B, curva dei tassi × spread di credito**: dati puliti e mai rivisti, ma anticipa il ciclo più che misurarlo.
- **D, recessione in tempo reale** (regola di Sahm, CFNAI-MA3 < −0,7): binario e raro, circa 20 mesi dal 2005, quindi con lo stesso problema di scarsità dell'HMM.

**Criteri guida per un buon regime.**
1. Significato economico.
2. Disponibilità in tempo reale.
3. Regimi non troppo rari.
4. Storia lunga.
5. Poche soglie, fissate ex ante.

**Stato.** Decisione dell'autore.

### D8. Fonte dei dati del ciclo — 2026-11-10

**Decisione.** Real-Time Data Set for Macroeconomists della Philadelphia Fed (Croushore & Stark 2001): tutte le versioni mensili di produzione industriale (`IPT`) e occupazione non agricola (`EMPLOY`).

**Motivazione.** Gli indicatori pronti non hanno versioni storiche sufficienti per il periodo 2003-2026:
- su ALFRED il **CFNAI** ha versioni solo da maggio 2011;
- su ALFRED il **CLI dell'OCSE** ha versioni solo da luglio 2018 e si ferma a novembre 2025;
- la Chicago Fed dichiara di pubblicare la storia "real-time" del CFNAI, ma i file non sono risultati scaricabili.

**Nota tecnica.** FRED e ALFRED non rispondono alle richieste da script (timeout). Non si è cercato di aggirare il blocco.

**Stato.** Scelta dell'autore.

### D9. Costruzione dell'indicatore del ciclo — 2026-11-10

**Decisione (specifica iniziale).** A ogni formazione t si usa **la versione dei dati pubblicata nel mese t**, quindi l'ultimo dato è di norma il mese t−1. Su quella sola versione:
1. si calcola la crescita a 12 mesi di ciascuna serie;
2. la si standardizza rispetto alla media e alla deviazione standard dal 1948;
3. si fa la media delle due serie.

Le 4 fasi vengono da livello (sopra o sotto zero) × direzione (variazione su 3 mesi): ripresa, espansione, rallentamento, contrazione, la logica delle fasi del CLI dell'OCSE.

**Alternative.**
- Crescita × inflazione (lo "investment clock").
- Più serie: utilizzo della capacità, housing starts.
- Solo occupazione.

**Evidenza del tempo reale.** In 279 formazioni su 281 l'ultimo dato è di 1 mese prima; nelle altre 2 di 2 e 3 mesi. Mai dello stesso mese.

**Stato.** Specifica iniziale dell'autore, poi corretta (D12-D14).

### D10. Regime di politica monetaria — 2026-11-10

**Decisione.** Il regime è espansivo dopo un taglio del tasso obiettivo dei fed funds e restrittivo dopo un rialzo, fino alla mossa di segno opposto (Conover, Jensen, Johnson & Mercer 2008, che usano il tasso di sconto). A t conta l'ultima decisione annunciata entro la fine del mese t. Con i tassi a zero il regime resta espansivo.

**Fonte.** Le 110 decisioni dal 1990 della Federal Reserve Board, pagine *Open Market Operations*. Sono note il giorno stesso e mai riviste.

**Alternativa.** La direzione del rendimento del Treasury a 2 anni, che si muove anche con i tassi a zero.

**Evidenza.** La sequenza delle decisioni coincide con la storia nota: regime espansivo nel 59% dei mesi dal 2005, con 5 cambi.

**Stato.** Scelta dell'autore.

### D11. Criteri di accettazione delle fasi — 2026-11-10

**Decisione.** Il regime del ciclo è accettabile se, sul campione del backtest:
1. ogni fase copre **almeno il 10% dei mesi**;
2. ogni fase dura in media **almeno 5 mesi**, la durata minima di Bry & Boschan (1971);
3. le recessioni NBER cadono in larga parte nella fase più debole.

I criteri si valutano **solo sulle fasi, mai sui rendimenti**. Le date NBER, annunciate con mesi di ritardo, servono solo a questa verifica ex post e non entrano in alcuna decisione.

**Stato.** Fissati prima delle correzioni D12-D14.

### D12. Prima correzione: trend con media mobile e indicatore smussato — 2026-11-10

**Problema.** Con la specifica D9 il 47% dei mesi risultava in contrazione, con 81 cambi di fase in 257 mesi.
- Lo squilibrio dipende dal calo della crescita di lungo periodo dopo il 2000: rispetto alla media dal 1948 l'indicatore restava quasi sempre "sotto".
- L'instabilità dipende dal rumore mensile.

**Decisione.**
- Livello misurato rispetto alla **media mobile di 10 anni** della crescita, calcolata dentro ciascuna versione dei dati.
- Indicatore **smussato su 3 mesi** prima di misurarne livello e direzione, come il CFNAI-MA3.

**Alternative.**
- Crescita zero come soglia: renderebbe i regimi rari.
- Scarto dal trend con un filtro di Hodrick-Prescott in tempo reale: scartato perché in tempo reale è inaffidabile nell'ultimo tratto (Orphanides & van Norden 2002; Hamilton 2018).

**Effetto sulle fasi.** I cambi scendono a 48 e la contrazione al 18%. Due criteri restano mancati: la ripresa copre solo l'8,2% dei mesi e l'espansione dura in media 4,7 mesi.

**Stato.** Approvata dall'autore. Nessun rendimento della rotazione era stato calcolato.

### D13. Seconda correzione: conferma di 5 mesi — 2026-11-10

**Decisione.** Una fase nuova si adotta solo dopo essere stata segnalata per 5 mesi consecutivi, in parallelo con le regole di censura di Bry & Boschan. La regola usa solo le fasi già note a ciascuna data.

**Effetto sulle fasi.**
- 15 cambi dal 2005.
- Tutte le recessioni NBER classificate in contrazione.
- Durate medie da 10,5 a 20 mesi.
- La ripresa resta all'**8,2%** dei mesi (21), sotto la soglia del 10%.

**Costo.** Ritardo: per esempio la contrazione del 2020 resta segnalata per tutto l'anno.

**Stato.** Secondo passo previsto in caso di criteri mancati, concordato prima di applicarlo.

### D14. Terza correzione: 2 fasi — 2026-11-10

**Decisione.**
- **Specificazione principale**: 2 fasi, livello dell'indicatore sopra o sotto il trend, con media mobile, smussamento e conferma di 5 mesi.
- Le 4 fasi restano come **variante di robustezza**.

**Motivazione.** È il terzo passo concordato in caso di criteri ancora mancati. L'alternativa di tenere le 4 fasi dichiarando un'eccezione è stata scartata per rispettare il protocollo.

**Effetto sulle fasi.** Sotto il trend il 22,2% dei mesi, durate medie di 28,5 e 66,7 mesi, 4 cambi dal 2005, tutti i 20 mesi di recessione NBER sotto il trend. **Tutti i criteri sono soddisfatti.**

**Alternativa considerata e scartata.** Stimare le fasi con tecniche di machine learning: statistical jump model, oppure un classificatore addestrato sulle date NBER. Le ragioni:
- non cambia la scarsità di episodi;
- aumenta il rischio di look-ahead nella scelta degli iperparametri e nell'abbinamento dei cluster;
- dopo l'HMM sarebbe sembrata una ricerca del metodo "che funziona".

**Stato.** Decisione dell'autore, presa prima di qualunque rendimento della rotazione.

### D15. Esito dello studio 2

- **Rotazione sul ciclo**: **8,20%** netto annuo contro **9,02%** della rotazione incondizionata, una differenza di −0,93% con t = −0,91.
- **Rotazione sulla politica monetaria**: 8,99%, differenza −0,15% con t = −0,10.
- **Confronto con il benchmark**: nessuna rotazione lo batte.
- **Robustezza**: in nessuna delle 9 varianti il regime migliora in modo significativo sulla rotazione incondizionata. La migliore, a 4 fasi, guadagna +0,89% con t = 0,68.
- **Diagnosi**: il regime del ciclo distingue bene le condizioni di mercato. Dopo un mese sotto il trend il mercato rende 6,7% annuo con volatilità del 22,1%, contro 13,4% e 12,9% dopo un mese sopra il trend. Ma **i settori anticipano il ciclo**: sotto il trend hanno reso di più i ciclici (Materials, Consumer Cyclicals, Technology), non i difensivi. Con 3 soli episodi sotto il trend le medie per fase si basano su pochissimi casi.

**Proposta non adottata.** Stimare le medie per fase su una storia più lunga, con i portafogli industriali di French dal 1926. Sarebbe un nuovo studio, non una correzione di questo.

**Stato.** Esito riportato così com'è.

---

## Studio 3 — Metodo di costruzione (`CostruzioneCiclo.R`)

### D16. Specifica — 2026-11-10

**Decisione.** Il segnale resta **fisso**: i membri dei 3 settori scelti dalla rotazione sul ciclo (D14). Cambia solo il modo di pesarli, con 6 metodi:
1. settori equipesati con pesi di capitalizzazione al loro interno (il riferimento dello studio 2);
2. equal-weighted;
3. cap-weighted;
4. inverse volatility;
5. equal risk contribution (Maillard, Roncalli & Teiletche 2010);
6. minima varianza globale, long-only.

**Specificazione principale senza tetto ai pesi**; tetto del 5% per titolo come variante.

**Alternative.**
- Applicare i metodi anche alla rotazione incondizionata, o a tutto l'indice.
- Tetto del 5% come specificazione principale.

**Stato.** Confermata dall'autore, prima dei risultati.

### D17. Modello di rischio e insieme comune dei titoli — 2026-11-10

**Decisione.**
- **Covarianza**: shrinkage di Ledoit-Wolf (2004), su un massimo di 60 mesi fino alla formazione e un minimo di 24.
- **Insieme comune**: tutti i metodi pesano lo **stesso insieme di titoli**, cioè i candidati con storia completa nella finestra, così che la covarianza sia stimata sugli stessi mesi per tutti.

**Motivazione.** Ogni differenza fra i metodi deve dipendere dalla sola regola dei pesi.

**Effetto misurato.**
- Esclusi in mediana 5 titoli su 125 (massimo 11), pari all'1,7% della capitalizzazione (massimo 13,1%).
- Sul metodo di riferimento la restrizione costa 0,3 punti annui: 8,20% → 7,90%.
- Intensità mediana dello shrinkage: 0,18.

**Stato.** Applicata.

### D18. Inverse volatility con volatilità campionaria — 2026-11-10

**Decisione.** I pesi inverse volatility usano la deviazione standard campionaria della finestra, non la diagonale della matrice di Ledoit-Wolf.

**Motivazione.** Lo shrinkage verso un bersaglio scalare comprime le varianze verso la media e appiattirebbe i pesi. Le varianze singole sono stimate bene anche senza shrinkage.

**Stato.** Applicata.

### D19. Test della volatilità — 2026-11-10

**Decisione.** La volatilità di ogni metodo si confronta con quella dell'equal-weighted con il test di Pitman-Morgan per serie appaiate. I rendimenti si confrontano con differenze medie e errori standard Newey-West.

**Proposta non adottata (decisione dell'autore).** Il test sulle differenze di Sharpe (Ledoit & Wolf 2008) e il costo di transazione di pareggio per metodo.

**Stato.** Applicata.

### D20. Esito dello studio 3

**Rendimento: nessuna differenza significativa contro l'equal-weighted.**
- Cap-weighted: +2,16% annuo, t = 1,20.
- Tutti gli altri metodi entro un punto.
- Nessun metodo batte il benchmark al netto dei costi; il cap-weighted ci arriva vicino (−0,46%).

**Rischio: tutte le differenze sono significative** (Pitman-Morgan, p < 0,01).

| Metodo | Volatilità | Max drawdown | Beta |
|---|---|---|---|
| Equal-weighted | 19,0% | −57,5% | 1,12 |
| Minima varianza | 13,9% | −47,1% | 0,70 |

**Costi della minima varianza.**
- Concentrazione: 28 titoli detenuti, 16 effettivi.
- Turnover: 25% al mese.
- Volatilità ex ante sottostimata: 8,9% promessa contro 13,9% realizzata, l'errore di stima tipico dell'ottimizzazione.

**Tetto del 5%.** Non cambia le conclusioni: minima varianza 7,50% con volatilità del 14,1%, cap-weighted 9,95%.

**Stato.** Esito riportato così com'è.

---

## Organizzazione dei file

### D21. Cartella separata — 2026-11-09

**Decisione.** Tutti i file degli studi stanno in `studi/`.

**Verifica.** Dopo lo spostamento la tabella dei risultati dello studio 1 è risultata identica bit per bit.

**Superata da D22** per quanto riguarda l'origine di universo e dataset.

### D22. Universo e dataset sui moduli generici — 2026-11-11

**Decisione.** Universo e dataset dei costituenti non vengono più dallo script di un lavoro precedente, che non fa parte del progetto pubblico, ma da `R/00_universo.R`, costruito sui moduli generici `R/01_dati_pit.R`, `R/02_segnali_backtest.R` e `R/04_valutazione.R`. I dati congelati stanno in `dati/`, esclusa dal repository.

**Differenza di metodo.** Il modulo 02 applica la regola sui delisting a tutte le uscite definitive, mentre la prima pipeline la applicava solo a quelle eleggibili nel backtest. Fra le 306 righe interessate, solo due riguardano membri dell'indice (Southtrust e FleetBoston, 2004), che D3 trattava già con rendimento nullo.

**Verifica.**
- Pannello, crosswalk, benchmark e fattori identici alla prima pipeline, riga per riga.
- Le 15 tabelle dei tre studi identiche bit per bit dopo la nuova esecuzione.

**Stato.** Applicata.

---

## Progetti rinviati

- **HMM e gestione del rischio del momentum** (D6): usare il regime di volatilità per ridurre l'esposizione momentum nei regimi turbolenti.
- **Bibliografia.** Le 14 voci del paper e Stangl, Jacobsen & Visaltanachoti (2009) sono state verificate sui record bibliografici nel novembre 2026. Restano da verificare, se si citano, quelle discusse solo durante le scelte e non usate nel paper: Harding & Pagan (2002), Chauvet & Piger (2008), Ledoit & Wolf (2008).
