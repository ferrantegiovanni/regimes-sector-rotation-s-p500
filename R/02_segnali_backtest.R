# =============================================================================
#  MODULO 2 — SEGNALI, TIMELINE, DELISTING, BACKTEST
#  Interfaccia generica: un SEGNALE e' una funzione f(pannello) che
#  restituisce sec_id, periodo, segnale, usando solo dati fino a fine periodo.
#  Una STRATEGIA e' la coppia (selezione, pesatura) applicata al segnale.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate) })

# =============================================================================
#  1. SEGNALI
# =============================================================================
#  Rendimento composto fra t-k_a e t-k_da, agganciato al CALENDARIO: se un
#  mese della finestra manca, il segnale e' NA (mai calcolato scavalcando).
#  12-1 = (k_da = 2, k_a = 12); 6-1 = (2, 6); 12-0 = (1, 12).
segnale_momentum <- function(k_da = 2, k_a = 12) {
  function(df) {
    base <- df |> select(sec_id, periodo, r) |> distinct(sec_id, periodo, .keep_all = TRUE)
    out <- base |> select(sec_id, periodo)
    for (k in k_da:k_a)
      out <- out |> left_join(base |> transmute(sec_id, pt = periodo %m+% months(k),
                                                 !!paste0("r_m", k) := r),
                              by = c("sec_id", "periodo" = "pt"))
    M <- as.matrix(out[, paste0("r_m", k_da:k_a), drop = FALSE])
    out$segnale <- ifelse(rowSums(!is.na(M)) == ncol(M), apply(1 + M, 1, prod) - 1, NA_real_)
    out[, c("sec_id", "periodo", "segnale")]
  }
}

#  Altri esempi di segnali di soli prezzi, stessa interfaccia.
segnale_reversal <- function() {          # breve termine: -r(t)
  function(df) df |> transmute(sec_id, periodo, segnale = -r)
}
segnale_bassa_volatilita <- function(finestra = 36, min_oss = 24) {
  function(df) {
    df |> arrange(sec_id, periodo) |> group_by(sec_id) |>
      mutate(segnale = sapply(seq_along(r), function(i) {
        # finestra di calendario: solo mesi in (t - finestra, t]
        idx <- which(periodo > periodo[i] %m-% months(finestra) & periodo <= periodo[i])
        x <- r[idx]; if (sum(!is.na(x)) < min_oss) NA_real_ else -sd(x, na.rm = TRUE)
      })) |> ungroup() |> select(sec_id, periodo, segnale)
  }
}

# =============================================================================
#  2. TIMELINE
# =============================================================================
#  informazione fino a FINE MESE t -> formazione a fine t -> rendimento in t+1
#  r_next e' unito su calendario (mai lead()); l'eleggibilita' e' valutata
#  alla data di formazione, non al download.
prepara_timeline <- function(pannello, f_segnale, data_backtest, data_fine) {
  iv_key <- attr(pannello, "iv_key")
  bt <- pannello |> left_join(f_segnale(pannello), by = c("sec_id", "periodo")) |>
    mutate(data_formazione = ceiling_date(periodo, "month") - days(1),
           ritardo_giorni  = as.integer(data_formazione - ultima_oss))
  avanti <- pannello |> transmute(sec_id, pp = periodo %m-% months(1), r_next = r)
  bt <- bt |> left_join(avanti, by = c("sec_id", "periodo" = "pp"))
  bt <- marca_membership(bt, iv_key, data_fine, col_data = "data_formazione") |>
    rename(eleggibile = dentro) |>
    mutate(nel_backtest = data_formazione >= as.Date(data_backtest))
  cat("Righe eleggibili nel backtest:", sum(bt$eleggibile & bt$nel_backtest), "\n")
  attr(bt, "iv_key") <- iv_key
  bt
}

#  Ricalcola il segnale su un pannello TRONCATO a fine t: il valore a t non
#  deve cambiare. Vale per qualunque f_segnale.
test_look_ahead <- function(pannello, f_segnale, n_prove = 20, seme = 1, toll = 1e-10) {
  validi <- f_segnale(pannello) |> filter(!is.na(segnale))
  if (!nrow(validi)) { cat("Nessun segnale da testare.\n"); return(invisible(NA)) }
  set.seed(seme); prove <- validi[sample(nrow(validi), min(n_prove, nrow(validi))), ]
  disc <- 0L
  for (i in seq_len(nrow(prove))) {
    r <- f_segnale(pannello |> filter(periodo <= prove$periodo[i])) |>
      filter(sec_id == prove$sec_id[i], periodo == prove$periodo[i])
    if (!nrow(r) || is.na(r$segnale[1]) || abs(r$segnale[1] - prove$segnale[i]) > toll) disc <- disc + 1L
  }
  cat("Test look-ahead:", nrow(prove) - disc, "/", nrow(prove), "coincidono.",
      if (disc == 0) "Nessun uso di informazione futura.\n" else "!! IL SEGNALE GUARDA IL FUTURO.\n")
  invisible(disc)
}

# =============================================================================
#  3. RENDIMENTI MANCANTI IN t+1 E DELISTING
# =============================================================================
#  Classifica i rendimenti successivi mancanti in: censura di fine campione /
#  buco interno / uscita definitiva.
#  solo_eleggibili = TRUE  -> la diagnostica (quali uscite toccano l'universo);
#  solo_eleggibili = FALSE -> tutte le righe, perche' con holding superiore a
#  un mese si possono detenere titoli gia' usciti dall'indice.
classifica_mancanti <- function(bt, solo_eleggibili = TRUE, stampa = TRUE) {
  fine <- max(bt$periodo)
  ultimo <- bt |> filter(!is.na(r)) |> group_by(sec_id) |>
    summarise(ultimo_con_r = max(periodo), .groups = "drop")
  casi <- bt |> filter(is.na(r_next), if (solo_eleggibili) eleggibile & nel_backtest else TRUE) |>
    left_join(ultimo, by = "sec_id") |>
    mutate(tipo = case_when(periodo >= fine %m-% months(1) ~ "censura fine campione",
                            !is.na(ultimo_con_r) & ultimo_con_r > periodo ~ "buco interno",
                            TRUE ~ "uscita definitiva"))
  if (stampa) print(as.data.frame(casi |> count(tipo, name = "casi")), row.names = FALSE)
  casi
}

#  tab: ricerca documentale, una riga per uscita, colonne
#  sec_id, periodo, ultimo_prezzo, valore_terminale, contante_per_azione,
#  rapporto_cambio, ric_acquirente. Il prezzo dell'acquirente si legge dal
#  pannello; se manca, l'intero valore NON e' calcolabile (mai la sola cassa).
rendimenti_terminali <- function(tab, pannello) {
  vuoto <- data.frame(sec_id = character(), periodo = as.Date(character()), r_term = numeric())
  if (is.null(tab) || !nrow(tab)) return(vuoto)
  px_acq <- function(ric, per) {
    if (is.na(ric) || !nzchar(ric)) return(NA_real_)
    p <- pannello$prezzo[pannello$RIC == ric & pannello$periodo == as.Date(per)]
    if (length(p) && !is.na(p[1])) p[1] else NA_real_
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  tab |> mutate(periodo = as.Date(periodo),
                across(c(ultimo_prezzo, valore_terminale, contante_per_azione, rapporto_cambio), num),
                ric_acquirente = trimws(as.character(ric_acquirente))) |>
    rowwise() |>
    mutate(azioni = if (!is.na(rapporto_cambio)) rapporto_cambio * px_acq(ric_acquirente, periodo) else NA_real_,
           incompleto = !is.na(rapporto_cambio) && is.na(azioni),
           valore = if (!is.na(valore_terminale)) valore_terminale else if (incompleto) NA_real_
                    else if (!is.na(contante_per_azione) || !is.na(azioni))
                      sum(c(contante_per_azione, azioni), na.rm = TRUE) else NA_real_,
           r_term = if (is.na(valore) || is.na(ultimo_prezzo) || ultimo_prezzo <= 0) NA_real_
                    else valore / ultimo_prezzo - 1) |>
    ungroup() |> filter(!is.na(r_term)) |> select(sec_id, periodo, r_term)
}

#  regola: "esclusione" | "conservativa" (r_cons all'ultimo prezzo) |
#          "effettiva" (documentale dove disponibile, conservativa altrove)
#  Si applica a TUTTE le uscite definitive, eleggibili o no.
applica_delisting <- function(bt, regola = "effettiva", effettivi = NULL, r_cons = 0) {
  usc <- classifica_mancanti(bt, solo_eleggibili = FALSE, stampa = FALSE) |>
    filter(tipo == "uscita definitiva") |> distinct(sec_id, periodo) |> mutate(uscita_definitiva = TRUE)
  bt <- bt |> select(-any_of("uscita_definitiva")) |>
    left_join(usc, by = c("sec_id", "periodo")) |>
    mutate(uscita_definitiva = coalesce(uscita_definitiva, FALSE))
  if (regola == "esclusione") return(bt)
  d <- bt |> mutate(r_term = NA_real_)
  if (regola == "effettiva" && !is.null(effettivi) && nrow(effettivi))
    d <- d |> left_join(effettivi |> rename(r_e = r_term), by = c("sec_id", "periodo")) |>
      mutate(r_term = coalesce(r_term, r_e)) |> select(-r_e)
  d |> mutate(r_term = ifelse(uscita_definitiva & is.na(r_term), r_cons, r_term),
              r_next = ifelse(is.na(r_next) & !is.na(r_term), r_term, r_next)) |> select(-r_term)
}

# =============================================================================
#  4. FILTRI D'UNIVERSO, SELEZIONE, PESATURA, COSTRUTTORI (blocchi A e D)
# =============================================================================
#  Filtri aggiuntivi alla data di formazione: ricevono i candidati e ne
#  restituiscono un sottoinsieme, usando SOLO colonne note a t.
filtro_cap_minima <- function(quantile_min = 0.10) function(cand) {
  q <- stats::quantile(cand$mktcap, quantile_min, na.rm = TRUE)
  cand[!is.na(cand$mktcap) & cand$mktcap >= q, , drop = FALSE]
}
filtro_prezzo_minimo <- function(p_min = 5) function(cand)
  cand[!is.na(cand$prezzo) & cand$prezzo >= p_min, , drop = FALSE]
componi_filtri <- function(...) { fs <- list(...); function(cand) Reduce(function(d, f) f(d), fs, cand) }

#  Selettori: (cand, ctx) -> sottoinsieme. ctx$prec = titoli gia' detenuti,
#  ctx$buffer = banda aggiuntiva (quota dell'universo) entro cui un titolo
#  detenuto non viene venduto. buffer = 0 -> selezione pura.
ordina_segnale <- function(cand, alto) cand[order(if (alto) -cand$segnale else cand$segnale), , drop = FALSE]
seleziona_con_buffer <- function(cand, n, n_uscita, prec, alto) {
  o <- ordina_segnale(cand, alto); n <- min(n, nrow(o)); n_uscita <- min(n_uscita, nrow(o))
  if (!length(prec) || n_uscita <= n) return(o[seq_len(n), , drop = FALSE])
  tenuti <- o[seq_len(n_uscita), , drop = FALSE]; tenuti <- tenuti[tenuti$sec_id %in% prec, , drop = FALSE]
  if (nrow(tenuti) >= n) return(tenuti[seq_len(n), , drop = FALSE])
  nuovi <- o[!o$sec_id %in% tenuti$sec_id, , drop = FALSE]
  ordina_segnale(rbind(tenuti, nuovi[seq_len(n - nrow(tenuti)), , drop = FALSE]), alto)
}
sel_quantile <- function(quota = 0.20, alto = TRUE) function(cand, ctx = list()) {
  b <- if (is.null(ctx$buffer)) 0 else ctx$buffer
  seleziona_con_buffer(cand, max(1, ceiling(nrow(cand) * quota)),
                       ceiling(nrow(cand) * (quota + b)), ctx$prec, alto)
}
sel_top_n <- function(n = 50, alto = TRUE) function(cand, ctx = list()) {
  b <- if (is.null(ctx$buffer)) 0 else ctx$buffer
  seleziona_con_buffer(cand, n, n + ceiling(nrow(cand) * b), ctx$prec, alto)
}
sel_tutti <- function() function(cand, ctx = list()) cand

#  Pesature: (sel, ctx) -> pesi che sommano a 1.
pesi_ew <- function() function(sel, ctx = list()) rep(1 / nrow(sel), nrow(sel))
pesi_vw <- function() function(sel, ctx = list()) {
  m <- ifelse(is.na(sel$mktcap), 0, sel$mktcap)        # cap mancante: escluso e rinormalizzato
  if (sum(m) > 0) m / sum(m) else rep(1 / nrow(sel), nrow(sel))
}
pesi_rank <- function() function(sel, ctx = list()) { k <- rank(sel$segnale); k / sum(k) }
pesi_vw_cap <- function(tetto = 0.05, iter = 50) function(sel, ctx = list()) {
  w <- pesi_vw()(sel)
  for (i in seq_len(iter)) {                              # tetto con ridistribuzione
    sopra <- w > tetto; if (!any(sopra)) break
    ecc <- sum(w[sopra] - tetto); w[sopra] <- tetto
    w[!sopra] <- w[!sopra] + ecc * w[!sopra] / sum(w[!sopra])
  }
  w
}

#  COSTRUTTORE = la regola di costruzione di una strategia: (cand, ctx) ->
#  vettore nominato di pesi obiettivo (per sec_id), oppure NULL se a quella
#  data non e' costruibile. Puo' allegare attr(w, "diag"): un data frame di
#  una riga con diagnostiche che il motore riporta nell'output.
costruttore_selezione <- function(selettore, pesatore) function(cand, ctx = list()) {
  sel <- selettore(cand, ctx); if (!nrow(sel)) return(NULL)
  w <- pesatore(sel, ctx); names(w) <- sel$sec_id; w
}

# =============================================================================
#  5. IMPLEMENTAZIONE (blocco E) E MOTORE DI BACKTEST
# =============================================================================
#  frequenza : mesi fra due ribilanciamenti (1 mensile, 3 trimestrale, ...)
#  holding   : mesi di detenzione di ciascun portafoglio formato; multiplo
#              della frequenza. holding > frequenza = portafogli sovrapposti
#              (Jegadeesh-Titman): holding/frequenza maniche, ne viene
#              rinnovata una a ogni ribilanciamento.
#  ritardo   : mesi fra la data dell'informazione (formazione) e l'esecuzione
#  buffer    : banda di non-vendita per i selettori (quota dell'universo)
#  cassa     : liquidita' da titoli usciti fra due ribilanciamenti:
#              "reinvesti" pro quota nella manica, "cassa" a rendimento nullo
parametri_impl <- function(frequenza = 1, holding = frequenza, ritardo = 0, buffer = 0,
                           cassa = c("reinvesti", "cassa")) {
  cassa <- match.arg(cassa)
  stopifnot(frequenza >= 1, holding >= frequenza, holding %% frequenza == 0, ritardo >= 0, buffer >= 0)
  list(frequenza = as.integer(frequenza), holding = as.integer(holding),
       ritardo = as.integer(ritardo), buffer = buffer, cassa = cassa)
}

aggrega_maniche <- function(m) {
  v <- unlist(unname(m)); if (!length(v)) return(numeric(0))
  s <- tapply(v, names(v), sum); setNames(as.numeric(s), names(s))
}
distanza_l1 <- function(a, b) {
  n <- union(names(a), names(b)); x <- setNames(rep(0, length(n)), n); y <- x
  x[names(a)] <- a; y[names(b)] <- b; sum(abs(x - y))
}

#  Motore unico per QUALSIASI strategia. Posizioni tenute in valore, per
#  manica. Ogni fine mese d, nell'ordine:
#   (a) i titoli senza riga a d sono usciti: il loro valore diventa cassa;
#   (b) nei mesi senza ribilanciamento la cassa si reinveste (o resta cassa);
#   (c) se d e' data di esecuzione: universo e pesi dalla data di formazione
#       f = d - ritardo; i titoli non piu' quotati a d non sono eseguibili e
#       i pesi si rinormalizzano; si rinnova una manica; il movimentato si
#       misura sull'intero portafoglio (posizioni derivate prima, nuove dopo);
#   (d) si applicano i rendimenti del mese d+1 (r_next della riga d).
#  Il costo (movimentato x costo per lato) si sottrae al rendimento del mese
#  successivo. Un titolo detenuto senza rendimento che riprende piu' tardi
#  (buco interno) ferma l'esecuzione: va esaminato.
esegui_strategia <- function(bt, costruttore, impl = parametri_impl(), costo_unitario = 0.0010,
                             filtro = NULL, escludi_senza_rend = FALSE) {
  fine   <- suppressWarnings(max(bt$periodo[!is.na(bt$r_next)]))
  inizio <- min(bt$periodo[bt$nel_backtest])
  per    <- split(bt, bt$periodo)
  ok_r   <- !is.na(bt$r)
  ultimo_r <- tapply(as.numeric(bt$periodo[ok_r]), bt$sec_id[ok_r], max)
  formazioni <- seq(inizio, fine, by = paste(impl$frequenza, "months"))
  esecuzioni <- formazioni %m+% months(impl$ritardo); esecuzioni <- esecuzioni[esecuzioni <= fine]
  if (!length(esecuzioni)) return(data.frame())
  n_man <- impl$holding %/% impl$frequenza
  maniche <- replicate(n_man, numeric(0), simplify = FALSE)
  mesi <- sort(as.Date(names(per))); mesi <- mesi[mesi >= esecuzioni[1] & mesi <= fine]
  cassa_di <- function(h) if (".CASSA" %in% names(h)) h[[".CASSA"]] else 0
  k <- 0L; avviato <- FALSE; righe <- vector("list", length(mesi))

  for (ii in seq_along(mesi)) {
    d <- mesi[ii]; td <- per[[as.character(d)]]; presenti <- td$sec_id
    # (a) uscite -> cassa
    for (j in seq_len(n_man)) {
      h <- maniche[[j]]; out <- setdiff(names(h), c(".CASSA", presenti))
      if (length(out)) { c0 <- cassa_di(h) + sum(h[out]); h <- h[!names(h) %in% out]; h[".CASSA"] <- c0 }
      maniche[[j]] <- h
    }
    esegui <- d %in% esecuzioni
    # (b) reinvestimento nei mesi senza ribilanciamento
    if (!esegui && impl$cassa == "reinvesti") for (j in seq_len(n_man)) {
      h <- maniche[[j]]; c0 <- cassa_di(h); tit <- setdiff(names(h), ".CASSA")
      if (c0 > 0 && length(tit) && sum(h[tit]) > 0) {
        h[tit] <- h[tit] + c0 * h[tit] / sum(h[tit]); maniche[[j]] <- h[tit]
      }
    }
    # (c) esecuzione
    sc <- 0; f <- as.Date(NA); diag <- NULL; n_univ <- NA_integer_; non_es <- NA_integer_; saltato <- FALSE
    if (esegui) {
      f <- d %m-% months(impl$ritardo); tf <- per[[as.character(f)]]
      cand <- if (is.null(tf)) tf else tf[tf$eleggibile & !is.na(tf$segnale), , drop = FALSE]
      if (!is.null(cand) && !is.null(filtro)) cand <- filtro(cand)
      if (!is.null(cand) && escludi_senza_rend) cand <- cand[!is.na(cand$r_next), , drop = FALSE]
      prec <- setdiff(names(aggrega_maniche(maniche)), ".CASSA")
      w <- if (!is.null(cand) && nrow(cand)) costruttore(cand, list(t = f, dati = bt, prec = prec, buffer = impl$buffer)) else NULL
      if (is.null(w) || !length(w)) saltato <- TRUE else {
        diag <- attr(w, "diag"); n_univ <- nrow(cand)
        es <- names(w) %in% presenti; non_es <- sum(!es); w <- w[es]; w <- w / sum(w)
        rn <- setNames(td$r_next, td$sec_id)[names(w)]
        if (any(is.na(rn)))
          stop("Rendimento del mese successivo mancante per ", sum(is.na(rn)),
               " titoli acquistati a ", format(d), ": caso da esaminare, non da assorbire.")
        prima <- aggrega_maniche(maniche)
        V <- if (avviato) sum(prima) else 1
        if (!avviato) { maniche <- replicate(n_man, w / n_man, simplify = FALSE); avviato <- TRUE }
        else { j <- (k %% n_man) + 1L; maniche[[j]] <- w * sum(maniche[[j]]) }
        sc <- distanza_l1(aggrega_maniche(maniche), prima) / V
        k <- k + 1L
      }
    }
    if (!avviato) next
    # (d) rendimenti del mese d+1
    rn_all <- setNames(td$r_next, td$sec_id); V0 <- sum(aggrega_maniche(maniche))
    for (j in seq_len(n_man)) {
      h <- maniche[[j]]; tit <- setdiff(names(h), ".CASSA"); if (!length(tit)) next
      r <- rn_all[tit]
      for (m in tit[is.na(r)]) {
        if (!is.na(ultimo_r[m]) && ultimo_r[m] > as.numeric(d))
          stop("Buco interno nella serie di ", m, " detenuto a ", format(d), ": caso da esaminare.")
        r[m] <- 0                                         # uscita senza rendimento terminale: ultimo prezzo
      }
      h[tit] <- h[tit] * (1 + r); maniche[[j]] <- h
    }
    tot <- aggrega_maniche(maniche); rl <- sum(tot) / V0 - 1
    riga <- data.frame(formazione = f, mese_reso = d %m+% months(1), ribilancio = esegui && !saltato,
                       saltato = saltato, n_universo = n_univ,
                       n_titoli = sum(tot[names(tot) != ".CASSA"] > 1e-6 * sum(tot)),
                       r_lordo = rl, costo = sc * costo_unitario, r_netto = rl - sc * costo_unitario,
                       turnover = sc / 2, non_eseguibili = non_es)
    righe[[ii]] <- if (is.null(diag)) riga else cbind(riga, diag)
  }
  bind_rows(righe)
}
