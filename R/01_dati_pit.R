# =============================================================================
#  MODULO 1 — DATI POINT-IN-TIME
#  Membership dell'indice, chiave di security, pannello canonico.
#  Indipendente dalla fonte: le funzioni lavorano su data frame con colonne
#  dichiarate. Gli adattatori per LSEG sono in fondo al file.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
})

# ---- Funzioni di servizio ----------------------------------------------------
#  write.csv(na = "") scrive i mancanti come stringa vuota: senza na.strings
#  read.csv non li rilegge come NA e i controlli sui mancanti non li vedono.
leggi <- function(f) read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))

#  Gli identificativi sono codici, non quantita': sempre testo.
come_testo <- function(df, colonne) {
  for (cc in intersect(colonne, names(df))) {
    x <- trimws(as.character(df[[cc]])); x[x %in% c("", "NA")] <- NA_character_
    df[[cc]] <- x
  }
  df
}

#  Una fase si esegue se manca il suo prodotto oppure se la si forza.
da_rigenerare <- function(file, forza = FALSE) forza || !file.exists(file)

# =============================================================================
#  1. RICOSTRUZIONE DELLA MEMBERSHIP POINT-IN-TIME
# =============================================================================
#  eventi   : data frame con status ("Joiner"/"Leaver"), issuer, code, date
#  correnti : data frame con code, name (costituenti alla data odierna)
#  baseline : data di popolamento iniziale del database della fonte
#  Ritorna una lista: iv (intervalli [date_in, date_out)), anomalie, conteggi.
ricostruisci_membership <- function(eventi, correnti, baseline, end_date) {
  baseline <- as.Date(baseline); end_date <- as.Date(end_date)

  lj <- eventi |>
    mutate(date = as.Date(date), code = trimws(code)) |>
    filter(!is.na(date), status %in% c("Joiner", "Leaver"))

  # Coppie Leaver+Joiner stesso codice / stesso giorno: artefatti di
  # corporate action. Si rimuovono min(nJoiner, nLeaver) eventi per tipo.
  lj <- lj |>
    group_by(code, date, status) |> mutate(rk = row_number()) |>
    group_by(code, date) |>
    mutate(n_pairs = min(sum(status == "Joiner"), sum(status == "Leaver"))) |>
    ungroup() |> filter(rk > n_pairs) |> select(-rk, -n_pairs) |>
    arrange(date, desc(status))          # a parita' di data, Leaver prima

  current_set <- trimws(correnti$code)
  cur_names   <- setNames(correnti$name, current_set)

  open_start <- list(); open_issuer <- list()
  intervals <- list(); anomalies <- list()
  aggiungi_an <- function(tipo, cd, is_, dt)
    anomalies[[length(anomalies) + 1]] <<-
      data.frame(tipo = tipo, code = cd, issuer = is_, date = dt)

  for (i in seq_len(nrow(lj))) {
    st <- lj$status[i]; cd <- lj$code[i]; is_ <- lj$issuer[i]; dt <- lj$date[i]
    if (st == "Joiner") {
      if (!is.null(open_start[[cd]])) aggiungi_an("joiner_while_member(ignorato)", cd, is_, dt)
      else { open_start[[cd]] <- dt; open_issuer[[cd]] <- is_ }
    } else {
      if (!is.null(open_start[[cd]])) {
        intervals[[length(intervals) + 1]] <- data.frame(
          code = cd, issuer = open_issuer[[cd]], date_in = open_start[[cd]], date_out = dt)
        open_start[[cd]] <- NULL; open_issuer[[cd]] <- NULL
      } else {
        aggiungi_an("leaver_senza_joiner(riparato_da_baseline)", cd, is_, dt)
        intervals[[length(intervals) + 1]] <- data.frame(
          code = cd, issuer = is_, date_in = baseline, date_out = dt)
      }
    }
  }
  for (cd in names(open_start))
    intervals[[length(intervals) + 1]] <- data.frame(
      code = cd, issuer = open_issuer[[cd]], date_in = open_start[[cd]], date_out = as.Date(NA))

  for (cd in setdiff(current_set, names(open_start))) {
    intervals[[length(intervals) + 1]] <- data.frame(
      code = cd, issuer = unname(cur_names[cd]), date_in = baseline, date_out = as.Date(NA))
    aggiungi_an("corrente_senza_eventi(riparato_da_baseline)", cd, unname(cur_names[cd]), baseline)
  }

  iv <- bind_rows(intervals) |>
    arrange(code, date_in) |> group_by(code) |>
    mutate(prev_out  = lag(date_out),
           new_block = (is.na(prev_out) & row_number() > 1) |
                       (!is.na(prev_out) & date_in > prev_out),
           block = cumsum(as.integer(row_number() == 1 | new_block))) |>
    group_by(code, block) |>
    summarise(issuer = first(issuer), date_in = min(date_in),
              date_out = if (any(is.na(date_out))) as.Date(NA) else max(date_out),
              .groups = "drop") |>
    select(-block) |>
    mutate(base_ric = trimws(sub("\\^.*$", "", code)),
           still_member = is.na(date_out)) |>
    arrange(date_in, code)

  # Validazione: i membri finali DEVONO coincidere con i correnti
  finali <- iv$code[iv$still_member]
  if (!setequal(finali, current_set))
    stop("Membri finali diversi dai costituenti correnti: ",
         length(setdiff(finali, current_set)), " in piu', ",
         length(setdiff(current_set, finali)), " in meno.")

  firsts <- seq(ceiling_date(baseline, "month"), ceiling_date(end_date, "month"), by = "month")
  fine_mesi <- sort(unique(c(baseline, firsts - days(1), end_date)))
  fine_mesi <- fine_mesi[fine_mesi >= baseline & fine_mesi <= end_date]
  conteggi <- data.frame(date = fine_mesi, n_constituents = vapply(fine_mesi, function(d)
    sum(iv$date_in <= d & (is.na(iv$date_out) | iv$date_out > d)), integer(1)))

  an <- if (length(anomalies)) bind_rows(anomalies) else
    data.frame(tipo = character(), code = character(), issuer = character(), date = as.Date(character()))
  cat("Membership:", nrow(iv), "intervalli | costituenti per mese min/max:",
      min(conteggi$n_constituents), "/", max(conteggi$n_constituents), "\n")
  list(iv = iv, anomalie = an, conteggi = conteggi, eventi_puliti = lj)
}

# =============================================================================
#  2. CHIAVE DI SECURITY E GUARDIA SUI GRUPPI
# =============================================================================
#  La chiave di unione non e' mai il RIC (riassegnabile) ne' l'ISIN (cambia
#  con il domicilio). Default: PermID di organizzazione + ShareClass.
costruisci_chiave <- function(cw, col_entita = "PermID", col_classe = "ShareClass",
                              classe_unica = "UNICA") {
  cw |> mutate(classe = ifelse(is.na(.data[[col_classe]]), classe_unica, .data[[col_classe]]),
               sec_id = ifelse(is.na(.data[[col_entita]]), NA_character_,
                               paste(.data[[col_entita]], classe, sep = "_")))
}

#  Chiavi associate a piu' codici: non sono errori di per se'.
gruppi_multipli <- function(cw, col_codice = "RIC") {
  cw |> filter(!is.na(sec_id)) |> group_by(sec_id) |>
    summarise(n_ric = n(), RIC_associati = paste(sort(.data[[col_codice]]), collapse = " + "),
              .groups = "drop") |> filter(n_ric > 1)
}

#  Due codici sono la stessa security? Criterio indipendente dalle etichette:
#    nessuna sovrapposizione          -> successione (stessa security)
#    sovrapposte, prezzi uguali       -> stessa security su piu' sedi
#    sovrapposte, prezzi diversi      -> security DISTINTE
#  px: data frame long con RIC, Data, p.
stessa_security <- function(px, a, b, soglia = 0.01) {
  ov <- inner_join(px |> filter(RIC == a) |> distinct(Data, .keep_all = TRUE) |> select(Data, pa = p),
                   px |> filter(RIC == b) |> distinct(Data, .keep_all = TRUE) |> select(Data, pb = p),
                   by = "Data")
  if (nrow(ov) == 0) return(TRUE)
  max(abs(ov$pa - ov$pb) / pmax(abs(ov$pa), abs(ov$pb)), na.rm = TRUE) < soglia
}

#  Componenti connesse; numerazione stabile per data di prima osservazione.
raggruppa_codici <- function(rics, px, soglia = 0.01) {
  g <- setNames(seq_along(rics), rics)
  for (i in seq_along(rics)) for (j in seq_along(rics)) {
    if (j <= i) next
    if (stessa_security(px, rics[i], rics[j], soglia)) g[g == g[[rics[j]]]] <- g[[rics[i]]]
  }
  prime <- px |> group_by(RIC) |> summarise(prima = min(Data), .groups = "drop")
  ordine <- sapply(unique(g), function(k) {
    r <- names(g)[g == k]; x <- prime$prima[prime$RIC %in% r]
    if (length(x)) min(x) else as.Date("2999-12-31")
  })
  rango <- rank(ordine, ties.method = "first")
  setNames(rango[match(g, unique(g))], names(g))
}

#  Disambigua i gruppi multipli; f_prezzi(rics) deve restituire RIC, Data, p.
#  L'assegnazione va CONGELATA nel crosswalk (non e' una regola per riga).
disambigua_crosswalk <- function(cw, f_prezzi, soglia = 0.01) {
  multi <- gruppi_multipli(cw)
  mappa <- list(); esiti <- list()
  for (i in seq_len(nrow(multi))) {
    sid <- multi$sec_id[i]; rics <- strsplit(multi$RIC_associati[i], " \\+ ")[[1]]
    px <- f_prezzi(rics)
    if (is.null(px) || nrow(px) == 0) {
      esiti[[i]] <- data.frame(sec_id = sid, esito = "estrazione fallita"); next
    }
    g <- raggruppa_codici(rics, px, soglia)
    esiti[[i]] <- data.frame(sec_id = sid, RIC_associati = multi$RIC_associati[i],
                             esito = if (length(unique(g)) > 1) "SECURITY DISTINTE" else "stessa security")
    if (length(unique(g)) > 1)
      mappa[[length(mappa) + 1]] <- data.frame(RIC = rics, sec_id_vecchio = sid,
                                              sec_id_nuovo = sprintf("%s_s%02d", sid, g[rics]))
  }
  mappa <- bind_rows(mappa)
  cw_f <- cw |> mutate(sec_id_base = sec_id, disambiguato = FALSE)
  if (nrow(mappa)) cw_f <- cw_f |>
    left_join(mappa |> select(RIC, sec_id_nuovo), by = "RIC") |>
    mutate(disambiguato = !is.na(sec_id_nuovo),
           sec_id = ifelse(disambiguato, sec_id_nuovo, sec_id)) |> select(-sec_id_nuovo)
  cat("Crosswalk:", nrow(cw_f), "codici |", n_distinct(cw_f$sec_id, na.rm = TRUE),
      "security | disambiguati:", sum(cw_f$disambiguato), "\n")
  list(crosswalk = cw_f, disambiguazioni = mappa, esiti = bind_rows(esiti))
}

# =============================================================================
#  3. ELEGGIBILITA' E PANNELLO CANONICO
# =============================================================================
#  Convenzione unica: [date_in, date_out). Eccezione: il tappo di fine
#  campione (intervallo aperto) e' incluso, altrimenti l'ultimo mese
#  risulterebbe fuori indice per tutti.
marca_membership <- function(df, iv_key, data_fine, col_data = "Data") {
  ii <- iv_key |> mutate(aperto = is.na(date_out),
                         date_out_eff = coalesce(date_out, as.Date(data_fine))) |>
    select(sec_id, date_in, date_out_eff, aperto)
  d_vec <- as.Date(df[[col_data]])
  marcate <- data.frame(sec_id = df$sec_id, .d = d_vec, stringsAsFactors = FALSE) |>
    distinct() |> left_join(ii, by = "sec_id", relationship = "many-to-many") |>
    group_by(sec_id, .d) |>
    summarise(dentro = any(.d >= date_in & ifelse(aperto, .d <= date_out_eff, .d < date_out_eff),
                           na.rm = TRUE), .groups = "drop")
  df$.d <- d_vec
  df <- df |> left_join(marcate, by = c("sec_id", ".d")) |> mutate(dentro = coalesce(dentro, FALSE))
  df$.d <- NULL
  df
}

#  mensili : RIC, Data, ret (decimale), prezzo, mktcap  (storia INTERA)
#  ponte   : RIC -> sec_id (+ colonne descrittive)
#  iv      : intervalli con colonna code (o gia' sec_id)
#  Regole: righe segnaposto rimosse; rendimenti mancanti RESTANO NA (mai 0);
#  vincolo una security - un mese - una riga, ultimo valore non mancante.
costruisci_pannello <- function(mensili, ponte, iv, data_fine) {
  vuote <- is.na(mensili$Data) | (is.na(mensili$ret) & is.na(mensili$prezzo))
  if (any(vuote)) cat("Righe segnaposto rimosse:", sum(vuote), "\n")
  m <- mensili[!vuote, ] |> mutate(Data = as.Date(Data))

  if (!"sec_id" %in% names(m))
    m <- m |> left_join(ponte |> select(RIC, sec_id), by = "RIC") |> filter(!is.na(sec_id))
  iv_key <- if ("sec_id" %in% names(iv)) iv else
    iv |> mutate(code_pulito = trimws(sub(" \\(expired\\)", "", code))) |>
      left_join(ponte |> select(RIC, sec_id), by = c("code_pulito" = "RIC")) |>
      filter(!is.na(sec_id))
  iv_key <- iv_key |> mutate(date_in = as.Date(date_in), date_out = as.Date(date_out))

  m <- marca_membership(m, iv_key, data_fine) |> rename(in_indice = dentro) |>
    mutate(periodo = floor_date(Data, "month"))

  ultimo_nm <- function(x) if (all(is.na(x))) x[1] else x[max(which(!is.na(x)))]
  doppi <- m |> count(sec_id, periodo) |> filter(n > 1)
  cat("Violazioni una-security-un-mese-una-riga prima del consolidamento:", nrow(doppi), "\n")
  base_cols <- function(d) d |> transmute(sec_id, periodo, RIC, ultima_oss = Data,
                                          r = ret, prezzo, mktcap, in_indice)
  pann <- if (nrow(doppi)) bind_rows(
    m |> anti_join(doppi, by = c("sec_id", "periodo")) |> base_cols(),
    m |> semi_join(doppi, by = c("sec_id", "periodo")) |> arrange(sec_id, periodo, Data) |>
      group_by(sec_id, periodo) |>
      summarise(RIC = last(RIC), ultima_oss = max(Data[!is.na(prezzo)], Data[1]),
                r = ultimo_nm(ret), prezzo = ultimo_nm(prezzo), mktcap = ultimo_nm(mktcap),
                in_indice = ultimo_nm(in_indice), .groups = "drop")) else base_cols(m)
  pann <- pann |> arrange(sec_id, periodo)
  stopifnot(!any(duplicated(pann[, c("sec_id", "periodo")])))
  amp <- pann |> filter(in_indice) |> count(periodo)
  cat("Pannello:", nrow(pann), "righe |", n_distinct(pann$sec_id), "security | membri/mese mediana",
      median(amp$n), "\n")
  attr(pann, "iv_key") <- iv_key
  pann
}

# =============================================================================
#  4. ADATTATORI LSEG (Refinitiv / RefinitivR)
# =============================================================================
#  Credenziale SEMPRE da variabile d'ambiente (~/.Renviron: LSEG_APP_KEY=...).
connetti_lseg <- function() {
  if (!requireNamespace("Refinitiv", quietly = TRUE)) stop("Pacchetto Refinitiv non installato.")
  key <- Sys.getenv("LSEG_APP_KEY")
  if (!nzchar(key)) stop("LSEG_APP_KEY non impostata (usethis::edit_r_environ()).")
  Refinitiv::RDConnect(application_id = key, PythonModule = "JSON")
}

#  Riconosce le colonne per contenuto, non per posizione.
rinomina_per_contenuto <- function(df, mappa) {
  df <- as.data.frame(df); ch <- gsub("[^a-z]", "", tolower(names(df)))
  for (nuovo in names(mappa)) {
    i <- which(grepl(mappa[[nuovo]], ch))[1]
    if (!is.na(i)) names(df)[i] <- nuovo
  }
  for (a in names(mappa)) if (!a %in% names(df)) df[[a]] <- NA
  df[, names(mappa), drop = FALSE]
}

#  Estrazione a lotti con ritentativi, salvataggio incrementale e ciclo di
#  riparazione PER CODICE: il lotto puo' fallire in parte e in silenzio.
#  f_lotto(codici) -> data frame con colonna RIC (o NULL).
#  completo(df)    -> vettore logico: righe considerate valide.
estrai_a_lotti <- function(codici, f_lotto, lotto = 25, tentativi = 3,
                           giri_riparo = 3, lotto_riparo = 5, completo = function(d) rep(TRUE, nrow(d)),
                           file_parziale = NULL) {
  prova <- function(x, k_max) {
    for (k in seq_len(k_max)) {
      r <- try(f_lotto(x), silent = TRUE)
      if (!inherits(r, "try-error") && !is.null(r) && nrow(r) > 0) return(r)
      Sys.sleep(2 * k)
    }
    NULL
  }
  lotti <- split(codici, ceiling(seq_along(codici) / lotto)); racc <- list()
  for (i in seq_along(lotti)) {
    o <- prova(lotti[[i]], tentativi); if (!is.null(o)) racc[[length(racc) + 1]] <- o
    cat("  lotto", i, "/", length(lotti), if (is.null(o)) "FALLITO" else paste(nrow(o), "righe"), "\n")
    if (!is.null(file_parziale) && (i %% 5 == 0 || i == length(lotti)))
      write.csv(bind_rows(racc), file_parziale, row.names = FALSE, na = "")
  }
  tutto <- bind_rows(racc)
  ok <- if (nrow(tutto)) unique(tutto$RIC[completo(tutto)]) else character(0)
  resta <- setdiff(codici, ok)
  for (giro in seq_len(giri_riparo)) {
    if (!length(resta)) break
    cat("Riparazione giro", giro, "- mancanti:", length(resta), "\n")
    nuovi <- bind_rows(lapply(split(resta, ceiling(seq_along(resta) / lotto_riparo)),
                              function(x) prova(x, 2)))
    if (!nrow(nuovi)) break
    nuovi <- nuovi[completo(nuovi), , drop = FALSE]
    if (!nrow(nuovi)) break
    tutto <- bind_rows(tutto |> filter(!RIC %in% nuovi$RIC), nuovi)
    resta <- setdiff(resta, nuovi$RIC)
  }
  attr(tutto, "non_recuperati") <- resta
  cat("Estrazione: richiesti", length(codici), "| non recuperati", length(resta), "\n")
  tutto
}

#  Esempio di f_lotto per le serie mensili (rendimento totale, prezzo, cap).
lseg_serie_mensili <- function(data_inizio, data_fine,
                               campi = c("TR.TotalReturn", "TR.PriceClose", "TR.CompanyMarketCap")) {
  function(rics) {
    res <- Refinitiv::rd_GetHistory(universe = rics, fields = campi,
      parameters = list("SDate" = data_inizio, "EDate" = data_fine, "Frq" = "M"))
    d <- rinomina_per_contenuto(res, list(RIC = "^instrument$|^ric$", Data = "date",
           ret = "totalreturn", prezzo = "priceclose|closeprice", mktcap = "marketcap"))
    num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
    d |> mutate(RIC = trimws(as.character(RIC)), Data = as.Date(Data),
                ret = num(ret) / 100, prezzo = num(prezzo), mktcap = num(mktcap))
  }
}

#  Esempio di f_lotto per gli identificativi.
lseg_identificativi <- function(campi = c("TR.OrganizationID", "TR.QuoteId", "TR.ShareClass",
                                          "TR.ISIN", "TR.CommonName")) {
  function(rics) {
    res <- Refinitiv::rd_GetData(rics = rics, Eikonformulas = campi)
    rinomina_per_contenuto(res, list(RIC = "^instrument$|^ric$", PermID = "organi",
      QuoteId = "quoteid", ShareClass = "shareclass", ISIN = "isin", Nome = "commonname|company")) |>
      come_testo(c("RIC", "PermID", "QuoteId", "ShareClass", "ISIN", "Nome")) |>
      mutate(across(c(PermID, ShareClass, ISIN, Nome),
                    ~ ifelse(.x %in% "No best match available", NA_character_, .x)))
  }
}

#  Lettura dei file di report LSEG (intestazioni da saltare).
leggi_eventi_lseg <- function(file, skip = 4)
  readxl::read_excel(file, skip = skip, col_names = c("status", "issuer", "code", "date"))
leggi_correnti_lseg <- function(file, skip = 1, escludi = "^Totals|^\\.")
  readxl::read_excel(file, skip = skip, col_names = c("code", "name", "mcap")) |>
    mutate(code = trimws(code)) |> filter(!is.na(code), !grepl(escludi, code))
