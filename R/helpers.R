.construct_subtable <- function(company, data_name, id_name, sort_name, exclude_name = NULL) {
  data <- company[[data_name]][names(company[[data_name]]) == id_name]
  rows <- lapply(data, function(datapoint) {
    if (!is.null(exclude_name)) {
      datapoint <- datapoint[which(names(datapoint) != exclude_name)]
    }
    datapoint[lengths(datapoint) == 0] <- NA
    records <- lengths(datapoint) == 1
    row <- list()
    for (i in seq_len(length(datapoint))) {
      if (records[i]) {
        if (!is.null(names(datapoint[i][[1]]))) {
          row[[names(datapoint[i][[1]])]] <- unlist(datapoint[[i]])
        } else {
          row[[names(datapoint[i])]] <- unlist(datapoint[[i]])
        }
      } else {
        datapoint[[i]][lengths(datapoint[[i]]) == 0] <- NA
        subrecords <- lengths(datapoint[[i]]) == 1
        for (j in seq_along(subrecords)) {
          if (subrecords[j]) {
            row[[names(datapoint[[i]][j])]] <- unlist(datapoint[[i]][[j]])
          } else {
            print("Another sublayer")
          }
        }
      }
    }
    row <- data.frame(row)
    return(row)
  })
  tb <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  tb <- tb[order(tb[, sort_name]), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_mutations <- function(transactions, progress) {
  size <- as.numeric(transactions$linesCount[[1]])
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = size, initial = 0, width = 80, style = 3)
  }
  rows <- vector("list", size)
  index <- 1
  entries <- transactions[which(names(transactions) == "journal")]
  for (i in seq_along(entries)) {
    journal <- entries[i]$journal
    journal[lengths(journal) == 0] <- NA
    transaction_indices <- which(names(journal) == "transaction")
    non_transaction_indices <- which(names(journal) != "transaction")
    records <- journal[transaction_indices]
    row_part1 <- data.frame(journal[non_transaction_indices])
    colnames(row_part1) <- names(unlist(journal[non_transaction_indices]))
    row <- row_part1
    for (j in seq_along(records)) {
      record <- records[j]$transaction
      record[lengths(record) == 0] <- NA
      trLine_indices <- which(names(record) == "trLine")
      non_trLine_indices <- which(names(record) != "trLine")
      subfields <- record[trLine_indices]
      row_part2 <- data.frame(record[non_trLine_indices])
      colnames(row_part2) <- names(unlist(record[non_trLine_indices]))
      common_cols <- intersect(colnames(row_part1), colnames(row_part2))
      row_part1 <- row_part1[, !(names(row_part1) %in% common_cols)]
      row <- cbind(row_part1, row_part2)
      for (k in seq_along(subfields)) {
        subfield <- subfields[k]$trLine
        subfield[lengths(subfield) == 0] <- NA
        vat_indices <- which(names(subfield) == "vat")
        non_vat_indices <- which(names(subfield) != "vat")
        row_part3 <- data.frame(subfield[non_vat_indices])
        colnames(row_part3) <- names(subfield[non_vat_indices])
        if (length(vat_indices) > 0) {
          subfields1 <- subfield[vat_indices]
          subfields1[lengths(subfields1) == 0] <- NA
          vat <- data.frame(subfields1$vat)
          colnames(vat) <- names(subfields1$vat)
          row_part3 <- cbind(row_part3, vat)
        }
        common_cols <- intersect(colnames(row), colnames(row_part3))
        row <- row[, !(names(row) %in% common_cols)]
        row <- cbind(row, row_part3)
        row[["desc"]] <- if (is.null(row_part3[["desc"]])) NA else row_part3[["desc"]] # Always take description from the transaction, even if missing
        rows[[index]] <- row
        if (progress) {
          utils::setTxtProgressBar(pb, index)
        }
        index <- index + 1
      }
    }
  }
  df <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  return(df)
}

.raw_amounts <- function(df) {
  df$amount <- as.numeric(df$amnt)
  df$amount[df$amntTp != "D"] <- -df$amount[df$amntTp != "D"]
  df <- df[, !colnames(df) %in% c("amntTp", "amnt")]
  df$debet <- df$amount
  df$debet[df$debet < 0] <- NA
  df$credit <- -df$amount
  df$credit[df$credit < 0] <- NA
  return(df)
}

.raw_vats <- function(df, vats) {
  if ("vatID" %in% colnames(df)) {
    matched_vats <- vats[match(df$vatID, vats$vatID), ]
    df$vatDesc <- matched_vats$vatDesc
    df$vat_amount <- ifelse(!is.na(df$vatAmntTp), ifelse(df$vatAmntTp == "D", as.numeric(df$vatAmnt), -as.numeric(df$vatAmnt)), NA)
    df$vatToClaimAccID <- matched_vats$vatToClaimAccID
    df$vatToPayAccID <- matched_vats$vatToPayAccID
    df <- df[, !colnames(df) %in% c("vatAmntTp", "vatAmnt")]
  }
  return(df)
}

.raw_relations <- function(df, suppliers) {
  if ("custSupID" %in% colnames(df)) {
    matched_suppliers <- suppliers[match(df$custSupID, suppliers$custSupID), ]
    df$cs_custSupName <- matched_suppliers$custSupName
    df$cs_taxRegistrationCountry <- matched_suppliers$taxRegistrationCountry
    df$cs_custSupTp <- matched_suppliers$custSupTp
    df$cs_country <- matched_suppliers$country
    df$cs_website <- matched_suppliers$website
    df$cs_city <- matched_suppliers$city
    df$cs_bankAccNr <- matched_suppliers$bankAccNr
    df$cs_bankIdCd <- matched_suppliers$bankIdCd
    df$cs_telephone <- matched_suppliers$telephone
    df$cs_commerceNr <- matched_suppliers$commerceNr
    df$cs_streetname <- matched_suppliers$streetName
    df$cs_postalCode <- matched_suppliers$postalCode
    df$cs_fax <- matched_suppliers$fax
    df$cs_eMail <- matched_suppliers$email
    df$cs_taxRegIdent <- matched_suppliers$taxRegIdent
    df$cs_contact <- matched_suppliers$contact
  }
  return(df)
}

.raw_accounts <- function(df, accounts, lang) {
  matched_accounts <- accounts[match(df$accID, accounts$accID), ]
  df$accDesc <- matched_accounts$accDesc
  df$accTp <- matched_accounts$accTp
  df$leadCode <- matched_accounts$leadCode
  df$leadDescription <- matched_accounts$leadDescription
  df$leadReference <- matched_accounts$leadReference
  df$accountType <- switch(lang,
    "nl" = ifelse(df$accTp == "P", "Winst & Verlies", ifelse(df$accTp == "B", "Balans", "Onbekend balanstype")),
    "en" = ifelse(df$accTp == "P", "Profit & Loss", ifelse(df$accTp == "B", "Balance Sheet", "Unknown accounttype"))
  )
  lookup <- switch(lang,
    "nl" = c(
      "Vaste activa en passiva", "Vlottende activa en passiva", "Tussenrekening",
      "Voorraadrekening", "Kostenrekening", NA, NA, "Kostpijs rekening",
      "Omzet rekening", "Financiele baten en lasten"
    ),
    "en" = c(
      "Fixed assets and liabilities", "Current assest and liabilities", "Suspense account",
      "Inventory account", "Expense account", NA, NA, "Cost account",
      "Revenue account", "Financial income and expenses"
    )
  )
  df$accountKind <- ifelse(!is.na(df$accID), lookup[as.integer(substr(df$accID, 1, 1)) + 1], NA)
  return(df)
}

.raw_journals <- function(df, journals, lang) {
  matched_journals <- journals[match(df$jrnID, journals$jrnID), ]
  df$jrn_jrnID <- matched_journals$jrnID
  df$jrn_desc <- matched_journals$desc
  df$jrn_tp <- matched_journals$jrnTp
  df$jrn_offsetAccID <- matched_journals$offsetAccID
  df$jrn_bankAccNr <- matched_journals$bankAccNr
  lookup <- switch(lang,
    "nl" = c(
      "Z" = "Memoriaal", "B" = "Bankboek", "P" = "Inkoopboek", "O" = "Open/Sluit balans",
      "C" = "Kasboek", "M" = "Memoriaal", "Y" = "Salaris", "S" = "Verkoopboek"
    ),
    "en" = c(
      "Z" = "Memorial", "B" = "Bank book", "P" = "Purchase book", "O" = "Open/Close balance",
      "C" = "Cash book", "M" = "Memorial", "Y" = "Salary", "S" = "Sales book"
    )
  )
  unknown <- switch(lang,
    "nl" = "Onbekend dagboek",
    "en" = "Unknown journal"
  )
  df$jrn_journalType <- ifelse(!is.na(matched_journals$jrnTp), ifelse(df$jrn_tp %in% names(lookup), lookup[df$jrn_tp], unknown), NA)
  return(df)
}

.raw_info <- function(df, file, header, company, transactions) {
  df$periodNumber <- as.numeric(df$periodNumber)
  df$trDt <- as.Date(df$trDt)
  df$effDate <- as.Date(df$effDate)
  df$fiscalYear <- header$fiscalYear[[1]]
  df$startDate <- header$startDate[[1]]
  df$endDate <- header$endDate[[1]]
  df$curCode <- header$curCode[[1]]
  df$dateCreated <- header$dateCreated[[1]]
  df$softwareDesc <- header$softwareDesc[[1]]
  df$companyIdent <- company$companyIdent[[1]]
  df$companyName <- company$companyName[[1]]
  df$taxRegistrationCountry <- company$taxRegistrationCountry[[1]]
  df$taxRegIdent <- if (length(company$taxRegIdent) > 0) company$taxRegIdent[[1]] else NA
  df$linesCount <- as.numeric(transactions$linesCount[[1]])
  df$totalDebit <- as.numeric(transactions$totalDebit[[1]])
  df$totalCredit <- as.numeric(transactions$totalCredit[[1]])
  df$effMonth <- match(months(df$effDate), month.name)
  return(df)
}

.clean_xaf <- function(df, clean, lang) {
  if (!clean) {
    result <- df
  } else {
    if (lang == "nl") {
      result <- data.frame(
        Dagboekcode = df$jrnID,
        Dagboek = df$jrn_desc,
        Datum = df$effDate,
        Periode = df$periodNumber,
        Grootboekcode = df$accID,
        Grootboek = df$accDesc,
        Omschrijving = df$desc,
        Boekstuk = if (!is.null(df$docRef)) df$docRef else NA,
        Debet = df$debet,
        Credit = df$credit,
        Saldo = df$amount,
        BTW.Code = if ("vatID" %in% colnames(df)) df$vatID else NA,
        BTW.Omschrijving = if ("vatID" %in% colnames(df)) df$vatDesc else NA,
        BTW.Saldo = if ("vatID" %in% colnames(df)) df$vat_amount else NA,
        Relatienummer = if ("custSupID" %in% colnames(df)) df$custSupID else NA,
        Relatie = if ("custSupID" %in% colnames(df)) df$cs_custSupName else NA,
        Volgnummer = df$nr,
        Lead.Code = if ("leadCode" %in% colnames(df)) df$leadCode else NA,
        Lead.Omschrijving = if ("leadDescription" %in% colnames(df)) df$leadDescription else NA,
        Soort = df$accountType,
        Type = df$accountKind,
        Verwerkingsdatum = df$trDt
      )
      rownames(result) <- seq_len(nrow(result))
      attr(result, "Bestandsnaam") <- unique(df$file)
      attr(result, "Datum") <- unique(df$dateCreated)
      attr(result, "Bedrijfsnaam") <- unique(df$companyName)
      attr(result, "Software") <- unique(df$softwareDesc)
      attr(result, "BTW.Land") <- unique(df$taxRegistrationCountry)
      attr(result, "Valuta") <- unique(df$curCode)
      attr(result, "FiscaalJaar") <- unique(df$fiscalYear)
      attr(result, "StartDatum") <- unique(df$startDate)
      attr(result, "EindDatum") <- unique(df$endDate)
      attr(result, "TotaalDebet") <- unique(df$totalDebit)
      attr(result, "TotaalCredit") <- unique(df$totalCredit)
    } else if (lang == "en") {
      result <- data.frame(
        JournalID = df$jrnID,
        Journal = df$jrn_desc,
        Date = df$effDate,
        Period = df$periodNumber,
        AccountID = df$accID,
        Account = df$accDesc,
        Description = df$desc,
        Document = if (!is.null(df$docRef)) df$docRef else NA,
        Debit = df$debet,
        Credit = df$credit,
        Amount = df$amount,
        VAT.Code = if ("vatID" %in% colnames(df)) df$vatID else NA,
        VAT.Description = if ("vatID" %in% colnames(df)) df$vatDesc else NA,
        VAT.Amount = if ("vatID" %in% colnames(df)) df$vat_amount else NA,
        RelationID = if ("custSupID" %in% colnames(df)) df$custSupID else NA,
        Relation = if ("custSupID" %in% colnames(df)) df$cs_custSupName else NA,
        Number = df$nr,
        Lead.Code = if ("leadCode" %in% colnames(df)) df$leadCode else NA,
        Lead.Description = if ("leadDescription" %in% colnames(df)) df$leadDescription else NA,
        Type = df$accountType,
        Kind = df$accountKind,
        Effective.Date = df$trDt
      )
      rownames(result) <- seq_len(nrow(result))
      attr(result, "File") <- unique(df$file)
      attr(result, "Date") <- unique(df$dateCreated)
      attr(result, "Company") <- unique(df$companyName)
      attr(result, "Software") <- unique(df$softwareDesc)
      attr(result, "VAT.Country") <- unique(df$taxRegistrationCountry)
      attr(result, "Currency") <- unique(df$curCode)
      attr(result, "FiscalYear") <- unique(df$fiscalYear)
      attr(result, "StartDate") <- unique(df$startDate)
      attr(result, "EndDate") <- unique(df$endDate)
      attr(result, "TotalDebit") <- unique(df$totalDebit)
      attr(result, "TotalCredit") <- unique(df$totalCredit)
    }
  }
  return(result)
}
