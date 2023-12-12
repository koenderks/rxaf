.xafvalue <- function(x) {
  if (length(x) > 0) {
    return(x[[1]])
  } else {
    return(NA)
  }
}

.construct_vats_index <- function(company) {
  vatCodes <- company$vatCodes[names(company$vatCodes) == "vatCode"]
  rows <- lapply(vatCodes, function(vatCode) {
    vatCode[lengths(vatCode) == 0] <- NA
    row <- data.frame(vatCode)
    colnames(row) <- names(vatCode)
    return(row)
  })
  tb <- dplyr::bind_rows(rows)
  tb <- tb[order(tb$vatID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_custsup_index <- function(company) {
  indices <- which(names(company$customersSuppliers) == "customerSupplier")
  n <- length(indices)
  customersSuppliers <- company$customersSuppliers[indices]
  tb <- do.call(rbind, lapply(customersSuppliers, function(customerSupplier) {
    data.frame(
      custSupID = .xafvalue(customerSupplier$custSupID),
      custSupName = .xafvalue(customerSupplier$custSupName),
      commerceNr = .xafvalue(customerSupplier$commerceNr),
      taxRegistrationCountry = .xafvalue(customerSupplier$taxRegistrationCountry),
      custSupTp = .xafvalue(customerSupplier$custSupTp),
      streetName = .xafvalue(customerSupplier$streetAddress$streetname),
      country = .xafvalue(customerSupplier$streetAddress$country),
      city = .xafvalue(customerSupplier$streetAddress$city),
      postalCode = .xafvalue(customerSupplier$streetAddress$postalCode),
      telephone = .xafvalue(customerSupplier$telephone),
      website = .xafvalue(customerSupplier$website),
      contact = .xafvalue(customerSupplier$contact),
      fax = .xafvalue(customerSupplier$fax),
      taxRegIdent = .xafvalue(customerSupplier$taxRegIdent),
      email = .xafvalue(customerSupplier$eMail),
      bankAccNr = .xafvalue(customerSupplier$bankAccount$bankAccNr),
      bankIdCd = .xafvalue(customerSupplier$bankAccount$bankIdCd)
    )
  }))
  tb <- tb[order(tb$custSupID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_account_index <- function(company, lang) {
  indices <- which(names(company$generalLedger) == "ledgerAccount")
  n <- length(indices)
  generalLedger <- company$generalLedger[indices]
  tb <- do.call(rbind, lapply(generalLedger, function(ledgerAccount) {
    accTp <- .xafvalue(ledgerAccount$accTp)
    accID <- .xafvalue(ledgerAccount$accID)
    data.frame(
      accID = accID,
      accDesc = .xafvalue(ledgerAccount$accDesc),
      accTp = accTp,
      leadCode = .xafvalue(ledgerAccount$leadCode),
      leadDescription = .xafvalue(ledgerAccount$leadDescription),
      leadReference = .xafvalue(ledgerAccount$leadReference),
      accountType = switch(lang,
        "nl" = ifelse(accTp == "P", "Winst & Verlies", ifelse(accTp == "B", "Balans", "Onbekend balanstype")),
        "en" = ifelse(accTp == "P", "Profit & Loss", ifelse(accTp == "B", "Balance Sheet", "Unknown accounttype"))
      ),
      accountKind = ifelse(!is.na(accID), switch(lang,
        "nl" = switch(substring(accID, 1, 1),
          "0" = "Vaste activa en passiva",
          "1" = "Vlottende activa en passiva",
          "2" = "Tussenrekening",
          "3" = "Voorraadrekening",
          "4" = "Kostenrekening",
          "5" = NA,
          "6" = NA,
          "7" = "Kostpijs rekening",
          "8" = "Omzet rekening",
          "9" = "Financiele baten en lasten"
        ),
        "en" = switch(substring(accID, 1, 1),
          "0" = "Fixed assets and liabilities",
          "1" = "Current assest and liabilities",
          "2" = "Suspense account",
          "3" = "Inventory account",
          "4" = "Expense account",
          "5" = NA,
          "6" = NA,
          "7" = "Cost account",
          "8" = "Revenue account",
          "9" = "Financial income and expenses"
        )
      ), NA)
    )
  }))
  tb <- tb[order(tb$accID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_journal_index <- function(company, lang) {
  indices <- which(names(company$transactions) == "journal")
  n <- length(indices)
  transactions <- company$transactions[indices]
  tb <- do.call(rbind, lapply(transactions, function(transaction) {
    jrnTp <- .xafvalue(transaction$jrnTp)
    jrnID <- .xafvalue(transaction$jrnID)
    data.frame(
      jrnID = jrnID,
      jrnDesc = .xafvalue(transaction$desc),
      jrnTp = jrnTp,
      offsetAccID = .xafvalue(transaction$offsetAccID),
      bankAccNr = .xafvalue(transaction$bankAccNr),
      journalType = if (!is.na(jrnTp)) {
        if (lang == "nl") {
          ifelse(jrnTp %in% c("Z", "B", "P", "O", "C", "M", "Y", "S"), switch(jrnTp,
            "Z" = "Memoriaal",
            "B" = "Bankboek",
            "P" = "Inkoopboek",
            "O" = "Open/Sluit balans",
            "C" = "Kasboek",
            "M" = "Memoriaal",
            "Y" = "Salaris",
            "S" = "Verkoopboek"
          ), "Onbekend dagboek")
        } else {
          ifelse(jrnTp %in% c("Z", "B", "P", "O", "C", "M", "Y", "S"), switch(jrnTp,
            "Z" = "Memorial",
            "B" = "Bank book",
            "P" = "Purchase book",
            "O" = "Open/Close balance",
            "C" = "Cash book",
            "M" = "Memorial",
            "Y" = "Salary",
            "S" = "Sales book"
          ), "Unknown journal")
        }
      }
    )
  }))
  tb <- tb[order(tb$jrnID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_mutations <- function(transactions, progress) {
  size <- as.numeric(transactions$linesCount[[1]])
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = size, initial = 0, width = 80, style = 3)
  }
  # Extract all tags from journals
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
  suppressMessages({
    df <- dplyr::bind_rows(rows)
  })
  return(df)
}

.add_raw_amounts <- function(df) {
  df$amount <- as.numeric(df$amnt)
  df$amount[df$amntTp != "D"] <- -df$amount[df$amntTp != "D"]
  df <- df[, !colnames(df) %in% c("amntTp", "amnt")]
  df$debet <- df$amount
  df$debet[df$debet < 0] <- NA
  df$credit <- -df$amount
  df$credit[df$credit < 0] <- NA
  return(df)
}

.add_raw_vats <- function(df, vats) {
  if ("vatID" %in% colnames(df)) {
    matched_vats <- vats[match(df$vatID, vats$vatID), ]
    df$vatDesc <- matched_vats$vatDesc
    df$vat_amount <- ifelse(is.na(df$vatAmntTp), NA, ifelse(df$vatAmntTp == "D", as.numeric(df$vatAmnt), -as.numeric(df$vatAmnt)))
    df$vatToClaimAccID <- matched_vats$vatToClaimAccID
    df$vatToPayAccID <- matched_vats$vatToPayAccID
    df <- df[, !colnames(df) %in% c("vatAmntTp", "vatAmnt")]
  }
  return(df)
}

.add_raw_relations <- function(df, suppliers) {
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

.add_raw_accounts <- function(df, accounts) {
  matched_accounts <- accounts[match(df$accID, accounts$accID), ]
  df$accDesc <- matched_accounts$accDesc
  df$accTp <- matched_accounts$accTp
  df$leadCode <- matched_accounts$leadCode
  df$leadDescription <- matched_accounts$leadDescription
  df$leadReference <- matched_accounts$leadReference
  df$accountType <- matched_accounts$accountType
  df$accountKind <- matched_accounts$accountKind
  return(df)
}

.add_raw_journals <- function(df, journals) {
  matched_journals <- journals[match(df$jrnID, journals$jrnID), ]
  df$jrn_jrnID <- matched_journals$jrnID
  df$jrn_desc <- matched_journals$jrnDesc
  df$jrn_offsetAccID <- matched_journals$offsetAccID
  df$jrn_bankAccNr <- matched_journals$bankAccNr
  df$jrn_journaltype <- matched_journals$journalType
  return(df)
}

.add_raw_info <- function(df, file, header, company, transactions) {
  df$file <- basename(file)
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
  df$effMonth <- match(months(as.Date(df$effDate)), month.name)
  df$account <- paste0(df$accID, " - ", df$accDesc)
  df$journal <- paste0(df$jrnID, " - ", df$jrn_journaltype)
  df$trDt <- as.Date(df$trDt)
  df$periodNumber <- as.numeric(df$periodNumber)
  df$effDate <- as.Date(df$effDate)
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
        Lead.Code = df$leadCode,
        Lead.Omschrijving = df$leadDescription,
        Soort = df$accountType,
        Type = df$accountKind,
        Verwerkingsdatum = df$trDt
      )
      result <- result[order(result$Periode, result$Datum), ]
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
        Lead.Code = df$leadCode,
        Lead.Description = df$leadDescription,
        Type = df$accountType,
        Kind = df$accountKind,
        Effective.Date = df$trDt
      )
      result <- result[order(result$Period, result$Date), ]
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
