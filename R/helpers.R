.construct_vats_index <- function(company) {
  indices <- which(names(company$vatCodes) == "vatCode")
  n <- length(indices)
  tb <- data.frame(
    vatID = character(n),
    vatDesc = character(n),
    vatToClaimAccID = character(n),
    vatToPayAccID = character(n)
  )
  index <- 1
  for (i in indices) {
    vatCode <- company$vatCodes[i]$vatCode
    tb$vatID[index] <- vatCode$vatID[[1]]
    tb$vatDesc[index] <- if (length(vatCode$vatDesc) > 0) vatCode$vatDesc[[1]] else NA
    tb$vatToClaimAccID[index] <- if (length(vatCode$vatToClaimAccID) > 0) vatCode$vatToClaimAccID[[1]] else NA
    tb$vatToPayAccID[index] <- if (length(vatCode$vatToPayAccID) > 0) vatCode$vatToPayAccID[[1]] else NA
    index <- index + 1
  }
  tb <- tb[order(tb$vatID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_custsup_index <- function(company) {
  indices <- which(names(company$customersSuppliers) == "customerSupplier")
  n <- length(indices)
  tb <- data.frame(
    custSupID = character(n),
    custSupName = character(n),
    commerceNr = character(n),
    taxRegistrationCountry = character(n),
    custSupTp = character(n),
    streetName = character(n),
    country = character(n),
    city = character(n),
    postalCode = character(n),
    telephone = character(n),
    website = character(n),
    contact = character(n),
    fax = character(n),
    taxRegIdent = character(n),
    email = character(n),
    bankAccNr = character(n),
    bankIdCd = character(n)
  )
  index <- 1
  for (i in indices) {
    customerSupplier <- company$customersSuppliers[i]$customerSupplier
    tb$custSupID[index] <- customerSupplier$custSupID[[1]]
    tb$custSupName[index] <- if (length(customerSupplier$custSupName) > 0) customerSupplier$custSupName[[1]] else NA
    tb$commerceNr[index] <- if (length(customerSupplier$commerceNr) > 0) customerSupplier$commerceNr[[1]] else NA
    tb$taxRegistrationCountry[index] <- if (length(customerSupplier$taxRegistrationCountry) > 0) customerSupplier$taxRegistrationCountry[[1]] else NA
    tb$taxRegIdent[index] <- if (length(customerSupplier$taxRegIdent) > 0) customerSupplier$taxRegIdent[[1]] else NA
    tb$custSupTp[index] <- if (length(customerSupplier$custSupTp) > 0) customerSupplier$custSupTp[[1]] else NA
    tb$streetName[index] <- if (length(customerSupplier$streetAddress$streetname) > 0) customerSupplier$streetAddress$streetname[[1]] else NA
    tb$country[index] <- if (length(customerSupplier$streetAddress$country) > 0) customerSupplier$streetAddress$country[[1]] else NA
    tb$city[index] <- if (length(customerSupplier$streetAddress$city) > 0) customerSupplier$streetAddress$city[[1]] else NA
    tb$postalCode[index] <- if (length(customerSupplier$streetAddress$postalCode) > 0) customerSupplier$streetAddress$postalCode[[1]] else NA
    tb$telephone[index] <- if (length(customerSupplier$telephone) > 0) customerSupplier$telephone[[1]] else NA
    tb$website[index] <- if (length(customerSupplier$website) > 0) customerSupplier$website[[1]] else NA
    tb$contact[index] <- if (length(customerSupplier$contact) > 0) customerSupplier$contact[[1]] else NA
    tb$fax[index] <- if (length(customerSupplier$fax) > 0) customerSupplier$fax[[1]] else NA
    tb$email[index] <- if (length(customerSupplier$eMail) > 0) customerSupplier$eMail[[1]] else NA
    tb$bankAccNr[index] <- if (length(customerSupplier$bankAccount$bankAccNr) > 0) customerSupplier$bankAccount$bankAccNr[[1]] else NA
    tb$bankIdCd[index] <- if (length(customerSupplier$bankAccount$bankIdCd) > 0) customerSupplier$bankAccount$bankIdCd[[1]] else NA
    index <- index + 1
  }
  tb <- tb[order(tb$custSupID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_account_index <- function(company, lang) {
  indices <- which(names(company$generalLedger) == "ledgerAccount")
  n <- length(indices)
  tb <- data.frame(
    accID = character(n),
    accDesc = character(n),
    accTp = character(n),
    leadCode = character(n),
    leadDescription = character(n),
    leadReference = character(n),
    accountType = character(n),
    accountKind = character(n)
  )
  index <- 1
  for (i in indices) {
    ledgerAccount <- company$generalLedger[i]$ledgerAccount
    tb$accID[index] <- ledgerAccount$accID[[1]]
    tb$accDesc[index] <- ledgerAccount$accDesc[[1]]
    tb$accTp[index] <- ledgerAccount$accTp[[1]]
    tb$leadCode[index] <- if (length(ledgerAccount$leadCode) > 0) ledgerAccount$leadCode[[1]] else NA
    tb$leadDescription[index] <- if (length(ledgerAccount$leadDescription) > 0) ledgerAccount$leadDescription[[1]] else NA
    tb$leadReference[index] <- if (length(ledgerAccount$leadReference) > 0) ledgerAccount$leadReference[[1]] else NA
    tb$accountType[index] <- switch(lang,
      "nl" = ifelse(tb$accTp[index] == "P", "Winst & Verlies", ifelse(tb$accTp[index] == "B", "Balans", "Onbekend balanstype")),
      "en" = ifelse(tb$accTp[index] == "P", "Profit & Loss", ifelse(tb$accTp[index] == "B", "Balance Sheet", "Unknown accounttype"))
    )
    tb$accountKind[index] <- switch(lang,
      "nl" = switch(substring(tb$accID[index], 1, 1),
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
      "en" = switch(substring(tb$accID[index], 1, 1),
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
    )
    index <- index + 1
  }
  tb <- tb[order(tb$accID), ]
  rownames(tb) <- seq_len(nrow(tb))
  return(tb)
}

.construct_journal_index <- function(company, lang) {
  indices <- which(names(company$transactions) == "journal")
  n <- length(indices)
  tb <- data.frame(
    jrnID = character(n),
    jrnDesc = character(n),
    jrnTp = character(n),
    offsetAccID = character(n),
    bankAccNr = character(n),
    journalType = character(n)
  )
  index <- 1
  for (i in indices) {
    journal <- company$transactions[i]$journal
    tb$jrnID[index] <- journal$jrnID[[1]]
    tb$jrnDesc[index] <- journal$desc[[1]]
    tb$jrnTp[index] <- journal$jrnTp[[1]]
    tb$offsetAccID[index] <- if (is.null(journal$offsetAccID[[1]])) NA else journal$offsetAccID[[1]]
    tb$bankAccNr[index] <- if (!is.null(journal$bankAccNr)) journal$bankAccNr[[1]] else NA
    if (lang == "nl") {
      tb$journalType[index] <- ifelse(tb$jrnTp[index] %in% c("Z", "B", "P", "O", "C", "M", "Y", "S"), switch(tb$jrnTp[index],
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
      tb$journalType[index] <- ifelse(tb$jrnTp[index] %in% c("Z", "B", "P", "O", "C", "M", "Y", "S"), switch(tb$jrnTp[index],
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
    index <- index + 1
  }
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
  df$amount <- ifelse(df$amntTp == "D", as.numeric(df$amnt), -as.numeric(df$amnt))
  df <- df[, -which(colnames(df) %in% c("amntTp", "amnt"))]
  df$debet <- ifelse(df$amount > 0, df$amount, NA)
  df$credit <- ifelse(df$amount < 0, abs(df$amount), NA)
  return(df)
}

.add_raw_vats <- function(df, vats) {
  if ("vatID" %in% colnames(df)) {
    df$vatDesc <- vats$vatDesc[match(df$vatID, vats$vatID)]
    df$vat_amount <- ifelse(is.na(df$vatAmntTp), NA, ifelse(df$vatAmntTp == "D", as.numeric(df$vatAmnt), -as.numeric(df$vatAmnt)))
    df$vatToClaimAccID <- vats$vatToClaimAccID[match(df$vatID, vats$vatID)]
    df$vatToPayAccID <- vats$vatToPayAccID[match(df$vatID, vats$vatID)]
    df <- df[, -which(colnames(df) %in% c("vatAmntTp", "vatAmnt"))]
  }
  return(df)
}

.add_raw_relations <- function(df, suppliers) {
  if ("custSupID" %in% colnames(df)) {
    df$cs_custSupName <- suppliers$custSupName[match(df$custSupID, suppliers$custSupID)]
    df$cs_taxRegistrationCountry <- suppliers$taxRegistrationCountry[match(df$custSupID, suppliers$custSupID)]
    df$cs_custSupTp <- suppliers$custSupTp[match(df$custSupID, suppliers$custSupID)]
    df$cs_country <- suppliers$country[match(df$custSupID, suppliers$custSupID)]
    df$cs_website <- suppliers$website[match(df$custSupID, suppliers$custSupID)]
    df$cs_city <- suppliers$city[match(df$custSupID, suppliers$custSupID)]
    df$cs_bankAccNr <- suppliers$bankAccNr[match(df$custSupID, suppliers$custSupID)]
    df$cs_bankIdCd <- suppliers$bankIdCd[match(df$custSupID, suppliers$custSupID)]
    df$cs_telephone <- suppliers$telephone[match(df$custSupID, suppliers$custSupID)]
    df$cs_commerceNr <- suppliers$commerceNr[match(df$custSupID, suppliers$custSupID)]
    df$cs_streetname <- suppliers$streetName[match(df$custSupID, suppliers$custSupID)]
    df$cs_postalCode <- suppliers$postalCode[match(df$custSupID, suppliers$custSupID)]
    df$cs_fax <- suppliers$fax[match(df$custSupID, suppliers$custSupID)]
    df$cs_eMail <- suppliers$email[match(df$custSupID, suppliers$custSupID)]
    df$cs_taxRegIdent <- suppliers$taxRegIdent[match(df$custSupID, suppliers$custSupID)]
    df$cs_contact <- suppliers$contact[match(df$custSupID, suppliers$custSupID)]
  }
  return(df)
}

.add_raw_accounts <- function(df, accounts) {
  df$accDesc <- accounts$accDesc[match(df$accID, accounts$accID)]
  df$accTp <- accounts$accTp[match(df$accID, accounts$accID)]
  df$leadCode <- accounts$leadCode[match(df$accID, accounts$accID)]
  df$leadDescription <- accounts$leadDescription[match(df$accID, accounts$accID)]
  df$leadReference <- accounts$leadReference[match(df$accID, accounts$accID)]
  df$accountType <- accounts$accountType[match(df$accID, accounts$accID)]
  df$accountKind <- accounts$accountKind[match(df$accID, accounts$accID)]
  return(df)
}

.add_raw_journals <- function(df, journals) {
  df$jrn_jrnID <- journals$jrnID[match(df$jrnID, journals$jrnID)]
  df$jrn_desc <- journals$jrnDesc[match(df$jrnID, journals$jrnID)]
  df$jrn_offsetAccID <- journals$offsetAccID[match(df$jrnID, journals$jrnID)]
  df$jrn_bankAccNr <- journals$bankAccNr[match(df$jrnID, journals$jrnID)]
  df$jrn_journaltype <- journals$journalType[match(df$jrnID, journals$jrnID)]
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
