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
    tb$vatDesc[index] <- vatCode$vatDesc[[1]]
    tb$vatToClaimAccID[index] <- if (length(vatCode$vatToClaimAccID) > 0) vatCode$vatToClaimAccID[[1]] else NA
    tb$vatToPayAccID[index] <- if (length(vatCode$vatToPayAccID) > 0) vatCode$vatToPayAccID[[1]] else NA
    index <- index + 1
  }
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
    tb$custSupName[index] <- customerSupplier$custSupName[[1]]
    tb$commerceNr[index] <- if (length(customerSupplier$commerceNr) > 0) customerSupplier$commerceNr[[1]] else NA
    tb$taxRegistrationCountry[index] <- if (length(customerSupplier$taxRegistrationCountry) > 0) customerSupplier$taxRegistrationCountry[[1]] else NA
    tb$taxRegIdent[index] <- if (length(customerSupplier$taxRegIdent) > 0) customerSupplier$taxRegIdent[[1]] else NA
    tb$custSupTp[index] <- customerSupplier$custSupTp[[1]]
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
    accountType = character(n)
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
    index <- index + 1
  }
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
  return(tb)
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
        Debnr.Crednr = if ("custSupID" %in% colnames(df)) df$custSupID else NA,
        Debnaam.Crednaam = if ("custSupID" %in% colnames(df)) df$cs_custSupName else NA,
        Volgnr = df$nr,
        LeadCode = df$leadCode,
        LeadDescription = df$leadDescription,
        Soort = df$accountType,
        Verwerkingsdatum = df$trDt
      )
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
        CustomerSupplierID = if ("custSupID" %in% colnames(df)) df$custSupID else NA,
        CustomerSupplierName = if ("custSupID" %in% colnames(df)) df$cs_custSupName else NA,
        Number = df$nr,
        LeadCode = df$leadCode,
        LeadDescription = df$leadDescription,
        Type = df$accountType,
        EffectiveDate = df$trDt
      )
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
