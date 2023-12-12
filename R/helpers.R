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
            print("Unexplored layer found")
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
  journals <- transactions[which(names(transactions) == "journal")]
  for (journal in journals) {
    journal[lengths(journal) == 0] <- NA
    records <- journal[lengths(journal) > 1]
    journal <- journal[lengths(journal) == 1]
    row1 <- unlist(journal)
    row <- row1
    for (record in records) {
      record[lengths(record) == 0] <- NA
      subfields <- record[lengths(record) > 1]
      record <- record[lengths(record) == 1]
      row2 <- unlist(record)
      row1 <- row[!(names(row) %in% intersect(names(row), names(row2)))]
      row <- c(row1, row2)
      for (subfield in subfields) {
        subfield[lengths(subfield) == 0] <- NA
        subsubfields <- subfield[lengths(subfield) > 1]
        subfield <- subfield[lengths(subfield) == 1]
        row3 <- unlist(subfield)
        row1 <- row1[!(names(row1) %in% intersect(names(row1), names(row3)))]
        row2 <- row2[!(names(row2) %in% intersect(names(row2), names(row3)))]
        row <- c(row1, row2, row3)
        for (subsubfield in subsubfields) {
          subsubfield[lengths(subsubfield) == 0] <- NA
          subsubsubfields <- subsubfield[lengths(subsubfield) > 1]
          subsubfield <- subsubfield[lengths(subsubfield) == 1]
          row4 <- unlist(subsubfield)
          row1 <- row1[!(names(row1) %in% intersect(names(row1), names(row4)))]
          row2 <- row2[!(names(row2) %in% intersect(names(row2), names(row4)))]
          row2 <- row2[!(names(row2) %in% intersect(names(row2), names(row4)))]
          row <- c(row1, row2, row3, row4)
          if (length(subsubsubfields) > 0) {
            print("Unexplored layer found")
          }
        }
        row["desc"] <- if (!is.null(row3["desc"])) row3["desc"] else NA
        rows[[index]] <- as.data.frame(t(row))
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

.add_amounts <- function(x) {
  x$amount <- as.numeric(x$amnt)
  x$amount[x$amntTp != "D"] <- -x$amount[x$amntTp != "D"]
  x <- x[, !colnames(x) %in% c("amntTp", "amnt")]
  x$debet <- x$amount
  x$debet[x$debet < 0] <- NA
  x$credit <- -x$amount
  x$credit[x$credit < 0] <- NA
  return(x)
}

.add_vats <- function(x, vats) {
  if ("vatID" %in% colnames(x)) {
    matched_vats <- vats[match(x$vatID, vats$vatID), ]
    x$vatDesc <- matched_vats$vatDesc
    x$vat_amount <- ifelse(!is.na(x$vatAmntTp), ifelse(x$vatAmntTp == "D", as.numeric(x$vatAmnt), -as.numeric(x$vatAmnt)), NA)
    x$vatToClaimAccID <- matched_vats$vatToClaimAccID
    x$vatToPayAccID <- matched_vats$vatToPayAccID
    x <- x[, !colnames(x) %in% c("vatAmntTp", "vatAmnt")]
  }
  return(x)
}

.add_relations <- function(x, relations) {
  if ("custSupID" %in% colnames(x)) {
    matched_relations <- relations[match(x$custSupID, relations$custSupID), ]
    x$cs_custSupName <- matched_relations$custSupName
    x$cs_taxRegistrationCountry <- matched_relations$taxRegistrationCountry
    x$cs_custSupTp <- matched_relations$custSupTp
    x$cs_country <- matched_relations$country
    x$cs_website <- matched_relations$website
    x$cs_city <- matched_relations$city
    x$cs_bankAccNr <- matched_relations$bankAccNr
    x$cs_bankIdCd <- matched_relations$bankIdCd
    x$cs_telephone <- matched_relations$telephone
    x$cs_commerceNr <- matched_relations$commerceNr
    x$cs_streetname <- matched_relations$streetName
    x$cs_postalCode <- matched_relations$postalCode
    x$cs_fax <- matched_relations$fax
    x$cs_eMail <- matched_relations$email
    x$cs_taxRegIdent <- matched_relations$taxRegIdent
    x$cs_contact <- matched_relations$contact
  }
  return(x)
}

.add_accounts <- function(x, accounts, lang) {
  matched_accounts <- accounts[match(x$accID, accounts$accID), ]
  x$accDesc <- matched_accounts$accDesc
  x$accTp <- matched_accounts$accTp
  x$leadCode <- matched_accounts$leadCode
  x$leadDescription <- matched_accounts$leadDescription
  x$leadReference <- matched_accounts$leadReference
  x$accountType <- switch(lang,
    "nl" = ifelse(x$accTp == "P", "Winst & Verlies", ifelse(x$accTp == "B", "Balans", "Onbekend balanstype")),
    "en" = ifelse(x$accTp == "P", "Profit & Loss", ifelse(x$accTp == "B", "Balance Sheet", "Unknown accounttype"))
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
  x$accountKind <- ifelse(!is.na(x$accID), lookup[as.integer(substr(x$accID, 1, 1)) + 1], NA)
  return(x)
}

.add_journals <- function(x, journals, lang) {
  matched_journals <- journals[match(x$jrnID, journals$jrnID), ]
  x$jrn_jrnID <- matched_journals$jrnID
  x$jrn_desc <- matched_journals$desc
  x$jrn_tp <- matched_journals$jrnTp
  x$jrn_offsetAccID <- matched_journals$offsetAccID
  x$jrn_bankAccNr <- matched_journals$bankAccNr
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
  x$jrn_journalType <- ifelse(!is.na(matched_journals$jrnTp), ifelse(x$jrn_tp %in% names(lookup), lookup[x$jrn_tp], unknown), NA)
  return(x)
}

.add_info <- function(x, file, header, company, transactions) {
  x$periodNumber <- as.numeric(x$periodNumber)
  x$trDt <- as.Date(x$trDt)
  x$effDate <- as.Date(x$effDate)
  x$fiscalYear <- header$fiscalYear[[1]]
  x$startDate <- header$startDate[[1]]
  x$endDate <- header$endDate[[1]]
  x$curCode <- header$curCode[[1]]
  x$dateCreated <- header$dateCreated[[1]]
  x$softwareDesc <- header$softwareDesc[[1]]
  x$companyIdent <- company$companyIdent[[1]]
  x$companyName <- company$companyName[[1]]
  x$taxRegistrationCountry <- company$taxRegistrationCountry[[1]]
  x$taxRegIdent <- if (length(company$taxRegIdent) > 0) company$taxRegIdent[[1]] else NA
  x$linesCount <- as.numeric(transactions$linesCount[[1]])
  x$totalDebit <- as.numeric(transactions$totalDebit[[1]])
  x$totalCredit <- as.numeric(transactions$totalCredit[[1]])
  x$effMonth <- match(months(x$effDate), month.name)
  return(x)
}

.clean_mutations <- function(x, clean, lang) {
  if (!clean) {
    mutations <- x
  } else {
    if (lang == "nl") {
      mutations <- data.frame(
        Dagboekcode = x$jrnID,
        Dagboek = x$jrn_desc,
        Datum = x$effDate,
        Periode = x$periodNumber,
        Grootboekcode = x$accID,
        Grootboek = x$accDesc,
        Omschrijving = x$desc,
        Boekstuk = if (!is.null(x$docRef)) x$docRef else NA,
        Debet = x$debet,
        Credit = x$credit,
        Saldo = x$amount,
        BTW.Code = if ("vatID" %in% colnames(x)) x$vatID else NA,
        BTW.Omschrijving = if ("vatID" %in% colnames(x)) x$vatDesc else NA,
        BTW.Saldo = if ("vatID" %in% colnames(x)) x$vat_amount else NA,
        Relatienummer = if ("custSupID" %in% colnames(x)) x$custSupID else NA,
        Relatie = if ("custSupID" %in% colnames(x)) x$cs_custSupName else NA,
        Volgnummer = x$nr,
        Lead.Code = if ("leadCode" %in% colnames(x)) x$leadCode else NA,
        Lead.Omschrijving = if ("leadDescription" %in% colnames(x)) x$leadDescription else NA,
        Soort = x$accountType,
        Type = x$accountKind,
        Verwerkingsdatum = x$trDt
      )
      rownames(mutations) <- seq_len(nrow(mutations))
      attr(mutations, "Bestandsnaam") <- unique(x$file)
      attr(mutations, "Datum") <- unique(x$dateCreated)
      attr(mutations, "Bedrijfsnaam") <- unique(x$companyName)
      attr(mutations, "Software") <- unique(x$softwareDesc)
      attr(mutations, "BTW.Land") <- unique(x$taxRegistrationCountry)
      attr(mutations, "Valuta") <- unique(x$curCode)
      attr(mutations, "FiscaalJaar") <- unique(x$fiscalYear)
      attr(mutations, "StartDatum") <- unique(x$startDate)
      attr(mutations, "EindDatum") <- unique(x$endDate)
      attr(mutations, "TotaalDebet") <- unique(x$totalDebit)
      attr(mutations, "TotaalCredit") <- unique(x$totalCredit)
    } else if (lang == "en") {
      mutations <- data.frame(
        JournalID = x$jrnID,
        Journal = x$jrn_desc,
        Date = x$effDate,
        Period = x$periodNumber,
        AccountID = x$accID,
        Account = x$accDesc,
        Description = x$desc,
        Document = if (!is.null(x$docRef)) x$docRef else NA,
        Debit = x$debet,
        Credit = x$credit,
        Amount = x$amount,
        VAT.Code = if ("vatID" %in% colnames(x)) x$vatID else NA,
        VAT.Description = if ("vatID" %in% colnames(x)) x$vatDesc else NA,
        VAT.Amount = if ("vatID" %in% colnames(x)) x$vat_amount else NA,
        RelationID = if ("custSupID" %in% colnames(x)) x$custSupID else NA,
        Relation = if ("custSupID" %in% colnames(x)) x$cs_custSupName else NA,
        Number = x$nr,
        Lead.Code = if ("leadCode" %in% colnames(x)) x$leadCode else NA,
        Lead.Description = if ("leadDescription" %in% colnames(x)) x$leadDescription else NA,
        Type = x$accountType,
        Kind = x$accountKind,
        Effective.Date = x$trDt
      )
      rownames(mutations) <- seq_len(nrow(mutations))
      attr(mutations, "File") <- unique(x$file)
      attr(mutations, "Date") <- unique(x$dateCreated)
      attr(mutations, "Company") <- unique(x$companyName)
      attr(mutations, "Software") <- unique(x$softwareDesc)
      attr(mutations, "VAT.Country") <- unique(x$taxRegistrationCountry)
      attr(mutations, "Currency") <- unique(x$curCode)
      attr(mutations, "FiscalYear") <- unique(x$fiscalYear)
      attr(mutations, "StartDate") <- unique(x$startDate)
      attr(mutations, "EndDate") <- unique(x$endDate)
      attr(mutations, "TotalDebit") <- unique(x$totalDebit)
      attr(mutations, "TotalCredit") <- unique(x$totalCredit)
    }
  }
  return(mutations)
}
