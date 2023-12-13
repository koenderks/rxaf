# Helper function to extract subtables containing journal, accounts, VAT codes and customers/suppliers
.construct_subtable <- function(company, data_name, id_name, sort_name, exclude_name = NULL) {
  data <- company[[data_name]][names(company[[data_name]]) == id_name]
  rows <- lapply(data, function(datapoint) {
    if (!is.null(exclude_name)) {
      datapoint <- datapoint[names(datapoint) != exclude_name]
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
  subtable <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  subtable <- subtable[order(subtable[, sort_name]), ]
  rownames(subtable) <- seq_len(nrow(subtable))
  return(subtable)
}

# Function to extract all information from the transactions element of the XAF file
.construct_mutations <- function(transactions, progress) {
  size <- as.numeric(transactions$linesCount[[1]])
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = size, initial = 0, width = 80, style = 3)
  }
  rows <- vector("list", size)
  index <- 1
  journals <- transactions[names(transactions) == "journal"]
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
      row12 <- row1[!(names(row1) %in% intersect(names(row1), names(row2)))]
      row12 <- c(row12, row2)
      row <- row12
      for (subfield in subfields) {
        subfield[lengths(subfield) == 0] <- NA
        subsubfields <- subfield[lengths(subfield) > 1]
        subfield <- subfield[lengths(subfield) == 1]
        row3 <- unlist(subfield)
        row23 <- row12[!(names(row12) %in% intersect(names(row12), names(row3)))]
        row23 <- c(row23, row3)
        row <- row23
        for (subsubfield in subsubfields) {
          subsubfield[lengths(subsubfield) == 0] <- NA
          subsubsubfields <- subsubfield[lengths(subsubfield) > 1]
          subsubfield <- subsubfield[lengths(subsubfield) == 1]
          row4 <- unlist(subsubfield)
          row34 <- row23[!(names(row23) %in% intersect(names(row23), names(row4)))]
          row34 <- c(row34, row4)
          row <- row34
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
  mutations <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  return(mutations)
}

# Function to format the mutation amounts
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

# Function to match the VAT codes
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

# Function to match the customers/suppliers
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

# Function to match the accounts
.add_accounts <- function(x, accounts, lang) {
  matched_accounts <- accounts[match(x$accID, accounts$accID), ]
  x$accDesc <- matched_accounts$accDesc
  x$accTp <- matched_accounts$accTp
  x$leadCode <- matched_accounts$leadCode
  x$leadDescription <- matched_accounts$leadDescription
  x$leadReference <- matched_accounts$leadReference
  if (lang == "nl") {
    x$accountType <- ifelse(x$accTp == "P", "Winst & Verlies", ifelse(x$accTp == "B", "Balans", "Onbekend balanstype"))
    lookup <- c(
      "Vaste activa en passiva", "Vlottende activa en passiva", "Tussenrekening",
      "Voorraadrekening", "Kostenrekening", "Overig", "Overig", "Kostpijs rekening",
      "Omzet rekening", "Financiele baten en lasten"
    )
  } else {
    x$accountType <- ifelse(x$accTp == "P", "Profit & Loss", ifelse(x$accTp == "B", "Balance sheet", "Unknown accounttype"))
    lookup <- c(
      "Fixed assets and liabilities", "Current assest and liabilities", "Suspense account",
      "Inventory account", "Expense account", "Other", "Other", "Cost account",
      "Revenue account", "Financial income and expenses"
    )
  }
  x$accountKind <- ifelse(!is.na(x$accID), lookup[as.integer(substr(x$accID, 1, 1)) + 1], NA)
  return(x)
}

# Function to match the journals
.add_journals <- function(x, journals, lang) {
  matched_journals <- journals[match(x$jrnID, journals$jrnID), ]
  x$jrn_jrnID <- matched_journals$jrnID
  x$jrn_desc <- matched_journals$desc
  x$jrn_tp <- matched_journals$jrnTp
  x$jrn_offsetAccID <- matched_journals$offsetAccID
  x$jrn_bankAccNr <- matched_journals$bankAccNr
  if (lang == "nl") {
    lookup <- c(
      "Z" = "Memoriaal", "B" = "Bankboek", "P" = "Inkoopboek", "O" = "Open/Sluit balans",
      "C" = "Kasboek", "M" = "Memoriaal", "Y" = "Salaris", "S" = "Verkoopboek"
    )
    unknown <- "Onbekend dagboek"
  } else {
    lookup <- c(
      "Z" = "Memorial", "B" = "Bank book", "P" = "Purchase book", "O" = "Open/Close balance",
      "C" = "Cash book", "M" = "Memorial", "Y" = "Salary", "S" = "Sales book"
    )
    unknown <- "Unknown journal"
  }
  x$jrn_journalType <- ifelse(!is.na(matched_journals$jrnTp), ifelse(x$jrn_tp %in% names(lookup), lookup[x$jrn_tp], unknown), NA)
  return(x)
}

# Function to add additional info
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

# Function to clean the raw data
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

# Function to create a balance sheet or an income statement
.xaf_statement <- function(x, date = NULL, type = c("balance_sheet", "income_statement")) {
  type <- match.arg(type)
  stopifnot("'x' is not output from 'read_xaf()'" = inherits(x, "xaf"))
  stopifnot("not supported for uncleaned files'" = attr(x, "clean"))
  lang <- attr(x, "lang")
  if (!is.null(date)) {
    date <- try(as.Date(date, format = "%d-%m-%Y"))
    stopifnot("stop" = !inherits(date, "try-error"))
    if (lang == "nl") {
      x_new <- x[x$Datum <= date, ]
      dates <- x$Datum
    } else {
      x_new <- x[x$Date <= date, ]
      dates <- x$Date
    }
    if (nrow(x_new) == 0) {
      stop(paste0("No mutations before ", date, "; first mutation on ", min(dates), " and last mutation on ", max(dates)))
    }
    x <- x_new
  }
  if (lang == "nl") {
    cols <- c("Categorie", "Nummer", "Grootboek", "Saldo")
    accounts <- attr(x, "Grootboeken")
    tot <- "Totaal"
    subtot <- "Subtotaal"
    lookup <- c(
      "Vaste activa en passiva", "Vlottende activa en passiva", "Tussenrekeningen",
      "Voorraadrekeningen", "Kostenrekeningen", "Overig", "Overig", "Kostpijs rekeningen",
      "Omzet rekeningen", "Financiele baten en lasten"
    )
    if (type == "balance_sheet") {
      searchterm <- "Balans"
    } else {
      searchterm <- "Winst & Verlies"
    }
    statement <- aggregate(x[x$Soort == searchterm, ]$Saldo, by = list(b = x[x$Soort == searchterm, ]$Grootboek), FUN = sum, na.rm = TRUE)
  } else {
    cols <- c("Category", "Number", "Account", "Amount")
    accounts <- attr(x, "Accounts")
    tot <- "Total"
    subtot <- "Subtotal"
    lookup <- c(
      "Fixed assets and liabilities", "Current assest and liabilities", "Suspense accounts",
      "Inventory accounts", "Expense accounts", "Other", "Other", "Cost accounts",
      "Revenue accounts", "Financial income and expenses"
    )
    if (type == "balance_sheet") {
      searchterm <- "Balance sheet"
    } else {
      searchterm <- "Profit & Loss"
    }
    statement <- aggregate(x[x$Type == searchterm, ]$Amount, by = list(b = x[x$Soort == searchterm, ]$Account), FUN = sum, na.rm = TRUE)
  }
  if (type == "income_statement") {
    statement$x <- -statement$x
  }
  matched_accounts <- accounts[match(statement$b, accounts$accDesc), ]
  rek <- ifelse(!is.na(matched_accounts$accID), lookup[as.integer(substr(matched_accounts$accID, 1, 1)) + 1], NA)
  statement <- cbind(rek = rek, id = matched_accounts$accID, statement)
  statement <- statement[order(statement$id), ]
  colnames(statement) <- cols
  rownames(statement) <- seq_len(nrow(statement))
  statement[, 1] <- ifelse(duplicated(statement[, 1]), "", statement[, 1])
  result <- data.frame(character(), character(), character(), numeric())
  colnames(result) <- colnames(statement)
  for (i in seq_len(nrow(statement))) {
    if (i == 1) {
      index <- 1
      result <- rbind(result, statement[1, ])
    } else {
      if (i == nrow(statement)) {
        if (!is.na(statement[i, 1]) && statement[i, 1] == "") {
          result <- rbind(result, statement[i, ])
          df <- data.frame(subtot, "", "", sum(statement[index, 4]))
          names(df) <- names(statement)
          result <- rbind(result, df)
        } else {
          df <- data.frame(subtot, "", "", sum(statement[index, 4]))
          names(df) <- names(statement)
          result <- rbind(result, df)
          result <- rbind(result, statement[i, ])
          df <- data.frame(subtot, "", "", sum(statement[nrow(statement), 4]))
          names(df) <- names(statement)
          result <- rbind(result, df)
        }
      } else {
        if (!is.na(statement[i, 1]) && statement[i, 1] != "") {
          df <- data.frame(subtot, "", "", sum(statement[index, 4]))
          names(df) <- names(statement)
          result <- rbind(result, df)
          index <- i
          result <- rbind(result, statement[i, ])
        } else {
          if (is.na(statement[i, 1])) {
            df <- data.frame(subtot, "", "", sum(statement[index, 4]))
            names(df) <- names(statement)
            result <- rbind(result, df)
            result <- rbind(result, statement[i, ])
            index <- i
          } else {
            index <- c(index, i)
            result <- rbind(result, statement[i, ])
          }
        }
      }
    }
  }
  row <- data.frame(tot, "", "", sum(statement[, 4]))
  names(row) <- names(statement)
  result <- rbind(result, row)
  rownames(result) <- seq_len(nrow(result))
  return(result)
}
