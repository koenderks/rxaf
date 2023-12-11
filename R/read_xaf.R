# Copyright (C) 2023-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Read Dutch XML Audit Files (XAF)
#'
#' @description This function reads and parses Dutch XML Audit Files (XAF),
#'   which are commonly used in the Netherlands for financial and auditing
#'   purposes.
#'
#' @usage read_xaf(
#'   file,
#'   progress = interactive(),
#'   clean = TRUE,
#'   lang = c("nl", "en")
#' )
#'
#' @param file      a string specifying the path to the XAF file to be read.
#' @param progress  logical. Whether to show a progress bar.
#' @param clean     logical. Whether to clean data before return.
#' @param lang      character. Language of the added text and column names.
#'
#' @return A data frame containing the parsed data from the XAF file.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @keywords read xaf
#'
#' @references For more information about XAF files, see
#'   \url{http://www.auditfiles.nl}.
#'
#' @examples
#' \dontrun{
#' df <- read_xaf("path/to/xaf/file.xaf")
#' head(df)
#' }
#' @export

read_xaf <- function(file,
                     progress = interactive(),
                     clean = TRUE,
                     lang = c("nl", "en")) {
  lang <- match.arg(lang)
  stopifnot("'clean' must be logical (TRUE or FALSE)" = is.logical(clean))
  stopifnot("'file' does not have the .xaf extension" = endsWith(file, ".xaf"))
  flatfile <- xml2::read_xml(file)
  stopifnot("'file' is not an XML Auditfile" = tolower(xml2::xml_name(flatfile)) == "auditfile")
  namespace <- xml2::xml_ns(flatfile)
  if (length(namespace) > 0) {
    if (tolower(namespace[[1]]) == "http://www.auditfiles.nl/xaf/3.2") {
      version <- "3.2"
    } else if (tolower(namespace[[1]]) == "http://www.auditfiles.nl/xaf/3.1") {
      version <- "3.1"
    } else {
      stop(paste0("XML Auditfile version ", namespace[[1]], "is currently not supported"))
    }
  } else {
    stop("'file' does not have an identifiable namespace")
  }
  doc <- xml2::as_list(flatfile)
  header <- doc$auditfile$header
  company <- doc$auditfile$company
  transactions <- company$transactions
  size <- as.numeric(transactions$linesCount[[1]])
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = size + 1, initial = 0, width = 80, style = 3)
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
  # Amounts
  df$amount <- ifelse(df$amntTp == "D", as.numeric(df$amnt), -as.numeric(df$amnt))
  df <- df[, -which(colnames(df) %in% c("amntTp", "amnt"))]
  df$debet <- ifelse(df$amount > 0, df$amount, NA)
  df$credit <- ifelse(df$amount < 0, abs(df$amount), NA)
  # VATS
  if ("vatID" %in% colnames(df)) {
    vats <- .construct_vats_index(company)
    df$vatDesc <- vats$vatDesc[match(df$vatID, vats$vatID)]
    df$vat_amount <- ifelse(is.na(df$vatAmntTp), NA, ifelse(df$vatAmntTp == "D", as.numeric(df$vatAmnt), -as.numeric(df$vatAmnt)))
    df$vatToClaimAccID <- vats$vatToClaimAccID[match(df$vatID, vats$vatID)]
    df$vatToPayAccID <- vats$vatToPayAccID[match(df$vatID, vats$vatID)]
    df <- df[, -which(colnames(df) %in% c("vatAmntTp", "vatAmnt"))]
  }
  # Customer/Suppliers
  if ("custSupID" %in% colnames(df)) {
    suppliers <- .construct_custsup_index(company)
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
  # Accounts
  accounts <- .construct_account_index(company, lang)
  df$accDesc <- accounts$accDesc[match(df$accID, accounts$accID)]
  df$accTp <- accounts$accTp[match(df$accID, accounts$accID)]
  df$leadCode <- accounts$leadCode[match(df$accID, accounts$accID)]
  df$leadDescription <- accounts$leadDescription[match(df$accID, accounts$accID)]
  df$leadReference <- accounts$leadReference[match(df$accID, accounts$accID)]
  df$accountType <- accounts$accountType[match(df$accID, accounts$accID)]
  df$accountKind <- accounts$accountKind[match(df$accID, accounts$accID)]
  # Journals
  journals <- .construct_journal_index(company, lang)
  df$jrn_jrnID <- journals$jrnID[match(df$jrnID, journals$jrnID)]
  df$jrn_desc <- journals$jrnDesc[match(df$jrnID, journals$jrnID)]
  df$jrn_offsetAccID <- journals$offsetAccID[match(df$jrnID, journals$jrnID)]
  df$jrn_bankAccNr <- journals$bankAccNr[match(df$jrnID, journals$jrnID)]
  df$jrn_journaltype <- journals$journalType[match(df$jrnID, journals$jrnID)]
  # Additional information
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
  # Formatting
  df$trDt <- as.Date(df$trDt)
  df$periodNumber <- as.numeric(df$periodNumber)
  df$effDate <- as.Date(df$effDate)
  # Cleaning
  result <- .clean_xaf(df, clean, lang)
  if (progress) {
    utils::setTxtProgressBar(pb, index)
  }
  attrNames <- switch(lang,
    "nl" = c("Dagboeken", "Grootboeken", "BTW.Codes", "Relaties"),
    "en" = c("Journals", "Accounts", "VAT.Codes", "Relations")
  )
  attr(result, "lang") <- lang
  attr(result, "clean") <- clean
  attr(result, attrNames[1]) <- journals
  attr(result, attrNames[2]) <- accounts
  if ("vatID" %in% colnames(df)) {
    attr(result, attrNames[3]) <- vats
  }
  if ("custSupID" %in% colnames(df)) {
    attr(result, attrNames[4]) <- suppliers
  }
  class(result) <- c(class(result), "xaf")
  return(result)
}
