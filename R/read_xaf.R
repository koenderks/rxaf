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
  journals <- .construct_journal_index(company, lang)
  accounts <- .construct_account_index(company, lang)
  vats <- .construct_vats_index(company)
  relations <- .construct_custsup_index(company)
  transactions <- company$transactions
  df <- .construct_mutations(transactions, progress)
  df <- .add_raw_amounts(df)
  df <- .add_raw_vats(df, vats)
  df <- .add_raw_relations(df, relations)
  df <- .add_raw_accounts(df, accounts)
  df <- .add_raw_journals(df, journals)
  df <- .add_raw_info(df, file, header, company, transactions)
  result <- .clean_xaf(df, clean, lang)
  attr(result, "lang") <- lang
  attr(result, "clean") <- clean
  attr(result, switch(lang,
    "nl" = "Dagboeken",
    "en" = "Journals"
  )) <- journals
  attr(result, switch(lang,
    "nl" = "Grootboeken",
    "en" = "Accounts"
  )) <- accounts
  attr(result, switch(lang,
    "nl" = "BTW.Codes",
    "en" = "VAT.Codes"
  )) <- vats
  attr(result, switch(lang,
    "nl" = "Relaties",
    "en" = "Relations"
  )) <- relations
  class(result) <- c(class(result), "xaf")
  return(result)
}
