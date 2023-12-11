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

#' Extract Subtables from a XAF file
#'
#' @description This function extracts the journals and accounts subtables from
#'   a XAF file. If the XAF file contains additional tables such as the VAT
#'   Codes and Customer / Supplier data, these are extracted as well.
#'
#' @usage xaf_subtables(x)
#'
#' @param x  a data frame resulting from a call to \code{read_xaf()}.
#'
#' @return A list containing the journals, accounts, vat codes and relations in
#'   the XAF file.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @keywords subtables xaf
#'
#' @examples
#' \dontrun{
#' df <- read_xaf("path/to/xaf/file.xaf")
#' xaf_subtables(df)
#' }
#' @export

xaf_subtables <- function(x) {
  stopifnot("'x' is not output from 'read_xaf()'" = inherits(x, "xaf"))
  attrNames <- switch(attr(x, "lang"),
    "nl" = c("Dagboeken", "Grootboeken", "BTW.Codes", "Relaties"),
    "en" = c("Journals", "Accounts", "VAT.Codes", "Relations")
  )
  tb <- list()
  for (i in seq_len(4)) {
    table <- attr(x, attrNames[i])
    if (!is.null(table)) {
      tb[[attrNames[i]]] <- table
    }
  }
  return(tb)
}
