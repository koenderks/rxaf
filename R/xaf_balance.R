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

#' Create a Balance Sheet from a cleaned XAF file
#'
#' @description This function creates a balance sheet from a cleaned XAF file
#'   at a specific point in time.
#'
#' @usage xaf_balance(x, at = NULL)
#'
#' @param x  a data frame resulting from a call to \code{read_xaf()}.
#' @param at a character specifying the date up until the balance should be made up.
#'
#' @return A data frame containing the balance sheet.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @keywords balance xaf
#'
#' @examples
#' \dontrun{
#' df <- read_xaf("path/to/xaf/file.xaf")
#' xaf_balance(df, at = "2016-03-08")
#' }
#' @export

xaf_balance <- function(x, at = NULL) {
  stopifnot("'x' is not output from 'read_xaf()'" = inherits(x, "xaf"))
  stopifnot("not supported for uncleaned files'" = attr(x, "clean"))
  lang <- attr(x, "lang")
  if (!is.null(at)) {
    at <- try(as.Date(at))
    stopifnot("stop" = !inherits(at, "try-error"))
    x_new <- switch(lang,
      "nl" = x[x$Datum <= at, ],
      "en" = x[x$Date <= at, ]
    )
    if (nrow(x_new) == 0) {
      message <- switch(lang,
        "nl" = paste0("No mutations before ", at, "; first mutation on ", min(x$Datum), " and last mutation on ", max(x$Datum)),
        "en" = paste0("No mutations before ", at, "; first mutation on ", min(x$Date), " and last mutation on ", max(x$Date))
      )
      stop(message)
    }
    x <- x_new
  }
  balance <- switch(lang,
    "nl" = aggregate(x[x$Soort == "Balans", ]$Saldo, by = list(a = x[x$Soort == "Balans", ]$Dagboek, b = x[x$Soort == "Balans", ]$Grootboek), FUN = sum, na.rm = TRUE),
    "en" = aggregate(x[x$Type == "Balance sheet", ]$Amount, by = list(a = x[x$Type == "Balance sheet", ]$Journal, b = x[x$Soort == "Balance sheet", ]$Account), FUN = sum, na.rm = TRUE)
  )
  balance <- balance[order(balance$a, balance$b), ]
  balance[duplicated(balance$a), 1] <- NA
  colnames(balance) <- switch(lang,
    "nl" = c("Dagboek", "Grootboek", "Saldo"),
    "en" = c("Journal", "Account", "Amount")
  )
  rownames(balance) <- seq_len(nrow(balance))
  return(balance)
}
