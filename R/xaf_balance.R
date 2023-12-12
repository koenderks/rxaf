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
#' @usage xaf_balance(x, date = NULL)
#'
#' @param x     a data frame resulting from a call to \code{read_xaf()}.
#' @param date  a character specifying the date up until the balance should be
#'   made up. The required format is day-month-year (i.e., 02-01-2016 is january
#'   second, 2016).
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
#' xaf_balance(df, date = "31-12-2022")
#' }
#' @export

xaf_balance <- function(x, date = NULL) {
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
    cols <- c("Categorie", "Grootboek", "Saldo")
    accounts <- attr(x, "Grootboeken")
    tot <- "Totaal"
    subtot <- "Subtotaal"
    lookup <- c(
      "Vaste activa en passiva", "Vlottende activa en passiva", "Tussenrekeningen",
      "Voorraadrekeningen", "Kostenrekeningen", NA, NA, "Kostpijs rekeningen",
      "Omzet rekeningen", "Financiele baten en lasten"
    )
    balance <- aggregate(x[x$Soort == "Balans", ]$Saldo, by = list(b = x[x$Soort == "Balans", ]$Grootboek), FUN = sum, na.rm = TRUE)
  } else {
    cols <- c("Category", "Account", "Amount")
    accounts <- attr(x, "Accounts")
    tot <- "Total"
    subtot <- "Subtotal"
    lookup <- c(
      "Fixed assets and liabilities", "Current assest and liabilities", "Suspense accounts",
      "Inventory accounts", "Expense accounts", NA, NA, "Cost accounts",
      "Revenue accounts", "Financial income and expenses"
    )
    balance <- aggregate(x[x$Type == "Balance sheet", ]$Amount, by = list(b = x[x$Soort == "Balance sheet", ]$Account), FUN = sum, na.rm = TRUE)
  }
  balance <- balance[order(balance$b), ]
  matched_accounts <- accounts[match(balance$b, accounts$accDesc), ]
  balance <- cbind(rek = ifelse(!is.na(matched_accounts$accID), lookup[as.integer(substr(matched_accounts$accID, 1, 1)) + 1], NA), balance)
  colnames(balance) <- cols
  rownames(balance) <- seq_len(nrow(balance))
  balance <- balance[order(balance[, 1], -(balance[, 3] > 0), balance[, 2]), ]
  balance[, 1] <- ifelse(duplicated(balance[, 1]), "", balance[, 1])
  new <- data.frame(numeric(), numeric(), numeric())
  colnames(new) <- colnames(balance)
  for (i in seq_len(nrow(balance))) {
    if (i == 1) {
      index <- 1
      new <- rbind(new, balance[1, ])
    } else {
      if (i == nrow(balance)) {
        if (balance[i, 1] == "") {
          new <- rbind(new, balance[i, ])
          df <- data.frame(subtot, "", sum(balance[index, 3]))
          names(df) <- names(balance)
          new <- rbind(new, df)
        } else {
          df <- data.frame(subtot, "", sum(balance[index, 3]))
          names(df) <- names(balance)
          new <- rbind(new, df)
          new <- rbind(new, balance[i, ])
          df <- data.frame(subtot, "", sum(balance[nrow(balance), 3]))
          names(df) <- names(balance)
          new <- rbind(new, df)
        }
      } else {
        if (balance[i, 1] != "") {
          df <- data.frame(subtot, "", sum(balance[index, 3]))
          names(df) <- names(balance)
          new <- rbind(new, df)
          index <- i
          new <- rbind(new, balance[i, ])
        } else {
          index <- c(index, i)
          new <- rbind(new, balance[i, ])
        }
      }
    }
  }
  newrow <- data.frame(tot, "", sum(balance[, 3]))
  names(newrow) <- names(balance)
  new <- rbind(new, newrow)
  rownames(new) <- seq_len(nrow(new))
  return(new)
}
