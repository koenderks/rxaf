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

#' Create an Income Statement from a Cleaned XAF File
#'
#' @description This function creates an income statement at a specific point in
#'   time from a cleaned XAF file.
#'
#' @usage xaf_income_statement(x, date = NULL)
#'
#' @param x     a data frame resulting from a call to \code{read_xaf()}.
#' @param date  a character specifying the date up until the income statement
#'   should be made up. The required format is day-month-year (i.e., 02-01-2016
#'   is january second, 2016).
#'
#' @return A data frame containing the income statement.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @keywords income statement xaf
#'
#' @examples
#' \dontrun{
#' df <- read_xaf("path/to/xaf/file.xaf")
#' xaf_income_statement(df, date = "31-12-2022")
#' }
#' @export

xaf_income_statement <- function(x, date = NULL) {
  return(.xaf_statement(x, date, "income_statement"))
}
