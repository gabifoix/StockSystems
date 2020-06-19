
#' Scrape Financial Ratios from Yahoo Finance
#'
#' @param ticker `character`
#' @return `data.frame` with 2 columns, Variable & Value
#' @importFrom XML htmlTreeParse getNodeSet
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%` set_colnames
#'
#' @examples scrapeFinRatiosYahoo("MC.PA")
scrapeFinRatiosYahoo <- function(ticker) {
  url <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s", ticker, ticker)
  webpage <- readLines(url)
  html <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- XML::getNodeSet(html, "//table")
  
  res <- lapply(tableNodes, function(x) {readHTMLTable(x) %>% 
      magrittr::set_colnames(c("Variable", "Value"))}) %>% 
    dplyr::bind_rows() 
  res
}


#' get Financial ratios for multiple Tickers
#' Wrapper aroundscrape FinRatiosYahoo
#'
#' @param tickers 
#'
#' @return `data.frame` with 3 columns, Variable, Value, Ticker
#' @export
#'
#' @examples getFinRatios(c("MA.PA", "VOW.DE"))
getFinRatios <- function(tickers) {

  lapply(tickers, function(one.ticker) {
    FinRat <- scrapeFinRatiosYahoo(one.ticker) %>% 
      dplyr::mutate(Ticker = one.ticker)
    FinRat
  }) %>% 
    dplyr::bind_rows()
}

.cleanNAs  <- function(x, replace = 0) { 
  x[is.na(x)] <- replace
  x
} 
