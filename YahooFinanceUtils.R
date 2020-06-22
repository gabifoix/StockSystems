
#' Scrape Financial Ratios from Yahoo Finance
#'
#' @param ticker `character`
#' @return `data.frame` with 2 columns, Variable & Value
#' @importFrom XML htmlTreeParse getNodeSet
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%` set_colnames
#'
#' @examples scrapeFinRatiosYahoo("MC.PA")
#' ticker = "MC.PP"
scrapeFinRatiosYahoo <- function(ticker) {
  url <- buildYahooFinanceURL(ticker, "key-statistics")
  res <- scrapeYahooFinance(url)
  res
}


# scrapeIndexComp("%5EMDAXI")
scrapeIndexComp <- function(ticker) {
  url <- buildYahooFinanceURL(ticker, "components")
  res <- scrapeYahooFinance(url)
  res
}

# ticker = "MC.PA"
# scrapeProfile(ticker)
scrapeProfile <- function(ticker){
  url <- sprintf("https://finance.yahoo.com/lookup?s=%s", ticker)
  res <- scrapeYahooFinance(url) %>% 
    subset(Symbol == ticker, select = c("Symbol", "Industry / Category")) %>% 
    dplyr::rename(ticker = Symbol, industry = "Industry / Category") 
  res
  
}


# buildYahooFinanceURL("MC.PA", "key-statistics")
buildYahooFinanceURL <- function(ticker, urlsection) {
  generic <- paste0("https://finance.yahoo.com/quote/%s/", urlsection,"?p=%s")
  url <- sprintf(generic, ticker, ticker)
  url
}



# scrapeYahooFinance(buildYahooFinanceURL("MC.PA", "key-statistics"))
scrapeYahooFinance <- function(url) {
  webpage <- readLines(url)
  html <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- XML::getNodeSet(html, "//table")
  res <- lapply(tableNodes, function(x) readHTMLTable(x)) %>% 
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
#' @examples res <- getFinRatios(c("MC.PA", "MC.PP", "VOW.DE", "VIV.PA", "wrong.ticker", "SAN.MC"))
getFinRatios <- function(tickers) {
  message(paste("Getting financial ratios from Yahoo Finance for", length(tickers), "tickers", sep = " "))
  # Define default df
  default.df <- data.frame(Variable = as.character(),
                           Value = as.character(),
                           Ticker = as.character())
  FinRat <- default.df
  for (one.ticker in tickers) {
    one.ticker <- trimws(one.ticker)
    dfextract <- scrapeFinRatiosYahoo(one.ticker)
    if (nrow(dfextract) == 0 || ncol(dfextract) > 2) {
      message(paste0("Wrong ticker: ", one.ticker))
      df <- default.df
    } else {
      df <- dfextract %>% 
        magrittr::set_colnames(c("Variable", "Value")) %>% 
        dplyr::mutate(Ticker = one.ticker)
    }
    FinRat <- rbind(FinRat, df)
  }
  FinRat
}

# renameFinRatio(FinRat.df, "200-Day Moving Average", "MA200")
renameFinRatios <- function(Object, fromYahooName, toFinalName, VariableName = "Variable") {
  stopifnot(length(fromYahooName) == length(toFinalName))
  for(i in 1:length(fromYahooName)) {
    Object[grep(fromYahooName[i], Object[,VariableName]), VariableName] <- toFinalName[i]
  }
  Object
}



.cleanNAs  <- function(x, replace = 0) { 
  x[is.na(x)] <- replace
  x
} 

convertPer2num <- function(Val) {
  as.numeric(gsub("%", "", Val))/100
}

