
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
scrapeYahooFinance <- function(url, time_in_seconds = 1) {
  Sys.sleep(time_in_seconds)
  webpage <- readLines(url)
  html <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- XML::getNodeSet(html, "//table")
  res <- lapply(tableNodes, function(x) readHTMLTable(x)) %>% 
    dplyr::bind_rows() 
  Sys.sleep(time_in_seconds)
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
    saveRDS(FinRat, "FinRat.tmp.rds")
  }
  FinRat
}

# FinRatios.df <- readRDS("FinRatiosYahoo/FinRatiosYahoo20200627.rds")
# renameFinRatios(FinRatios.df, "Enterprise Value 3", "EV")
renameFinRatios <- function(Object, fromYahooName, toFinalName, VariableName = "Variable") {
  stopifnot(length(fromYahooName) == length(toFinalName))
  for(i in 1:length(fromYahooName)) {
    idx <- which(Object[,VariableName] %in% fromYahooName[i])
    Object[idx, VariableName] <- toFinalName[i]
    if (length(idx) == 0) {
      Object[grep(fromYahooName[i], Object[,VariableName]), VariableName] <- toFinalName[i]
    }
  }
  Object
}

# getHistPriceYF(c("MC.PA", "AI.PA", "CS.PA"), "w", "2017-07-02")
getHistPriceYF <- function(tickers, periodicity, from, to = Sys.Date()) {
  HistPrices.yahoo.list <- lapply(tickers, 
                                  function(ticker) {
                                    quantmod::getSymbols(ticker,
                                                         from = from,
                                                         to = to,
                                                         periodicity = periodicity,
                                                         auto.assign = FALSE,
                                                         warnings = FALSE) %>%
                                      .[, grep("Adjusted", colnames(.))]
                                  }) %>%
    setNames(tickers)
  HistPrices.yahoo.list
}


convertPer2num <- function(Val) {
  as.numeric(gsub("%", "", Val))/100
}


# val = c("100B", "87000", "300.5M" , "5.25M")
# convertLetterAmount2num(val)
convertLetterAmount2num <- function(Val) {
  res <- sapply(Val, function(x) { # x = val[1]
    ifelse(grepl("B$", x), as.numeric(gsub("B", "", x)) * 1000,
           ifelse(grepl("M$", x), as.numeric(gsub("M", "", x)) * 1, 
                  as.numeric(x)))
  })
  res
}

