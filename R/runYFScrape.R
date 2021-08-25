

# US ----
Tickers <- read.csv("MD/CompaniesUS.20210319.csv", sep = ";") %>% 
  subset(Sector != "NAYF" & Sector != "" ) %>% 
  .[, c("Ticker", "CompanyName", "Country", "Sector", "Industry")] %>% 
  unique()
AppendName <- "US"
scrapeYF(Tickers, AppendName)

# EUR ----
Tickers <- read.csv("MD/CompaniesEUR.20210429.csv", sep = ";", stringsAsFactors = FALSE) %>% 
  subset(Sector != "NAYF" & Sector != "" ) %>% 
  .[, c("Ticker", "CompanyName", "Country", "Sector", "Industry")] %>% 
  unique()


AppendName <- "EUR"

scrapeYF <- function(Tickers, AppendName, Hist = TRUE) {
  
  KeyStats <- YHFinR::getYFKeyStatistics(Tickers$Ticker, block = 100, slp = 10)
  output_file = paste0('RawData/KeyStats.list_n', length(KeyStats), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
  saveRDS(KeyStats, output_file)

  FinBasic <- YHFinR::getYFFinancialsBasics(Tickers$Ticker, block = 100, slp = 10)
  output_file = paste0('RawData/FinBasic.list_n', length(KeyStats), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
  saveRDS(FinBasic, output_file)

  BS <- getYH_qBS(Tickers$Ticker, block = 100, slp = 10)
  output_file = paste0('RawData/BS.list_n', length(BS), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
  saveRDS(BS, output_file)

  IS <- getYH_qIS(Tickers$Ticker, block = 100, slp = 10)
  output_file = paste0('RawData/IS.list_n', length(IS), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
  saveRDS(IS, output_file)

  CF <- getYH_qCF(Tickers$Ticker, block = 100, slp = 10)
  output_file = paste0('RawData/CF.list_n', length(CF), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
  saveRDS(CF, output_file)
  
  if (Hist) {
    HistPrices <- YHFinR::getYFHistPrices(Tickers$Ticker, "2y", "1d", block = 100, slp = 10)
    output_file = paste0('RawData/HistPr.list_n', length(HistPrices), "_",AppendName, "_", gsub("-", "", Sys.Date()), '.rds')
    saveRDS(HistPrices, output_file)
  }
}




