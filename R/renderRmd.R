require(rmarkdown)
require(dplyr)
require(YHFinR)


dateout <- gsub("-", "", Sys.Date())
rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', dateout, '.html'),
                  params = list(dump_file = paste0('PortfolioReport/PortfolioReport_', dateout, '.csv')))


# DJ ----
SearchName <- "DJ"
Index <- "DJ"

Tickers<- YHFinR::getYFIndexComp("^DJI")$Symbol

rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^DJI"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# Europe ----
Tickers <- read.csv("CompaniesEUR.20210103.csv", sep = ";") %>% 
  subset(country != "  ", select = "tickers")

SearchName <- "AllEUR"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$tickers),
                                Index = "^GSPC",
                                dump_file = paste0('StockResearch/StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))


# SP500 ----
Tickers<- YHFinR::getYFIndexComp("SP500")

SearchName <- "SP500"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^GSPC",
                                dump_file = paste0('StockResearch/StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# TSX ----
Tickers <- read.csv("MD/TSX.20201013.csv", sep = ",") 
nrow(Tickers)

SearchName <- "TSX"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$Symbol),
                                Index = "^GSPC"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# NYSE ----
Tickers <- read.csv("MD/NYSE.20201010.csv", sep = ",") 
nrow(Tickers)

SearchName <- "NYSE"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$Symbol),
                                Index = "^GSPC",
                                dump_file = paste0('StockResearch/StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# NYSE + NASDAQ + AMEX ----
Tickers <- rbind( read.csv("MD/NYSE.20201010.csv", sep = ","),
                  read.csv("MD/NASDAQ.20201010.csv", sep = ","), 
                  read.delim("MD/AMEX.txt", header = TRUE, sep = "\t"))

SearchName <- "All_US"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$Symbol),
                                Index = "^GSPC",
                                dump_file = paste0('StockResearch/StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))


