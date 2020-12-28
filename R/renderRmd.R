require(rmarkdown)
require(dplyr)
require(YHFinR)


dateout <- gsub("-", "", Sys.Date())
rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', dateout, '.html'),
                  params = list(dump_file = paste0('PortfolioReport_', dateout, '.csv')))


# DJ ----
SearchName <- "DJ"
Index <- "DJ"

Tickers<- YHFinR::getYFIndexComp("^DJI")$Symbol

rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^DJI"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# Europe with industry informed ----
Tickers <- read.csv("OldFiles/CompanyList.20200703.csv", sep = ";") %>% 
  subset(!is.na(industry) & country == "ES", select = "tickers")

SearchName <- "BE"
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
                  params = list(Ticker = unique(Tickers)[1:40],
                                Index = "^GSPC",
                                dump_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')),
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
