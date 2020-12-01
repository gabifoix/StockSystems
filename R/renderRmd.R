require(rmarkdown)
require(dplyr)
require(YHFinR)


rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', gsub("-", "", Sys.Date()), '.html'))


# DJ ----
SearchName <- "DJ"
Index <- "DJ"

Tickers<- YHFinR::getYFIndexComp(Index)$Symbol

rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^DJI"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# Europe with industry informed ----
Tickers <- read.csv("CompanyList.20200703.csv", sep = ";") %>% 
  subset(!is.na(industry), select = "tickers")

SearchName <- "AllEUR"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$tickers),
                                Index = "^GSPC"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))


# SP500 ----
Tickers<- YHFinR::getYFIndexComp("SP500")

SearchName <- "SP500"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^GSPC"),
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
