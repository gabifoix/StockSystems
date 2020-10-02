require(rmarkdown)
require(dplyr)
require(YHFinR)


rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', gsub("-", "", Sys.Date()), '.html'))




Tickers <- read.csv("CompanyList.20200713.csv", sep = ";") %>% 
  subset(country == "HO", select = "tickers")

# DJ ----
SearchName <- "DJ"
Index <- "^DJI"

Tickers<- YHFinR::getYFIndexComp(Index)$Symbol

rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers),
                                Index = "^DJI"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))

# Europe with industry informed ----
Tickers <- read.csv("CompanyList.20200713.csv", sep = ";") %>% 
  subset(!is.na(industry), select = "tickers")

SearchName <- "AllEUR"
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = unique(Tickers$tickers),
                                Index = "^DJI"),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))


# SP500 ----
Tickers<- YHFinR::getYFIndexComp("SP500")$Symbol

